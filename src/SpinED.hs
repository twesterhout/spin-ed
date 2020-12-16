-- |
-- Copyright: (c) 2020 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
--
-- User-friendly exact diagonalization for spin systems
module SpinED
  ( -- * For the app
    UserConfig (..),
    Environment (..),
    EnvT (..),
    readConfig,
    toConfig,
    runEnvT,
    withDatatype,
    version,
    buildRepresentatives,
    diagonalize,
    computeExpectations,

    -- * For testing
    SpinEDException (..),
    LatticeSymmetriesException (..),
    SymmetrySpec (..),
    BasisSpec (..),
    InteractionSpec (..),
    OperatorSpec (..),
    ConfigSpec (..),
    Symmetry,
    SymmetryGroup,
    SpinBasis,
    Interaction,
    Operator,
    Operator',
    Datatype (..),
    BlasDatatype (..),
    toSymmetry,
    toBasis,
    toInteraction,
    isRealInteraction,
    isOperatorReal,
    getSector,
    getPeriodicity,
    getPhase,
    mkGroup,
    getGroupSize,
  )
where

import Colog
  ( HasLog (..),
    LogAction,
    Message,
    WithLog,
    logInfo,
    logWarning,
  )
import Colog.Monad (liftLogAction)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, throw)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Complex
import qualified Data.HDF5 as H5
import Data.Scientific (toRealFloat)
import Data.Text (toLower)
import Data.Vector.Storable (Vector)
import Data.Yaml (decodeFileWithWarnings)
import Foreign.Storable (Storable)
import Numeric.PRIMME
import Paths_spin_ed (version)
import SpinED.Internal

data SymmetrySpec = SymmetrySpec ![Int] !Int
  deriving stock (Read, Show, Eq)

instance FromJSON SymmetrySpec where
  parseJSON = withObject "symmetry" $ \v ->
    SymmetrySpec
      <$> v .: "permutation"
      <*> v .: "sector"

data BasisSpec = BasisSpec !Int !(Maybe Int) !(Maybe Int) ![SymmetrySpec]
  deriving stock (Read, Show, Eq)

instance FromJSON BasisSpec where
  parseJSON = withObject "basis" $ \v ->
    BasisSpec
      <$> v .: "number_spins"
      <*> v .:? "hamming_weight"
      <*> v .:? "spin_inversion"
      <*> v .: "symmetries"

instance FromJSON (Complex Double) where
  parseJSON (Number x) = pure . fromReal . toRealFloat $ x
    where
      fromReal :: Num a => a -> Complex a
      fromReal x' = x' :+ 0
  parseJSON v@(Array xs) = case (toList xs) of
    [re, im] -> (:+) <$> parseJSON re <*> parseJSON im
    _ -> typeMismatch "Complex" v
  parseJSON v = typeMismatch "Complex" v

data InteractionSpec = InteractionSpec ![[Complex Double]] ![[Int]]
  deriving stock (Read, Show, Eq)

instance FromJSON InteractionSpec where
  parseJSON = withObject "interaction" $ \v ->
    InteractionSpec
      <$> v .: "matrix"
      <*> v .: "sites"

data OperatorSpec = OperatorSpec !Text ![InteractionSpec]
  deriving stock (Read, Show)

instance FromJSON OperatorSpec where
  parseJSON = withObject "operator" $ \v ->
    OperatorSpec
      <$> v .: "name"
      <*> v .: "terms"

data Datatype
  = DatatypeFloat32
  | DatatypeFloat64
  deriving stock (Read, Show, Eq)

instance FromJSON Datatype where
  parseJSON = withText "Datatype" $ \v ->
    case (toLower v) of
      "float32" -> return DatatypeFloat32
      "float64" -> return DatatypeFloat64
      _ ->
        fail . toString $
          "parsing Datatype failed, expected either 'float32' or 'float64', but got '"
            <> v
            <> "'"

data ConfigSpec = ConfigSpec !BasisSpec !OperatorSpec ![OperatorSpec] !Text !Int !Double !Datatype !(Maybe Int) !(Maybe Int)
  deriving stock (Read, Show)

instance FromJSON ConfigSpec where
  parseJSON = withObject "config" $ \v ->
    ConfigSpec
      <$> v .: "basis"
      <*> v .: "hamiltonian"
      <*> v .: "observables"
      <*> v .:! "output" .!= "exact_diagonalization_result.h5"
      <*> v .:! "number_vectors" .!= 1
      <*> v .:! "precision" .!= 0.0
      <*> v .:! "datatype" .!= DatatypeFloat64
      <*> v .:! "max_primme_basis_size"
      <*> v .:! "max_primme_block_size"

toSymmetry :: (MonadIO m, MonadThrow m) => SymmetrySpec -> m Symmetry
toSymmetry (SymmetrySpec p s) = mkSymmetry p s

toBasis :: (WithLog env Message m, MonadIO m, MonadThrow m) => BasisSpec -> m SpinBasis
toBasis (BasisSpec numberSpins hammingWeight spinInversion symmetries) = do
  -- We need to make sure we use "small" basis, otherwise we won't be able to
  -- build a list of representatives later
  when (numberSpins > 64) . throw . SpinEDException $
    "invalid number_spins: " <> show numberSpins <> "; exact diagonalization is not feasible "
      <> "for systems larger than 64 spins"
  logInfo "Building symmetry group..."
  symmetryGroup <- mkGroup =<< mapM toSymmetry symmetries
  logInfo $ "Symmetry group contains " <> show (getGroupSize symmetryGroup) <> " elements"
  mkBasis symmetryGroup numberSpins hammingWeight spinInversion

toInteraction :: (MonadIO m, MonadThrow m) => InteractionSpec -> m Interaction
toInteraction (InteractionSpec matrix sites) = mkInteraction' matrix sites

data Operator = Operator
  { operatorName :: !Text,
    operatorObject :: !Operator'
  }

toOperator :: (MonadIO m, MonadThrow m) => SpinBasis -> OperatorSpec -> m Operator
toOperator basis (OperatorSpec name terms) =
  Operator <$> pure name <*> (mkOperator basis =<< mapM toInteraction terms)

data UserConfig = UserConfig
  { cBasis :: !SpinBasis,
    cHamiltonian :: !Operator,
    cObservables :: ![Operator],
    cOutput :: !Text,
    cNumEvals :: !Int,
    cEps :: !Double,
    cDatatype :: !Datatype,
    cMaxBasisSize :: !(Maybe Int),
    cMaxBlockSize :: !(Maybe Int)
  }

data Environment m = Environment
  { eConfig :: !UserConfig,
    eLog :: LogAction m Message,
    eData :: H5.File
  }

newtype EnvT m a = EnvT {unEnvT :: ReaderT (Environment (EnvT m)) m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

deriving newtype instance Monad m => MonadReader (Environment (EnvT m)) (EnvT m)

runEnvT :: Monad m => EnvT m a -> Environment m -> m a
runEnvT action env = runReaderT (unEnvT action) (liftEnv env)
  where
    liftEnv x = x {eLog = liftLogAction (eLog x)}

instance MonadTrans EnvT where
  lift = EnvT . lift

instance HasLog (Environment m) Message m where
  getLogAction = eLog
  setLogAction action env = env {eLog = action}

toConfig :: (WithLog env Message m, MonadIO m, MonadThrow m) => ConfigSpec -> m UserConfig
toConfig (ConfigSpec basisSpec hamiltonianSpec observablesSpecs output numEvals eps dtype maxBasisSize maxBlocksize) = do
  basis <- toBasis basisSpec
  hamiltonian <- toOperator basis hamiltonianSpec
  observables <- mapM (toOperator basis) observablesSpecs
  return $ UserConfig basis hamiltonian observables output numEvals eps dtype maxBasisSize maxBlocksize

readConfig :: (WithLog env Message m, MonadIO m) => Text -> m ConfigSpec
readConfig path = do
  logInfo "Parsing config file..."
  r <- liftIO $ decodeFileWithWarnings (toString path)
  case r of
    Left e -> liftIO $ throw e
    Right (warnings, config) -> do
      mapM_ (logWarning . show) warnings
      return config

withGroup' :: (MonadIO m, MonadMask m) => H5.File -> Text -> (H5.Group -> m a) -> m a
withGroup' file name func = do
  H5.exists file name >>= \exists ->
    unless exists $
      H5.makeGroup file name
  H5.byName file name $ H5.matchM @H5.Group $ func

observablesPath :: Text
observablesPath = "/observables"

basisPath :: Text
basisPath = "/basis"

hamiltonianPath :: Text
hamiltonianPath = "/hamiltonian"

computeExpectation ::
  (MonadIO m, MonadMask m, BlasDatatype a) =>
  Operator ->
  Block a ->
  EnvT m ()
computeExpectation (Operator name operator) x = do
  logInfo $ "Computing expectation values of " <> name <> "..."
  measurements <- liftIO $ expectation operator x
  asks eData >>= \file ->
    withGroup' file observablesPath $ \g -> writeDataset' g name measurements

computeExpectations ::
  (MonadIO m, MonadMask m, BlasDatatype a) =>
  Block a ->
  EnvT m ()
computeExpectations x = asks (cObservables . eConfig) >>= mapM_ (flip computeExpectation x)

buildRepresentatives :: (HasCallStack, MonadIO m, MonadMask m) => EnvT m ()
buildRepresentatives = do
  logInfo "Building a list of representatives..."
  basis <- asks (cBasis . eConfig)
  buildBasis basis
  dim <- getNumberStates basis
  asks eData >>= \file ->
    withGroup' file basisPath $ \g ->
      writeDataset' g "representatives" =<< basisGetStates basis
  logInfo $ "Hilbert space dimension is " <> show dim

isOperatorReal :: MonadIO m => Operator -> m Bool
isOperatorReal (Operator _ op) = liftIO $ isOperatorReal' op

withDatatype ::
  Bool ->
  Datatype ->
  (forall a. (H5.KnownDatatype' a, H5.KnownDatatype' (BlasRealPart a), BlasDatatype a) => Proxy a -> b) ->
  b
withDatatype True DatatypeFloat32 f = f (Proxy @Float)
withDatatype True DatatypeFloat64 f = f (Proxy @Double)
withDatatype False DatatypeFloat32 f = f (Proxy @(Complex Float))
withDatatype False DatatypeFloat64 f = f (Proxy @(Complex Double))

writeDataset' :: (HasCallStack, MonadIO m, MonadMask m, H5.ToBlob a b) => H5.Group -> Text -> a -> EnvT m ()
writeDataset' g name x = do
  H5.exists g name >>= \alreadyExists -> when alreadyExists $ do
    prefix <- H5.getName g
    logWarning $ "Overwriting " <> prefix <> "/" <> name <> "..."
    H5.delete g name
  H5.writeDataset g name x

instance (Storable a, H5.KnownDatatype' a) => H5.ToBlob (Block a) a where
  toBlob (Block (d₁, d₂) stride v)
    | d₁ == stride = H5.Blob [d₁, d₂] v
    | otherwise = error "non-contiguous Blocks are not yet supported"

diagonalize ::
  forall a m proxy.
  ( HasCallStack,
    MonadIO m,
    MonadMask m,
    BlasDatatype a,
    H5.KnownDatatype' a,
    H5.KnownDatatype' (BlasRealPart a)
  ) =>
  proxy a ->
  EnvT m (Vector (BlasRealPart a), Block a, Vector (BlasRealPart a))
diagonalize _ = do
  config <- asks eConfig
  let hamiltonian = cHamiltonian config
      numEvals = cNumEvals config
      eps = cEps config
  dim <- getNumberStates (cBasis config)
  logInfo $ "Diagonalizing " <> operatorName hamiltonian <> "..."
  let o₁ = Numeric.PRIMME.defaultOptions {pDim = dim, pNumEvals = numEvals, pTarget = PrimmeSmallest, pEps = eps}
      o₂ = case cMaxBasisSize config of
        Just x -> o₁ {pMaxBasisSize = x}
        Nothing -> o₁
      o₃ = case cMaxBlockSize config of
        Just x -> o₂ {pMaxBlockSize = x}
        Nothing -> o₂
      primmeOptions = o₃
      primmeOperator = inplaceApply . operatorObject $ hamiltonian
  result@(evals, evecs, rnorms) <- liftIO $ eigh primmeOptions primmeOperator
  asks eData >>= \file ->
    withGroup' file hamiltonianPath $ \g -> do
      writeDataset' g "eigenvalues" evals
      writeDataset' g "eigenvectors" evecs
      writeDataset' g "residuals" rnorms
  return result
