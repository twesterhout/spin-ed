-- |
-- Copyright: (c) 2020 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
--
-- User-friendly exact diagonalization for spin systems
module SpinED
  ( SpinEDException (..),
    LatticeSymmetriesException (..),
    SymmetrySpec (..),
    BasisSpec (..),
    Symmetry (..),
    SymmetryGroup (..),
    InteractionSpec (..),
    UserConfig (..),
    Environment (..),
    EnvT (..),
    runEnvT,
    toConfig,
    diagonalize,
    buildRepresentatives,
    computeExpectations,
    toSymmetry,
    toBasis,
    toInteraction,
    isRealInteraction,
    getSector,
    getPeriodicity,
    getPhase,
    mkGroup,
    getGroupSize,
    readConfig,
  )
where

import Colog
  ( HasLog (..),
    LogAction,
    Message,
    WithLog,
    log,
    logInfo,
    logWarning,
    richMessageAction,
    pattern D,
    pattern E,
    pattern I,
    pattern W,
  )
import Colog.Monad (liftLogAction)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, throw)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Complex
import qualified Data.HDF5 as H5
import Data.Scientific (toRealFloat)
import Data.Text (toLower)
import Data.Vector.Storable (MVector, Vector)
import qualified Data.Vector.Storable as V
import Data.Yaml (decodeFileWithWarnings)
import Foreign.Storable (Storable)
import Numeric.PRIMME
import SpinED.Internal

data SymmetrySpec = SymmetrySpec ![Int] !Bool !Int
  deriving (Read, Show, Eq)

instance FromJSON SymmetrySpec where
  parseJSON = withObject "symmetry" $ \v ->
    SymmetrySpec
      <$> v .: "permutation"
      <*> v .:! "flip" .!= False
      <*> v .: "sector"

data BasisSpec = BasisSpec !Int !(Maybe Int) ![SymmetrySpec]
  deriving (Read, Show, Eq)

instance FromJSON BasisSpec where
  parseJSON = withObject "basis" $ \v ->
    BasisSpec
      <$> v .: "number_spins"
      <*> v .:? "hamming_weight"
      <*> v .: "symmetries"

instance FromJSON (Complex Double) where
  parseJSON (Number x) = pure . fromReal . toRealFloat $ x
    where
      fromReal :: Num a => a -> Complex a
      fromReal x = x :+ 0
  parseJSON v@(Array xs) = case (toList xs) of
    [re, im] -> (:+) <$> parseJSON re <*> parseJSON im
    _ -> typeMismatch "Complex" v
  parseJSON v = typeMismatch "Complex" v

data InteractionSpec = InteractionSpec ![[Complex Double]] ![[Int]]
  deriving (Read, Show, Eq)

instance FromJSON InteractionSpec where
  parseJSON = withObject "interaction" $ \v ->
    InteractionSpec
      <$> v .: "matrix"
      <*> v .: "sites"

data OperatorSpec = OperatorSpec !Text ![InteractionSpec]
  deriving (Read, Show)

instance FromJSON OperatorSpec where
  parseJSON = withObject "operator" $ \v ->
    OperatorSpec
      <$> v .: "name"
      <*> v .: "terms"

data Datatype
  = DatatypeFloat32
  | DatatypeFloat64
  | DatatypeComplex64
  | DatatypeComplex128
  deriving (Read, Show, Eq)

instance FromJSON Datatype where
  parseJSON = withText "Datatype" $ \v ->
    case (toLower v) of
      "float32" -> undefined
      "float64" -> undefined
      "complex64" -> undefined
      "complex128" -> undefined
      _ ->
        fail . toString $
          "parsing Datatype failed, expected 'float32', "
            <> "'float64', 'complex64' or 'complex128', but got '"
            <> v
            <> "'"

data ConfigSpec = ConfigSpec !BasisSpec !OperatorSpec ![OperatorSpec] !Text !Int !Double !Datatype
  deriving (Read, Show)

instance FromJSON ConfigSpec where
  parseJSON = withObject "config" $ \v ->
    ConfigSpec
      <$> v .: "basis"
      <*> v .: "hamiltonian"
      <*> v .: "observables"
      <*> v .:! "output" .!= "exact_diagonalization_result.h5"
      <*> v .:! "number_vectors" .!= 1
      <*> v .:! "precision" .!= 0.0
      <*> v .:! "datatype" .!= DatatypeComplex128

toSymmetry :: (MonadIO m, MonadThrow m) => SymmetrySpec -> m Symmetry
toSymmetry (SymmetrySpec p f s) = mkSymmetry p f s

toBasis :: (MonadIO m, MonadThrow m) => BasisSpec -> m SpinBasis
toBasis (BasisSpec numberSpins hammingWeight symmetries) = do
  -- We need to make sure we use "small" basis, otherwise we won't be able to
  -- build a list of representatives later
  when (numberSpins > 64) . throw . SpinEDException $
    "invalid number_spins: " <> show numberSpins <> "; exact diagonalization is not feasible "
      <> "for systems larger than 64 spins"
  symmetryGroup <- mkGroup =<< mapM toSymmetry symmetries
  mkBasis symmetryGroup numberSpins hammingWeight

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
    cDatatype :: !Datatype
  }

data Environment m = Environment
  { eConfig :: !UserConfig,
    eLog :: LogAction m Message,
    eData :: H5.File
  }

newtype EnvT m a = EnvT {unEnvT :: ReaderT (Environment (EnvT m)) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runEnvT :: Monad m => EnvT m a -> Environment m -> m a
runEnvT action env = runReaderT (unEnvT action) (liftEnv env)
  where
    liftEnv x = x {eLog = liftLogAction (eLog x)}

deriving instance Monad m => MonadReader (Environment (EnvT m)) (EnvT m)

instance MonadTrans EnvT where
  lift = EnvT . lift

-- deriving instance MonadIO m => MonadIO (EnvT m)

instance HasLog (Environment m) Message m where
  getLogAction = eLog
  setLogAction action env = env {eLog = action}

toConfig :: (MonadIO m, MonadThrow m) => ConfigSpec -> m UserConfig
toConfig (ConfigSpec basisSpec hamiltonianSpec observablesSpecs output numEvals eps dtype) = do
  basis <- toBasis basisSpec
  hamiltonian <- toOperator basis hamiltonianSpec
  observables <- mapM (toOperator basis) observablesSpecs
  return $ UserConfig basis hamiltonian observables output numEvals eps dtype

readConfig :: (WithLog env Message m, MonadIO m) => FilePath -> m ConfigSpec
readConfig path = do
  log I "Parsing config file..."
  r <- liftIO $ decodeFileWithWarnings path
  case r of
    Left e -> liftIO $ throw e
    Right (warnings, config) -> do
      mapM_ (log W . show) warnings
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
    withGroup' file observablesPath $ \group -> writeDataset' group name measurements

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
  asks eData >>= \file ->
    withGroup' file basisPath $ \group ->
      writeDataset' group "representatives" =<< basisGetStates basis

writeDataset' :: (HasCallStack, MonadIO m, MonadMask m, H5.ToBlob a b) => H5.Group -> Text -> a -> EnvT m ()
writeDataset' group name x = do
  H5.exists group name >>= \alreadyExists -> when alreadyExists $ do
    prefix <- H5.getName group
    logWarning $ "Overwriting " <> prefix <> "/" <> name <> "..."
    H5.delete group name
  H5.writeDataset group name x

instance (Storable a, H5.KnownDatatype' a) => H5.ToBlob (Block a) a where
  toBlob (Block (d₁, d₂) stride v)
    | d₁ == stride = H5.Blob [d₁, d₂] v
    | otherwise = error "non-contiguous Blocks are not yet supported"

diagonalize ::
  forall a m.
  ( HasCallStack,
    MonadIO m,
    MonadMask m,
    BlasDatatype a,
    H5.KnownDatatype' a,
    H5.KnownDatatype' (BlasRealPart a)
  ) =>
  EnvT m (Vector (BlasRealPart a), Block a, Vector (BlasRealPart a))
diagonalize = do
  hamiltonian <- asks (cHamiltonian . eConfig)
  dim <- getNumberStates =<< asks (cBasis . eConfig)
  numEvals <- asks (cNumEvals . eConfig)
  eps <- asks (cEps . eConfig)
  logInfo $ "Diagonalizing " <> operatorName hamiltonian <> "..."
  let primmeOptions = PrimmeOptions dim numEvals PrimmeSmallest eps
      primmeOperator = inplaceApply . operatorObject $ hamiltonian
  result@(evals, evecs, rnorms) <- liftIO $ eigh primmeOptions primmeOperator
  asks eData >>= \file ->
    withGroup' file hamiltonianPath $ \group -> do
      writeDataset' group "eigenvalues" evals
      writeDataset' group "eigenvectors" evecs
      writeDataset' group "residuals" rnorms
  return result
