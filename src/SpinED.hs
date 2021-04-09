-- |
-- Copyright: (c) 2020 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
--
-- User-friendly exact diagonalization for spin systems
module SpinED
  ( -- * For the app.
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
    prepareOutputFile,

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
    ls_enable_logging,
  )
where

import Colog
  ( HasLog (..),
    LogAction (..),
    Message,
    WithLog,
    logDebug,
    logInfo,
    logWarning,
  )
import Colog.Core.Action (hoistLogAction)
import Colog.Monad (liftLogAction)
import Control.Monad.IO.Unlift (askRunInIO)
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Complex
import qualified Data.HDF5 as H5
import Data.Scientific (toRealFloat)
import Data.Text (toLower)
import qualified Data.Text.IO as T
import Data.Vector.Storable (Vector)
import Data.Yaml (decodeFileWithWarnings)
import Foreign.Storable (Storable)
import Numeric.PRIMME
import Paths_spin_ed (version)
import SpinED.Internal
import UnliftIO.Exception (throwIO)
import Prelude

data SymmetrySpec = SymmetrySpec ![Int] !Int
  deriving stock (Read, Show, Eq)

instance FromJSON SymmetrySpec where
  parseJSON = withObject "symmetry" $ \v ->
    SymmetrySpec
      <$> v .: "permutation"
      <*> v .: "sector"

instance ToJSON SymmetrySpec where
  toJSON (SymmetrySpec permutation sector) =
    object ["permutation" .= permutation, "sector" .= sector]

data BasisSpec = BasisSpec !Int !(Maybe Int) !(Maybe Int) ![SymmetrySpec]
  deriving stock (Read, Show, Eq)

instance FromJSON BasisSpec where
  parseJSON = withObject "basis" $ \v ->
    BasisSpec
      <$> v .: "number_spins"
      <*> v .:? "hamming_weight"
      <*> v .:? "spin_inversion"
      <*> v .: "symmetries"

instance ToJSON BasisSpec where
  toJSON (BasisSpec numberSpins hammingWeight spinInversion symmetries) =
    object
      [ "number_spins" .= numberSpins,
        "hamming_weight" .= maybe Null toJSON hammingWeight,
        "spin_inversion" .= maybe Null toJSON spinInversion,
        "symmetries" .= symmetries
      ]

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

data ConfigSpec = ConfigSpec !BasisSpec !OperatorSpec ![OperatorSpec] !Text !Int !Double !Datatype !Int !Int !Int
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
      <*> v .:! "max_primme_basis_size" .!= (pMaxBasisSize primmeDefaults)
      <*> v .:! "max_primme_block_size" .!= (pMaxBlockSize primmeDefaults)
      <*> v .:! "min_primme_restart_size" .!= (pMinRestartSize primmeDefaults)

toSymmetry :: MonadIO m => SymmetrySpec -> m Symmetry
toSymmetry (SymmetrySpec p s) = mkSymmetry p s

toBasis :: (WithLog env Message m, MonadIO m) => BasisSpec -> m SpinBasis
toBasis (BasisSpec numberSpins hammingWeight spinInversion symmetries) = do
  -- We need to make sure we use "small" basis, otherwise we won't be able to
  -- build a list of representatives later
  when (numberSpins > 64) . throwIO . SpinEDException $
    "invalid number_spins: " <> show numberSpins <> "; exact diagonalization is not feasible "
      <> "for systems larger than 64 spins"
  logDebug "Building symmetry group..."
  symmetryGroup <- mkGroup =<< mapM toSymmetry symmetries
  logInfo $ "Symmetry group contains " <> show (getGroupSize symmetryGroup) <> " elements"
  mkBasis symmetryGroup numberSpins hammingWeight spinInversion

toInteraction :: MonadIO m => InteractionSpec -> m Interaction
toInteraction (InteractionSpec matrix sites) = mkInteraction' matrix sites

data Operator = Operator
  { operatorName :: !Text,
    operatorObject :: !Operator'
  }

toOperator :: MonadIO m => SpinBasis -> OperatorSpec -> m Operator
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
    cMaxBasisSize :: !Int,
    cMaxBlockSize :: !Int,
    cMinRestartSize :: !Int
  }

data Environment m = Environment
  { eConfig :: !UserConfig,
    eLog :: !(LogAction m Message)
  }

newtype EnvT m a = EnvT {unEnvT :: ReaderT (Environment (EnvT m)) m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

deriving newtype instance Monad m => MonadReader (Environment (EnvT m)) (EnvT m)

deriving newtype instance MonadUnliftIO m => MonadUnliftIO (EnvT m)

runEnvT :: Monad m => EnvT m a -> Environment m -> m a
runEnvT action env = runReaderT (unEnvT action) (liftEnv env)
  where
    liftEnv x = x {eLog = liftLogAction (eLog x)}

instance MonadTrans EnvT where
  lift = EnvT . lift

instance HasLog (Environment m) Message m where
  getLogAction = eLog
  setLogAction action env = env {eLog = action}

instance Monad m => HasLog (Environment m) Message (ResourceT m) where
  getLogAction = hoistLogAction lift . eLog
  setLogAction action env = error "setLogAction is not supported inside ResourceT"

toConfig :: (WithLog env Message m, MonadIO m) => ConfigSpec -> m UserConfig
toConfig (ConfigSpec basisSpec hamiltonianSpec observablesSpecs output numEvals eps dtype maxBasisSize maxBlocksize minRestartSize) = do
  basis <- toBasis basisSpec
  hamiltonian <- toOperator basis hamiltonianSpec
  observables <- mapM (toOperator basis) observablesSpecs
  return $ UserConfig basis hamiltonian observables output numEvals eps dtype maxBasisSize maxBlocksize minRestartSize

readConfig :: (WithLog env Message m, MonadIO m) => Text -> m ConfigSpec
readConfig path = do
  logDebug "Parsing config file..."
  r <- liftIO $ decodeFileWithWarnings (toString path)
  case r of
    Left e -> throwIO e
    Right (warnings, config) -> do
      mapM_ (logWarning . show) warnings
      return config

-- withGroup' :: (MonadIO m, MonadMask m) => H5.File -> Text -> (H5.Group -> m a) -> m a
-- withGroup' file name func = do
--   H5.exists file name >>= \exists ->
--     unless exists $
--       H5.makeGroup file name
--   H5.byName file name $ H5.matchM @H5.Group $ func

observablesPath :: Text
observablesPath = "/observables"

basisPath :: Text
basisPath = "/basis"

hamiltonianPath :: Text
hamiltonianPath = "/hamiltonian"

workspacePath :: Text
workspacePath = "/_workspace"

createGroupWhenNeeded :: H5.MonadResource m => H5.Group -> Text -> m ()
createGroupWhenNeeded group path = do
  alreadyExists <- H5.exists group path
  unless alreadyExists $ H5.createGroup group path

withOutputFile :: (HasCallStack, MonadUnliftIO m) => (H5.Group -> ResourceT (EnvT m) a) -> EnvT m a
withOutputFile action =
  asks (cOutput . eConfig) >>= \filename ->
    H5.withFile' filename H5.WriteAppend action

prepareOutputFile :: (HasCallStack, MonadUnliftIO m) => EnvT m ()
prepareOutputFile = withOutputFile $ \file -> do
  createGroupWhenNeeded file basisPath
  createGroupWhenNeeded file hamiltonianPath
  createGroupWhenNeeded file observablesPath
  createGroupWhenNeeded file workspacePath

computeExpectation ::
  (MonadUnliftIO m, BlasDatatype a, H5.KnownDatatype a) =>
  Operator ->
  Block a ->
  EnvT m ()
computeExpectation (Operator name operator) x = do
  logInfo $ "Computing expectation values of " <> name <> "..."
  measurements <- liftIO $ expectation operator x
  logInfo $ "Obtained expectation values " <> show measurements
  withOutputFile $ \file -> do
    group <- H5.openGroup file observablesPath
    writeDataset group name x

computeExpectations ::
  (MonadUnliftIO m, BlasDatatype a, H5.KnownDatatype a) =>
  Block a ->
  EnvT m ()
computeExpectations x = asks (cObservables . eConfig) >>= mapM_ (flip computeExpectation x)

buildRepresentatives :: (HasCallStack, MonadUnliftIO m) => EnvT m ()
buildRepresentatives = do
  basis <- asks (cBasis . eConfig)
  let datasetName = "representatives"
  (representatives :: Maybe (Vector Word64)) <-
    withOutputFile $ \file -> do
      group <- H5.openGroup file basisPath
      hasRepresentatives <- H5.exists group datasetName
      if hasRepresentatives
        then do
          logInfo $ "Loading representatives from " <> basisPath <> "/" <> datasetName <> "..."
          H5.openDataset group datasetName >>= \dataset ->
            Just <$> H5.readDataset dataset
        else return Nothing
  unless (isJust representatives) $
    logInfo "Building a list of representatives..."
  buildBasis basis representatives
  dim <- getNumberStates basis
  logInfo $ "Hilbert space dimension is " <> show dim
  unless (isJust representatives) $
    withOutputFile $ \file -> do
      group <- H5.openGroup file basisPath
      H5.writeDataset group datasetName =<< basisGetStates basis

isOperatorReal :: MonadIO m => Operator -> m Bool
isOperatorReal (Operator _ op) = liftIO $ isOperatorReal' op

type SupportedDatatype a = (H5.KnownDatatype a, H5.KnownDatatype (BlasRealPart a), BlasDatatype a)

withDatatype ::
  Bool ->
  Datatype ->
  (forall a. SupportedDatatype a => Proxy a -> b) ->
  b
withDatatype True DatatypeFloat32 f = f (Proxy @Float)
withDatatype True DatatypeFloat64 f = f (Proxy @Double)
withDatatype False DatatypeFloat32 f = f (Proxy @(Complex Float))
withDatatype False DatatypeFloat64 f = f (Proxy @(Complex Double))

writeDataset :: (HasCallStack, H5.KnownDataset a, WithLog env Message m, MonadResource m) => H5.Group -> Text -> a -> m ()
writeDataset g name x = do
  H5.exists g name >>= \alreadyExists -> when alreadyExists $ do
    prefix <- H5.getName g
    logWarning $ "Overwriting " <> prefix <> "/" <> name <> "..."
    H5.delete g name
  H5.writeDataset g name x

instance (BlasDatatype a, H5.KnownDatatype a) => H5.KnownDataset (Block a) where
  -- Block is in column-major format, but TemporaryStridedMatrix -- in row-major
  withArrayView (Block (d₀, d₁) s₁ v) action =
    H5.withArrayView (H5.TemporaryStridedMatrix (d₁, d₀) s₁ v) action
  peekArrayView view =
    H5.peekArrayView view >>= \m -> case m of
      (H5.TemporaryStridedMatrix (d₀, d₁) s₀ v) -> return (Block (d₁, d₀) s₀ v)

diagonalize ::
  forall a m proxy.
  (HasCallStack, MonadUnliftIO m, SupportedDatatype a) =>
  proxy a ->
  EnvT m (Vector (BlasRealPart a), Block a, Vector (BlasRealPart a))
diagonalize _ = do
  config <- asks eConfig
  dim <- getNumberStates (cBasis config)
  runLoggerInIO <- askRunInIO
  let primmeMonitor :: BlasDatatype t => PrimmeInfo t -> IO Bool
      primmeMonitor info = do
        runLoggerInIO . logInfo $ primmePrettyInfo info
        return False
  primmeOptions <-
    liftIO . finalizeOptions $
      primmeDefaults
        { pDim = dim,
          pNumEvals = cNumEvals config,
          pTarget = PrimmeSmallest,
          pEps = cEps config,
          pMaxBasisSize = cMaxBasisSize config,
          pMaxBlockSize = cMaxBlockSize config,
          pMinRestartSize = cMinRestartSize config,
          pLogAction = Right (PrimmeMonitor primmeMonitor)
        }
  logDebug $
    "Using max_primme_basis_size=" <> show (pMaxBasisSize primmeOptions)
      <> ", max_primme_block_size="
      <> show (pMaxBlockSize primmeOptions)
      <> ", min_primme_restart_size="
      <> show (pMinRestartSize primmeOptions)
  let hamiltonian = cHamiltonian config
      primmeOperator = inplaceApply . operatorObject $ hamiltonian
  logInfo $ "Diagonalizing " <> operatorName hamiltonian <> "..."
  result@(evals, evecs, rnorms) <- liftIO $ eigh primmeOptions primmeOperator
  logInfo $ "Obtained eigenvalues " <> show evals
  withOutputFile $ \file -> do
    g <- H5.openGroup file hamiltonianPath
    writeDataset g "eigenvalues" evals
    writeDataset g "eigenvectors" evecs
    writeDataset g "residuals" rnorms
  return result
