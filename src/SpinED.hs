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
    toConfig,
    diagonalize,
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
    richMessageAction,
    pattern D,
    pattern E,
    pattern I,
    pattern W,
  )
import Control.Exception.Safe (MonadThrow, throw)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Complex
import Data.Scientific (toRealFloat)
import Data.Vector.Storable (MVector, Vector)
import qualified Data.Vector.Storable as V
import Data.Yaml (decodeFileWithWarnings)
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

data ConfigSpec = ConfigSpec !BasisSpec !OperatorSpec ![OperatorSpec] !Text !Int !Double
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

data Operator = Operator !Text !Operator'

toOperator :: (MonadIO m, MonadThrow m) => SpinBasis -> OperatorSpec -> m Operator
toOperator basis (OperatorSpec name terms) =
  Operator <$> pure name <*> (mkOperator basis =<< mapM toInteraction terms)

data UserConfig = UserConfig
  { cBasis :: !SpinBasis,
    cHamiltonian :: !Operator,
    cObservables :: ![Operator],
    cOutput :: Text,
    cNumEvals :: Int,
    cEps :: Double
  }

toConfig :: (MonadIO m, MonadThrow m) => ConfigSpec -> m UserConfig
toConfig (ConfigSpec basisSpec hamiltonianSpec observablesSpecs output numEvals eps) = do
  basis <- toBasis basisSpec
  hamiltonian <- toOperator basis hamiltonianSpec
  observables <- mapM (toOperator basis) observablesSpecs
  return $ UserConfig basis hamiltonian observables output numEvals eps

readConfig :: (WithLog env Message m, MonadIO m) => FilePath -> m ConfigSpec
readConfig path = do
  log I "Parsing config file..."
  r <- liftIO $ decodeFileWithWarnings path
  case r of
    Left e -> liftIO $ throw e
    Right (warnings, config) -> do
      mapM_ (log W . show) warnings
      return config

-- batchedExpectationValue :: (WithLog env Message m, MonadIO m, MonadThrow m) => Operator -> Block a -> Vector Double
-- batchedExpectationValue = undefined

diagonalize :: (WithLog env Message m, MonadIO m, MonadThrow m) => UserConfig -> m [(Double, Vector Double, Double)]
diagonalize config = do
  log I "Building a list of representatives..."
  buildBasis (cBasis config)
  dim <- getNumberStates . cBasis $ config
  log I $ "Hilbert space dimension is " <> show dim
  let primmeOptions = PrimmeOptions dim (cNumEvals config) PrimmeSmallest (cEps config)
  log I "Diagonalizing..."
  let primmeOperator = case (cHamiltonian config) of (Operator _ op) -> fromOperator' op
  undefined

-- liftIO $ eigh primmeOptions primmeOperator
