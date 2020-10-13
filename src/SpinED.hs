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
    toSymmetry,
    toInteraction,
    isRealInteraction,
    getSector,
    getPeriodicity,
    getPhase,
    mkGroup,
    getGroupSize,
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Complex
import Data.Scientific (toRealFloat)
import Data.Vector.Storable (MVector, Vector)
import qualified Data.Vector.Storable as V
import SpinED.Internal

data SymmetrySpec = SymmetrySpec ![Int] !Bool !Int
  deriving (Read, Show, Eq)

instance FromJSON SymmetrySpec where
  parseJSON = withObject "symmetry" $ \v ->
    SymmetrySpec
      <$> v .: "permutation"
      <*> v .:! "flip" .!= False
      <*> v .: "sector"

validatePermutation :: MonadThrow m => [Int] -> m [Int]
validatePermutation indices = do
  let n = length indices
  when (n == 0) . throw . SpinEDException $
    "permutation " <> show indices <> " is too short: the system must contain at least one spin"
  when (n > 64) . throw . SpinEDException $
    "permutation " <> show indices <> " is too long: exact diagonalization is not feasible "
      <> "for systems larger than 64 spins"
  when (any (< 0) indices) . throw . SpinEDException $
    "permutation " <> show indices <> " contains negative indices"
  let indices' = sort indices
      i₀ = case (viaNonEmpty head indices') of
        Just x -> x
        Nothing -> error "n must be positive"
  when (indices' /= [i₀ .. i₀ + n - 1]) . throw . SpinEDException $
    "permutation " <> show indices <> " is not a valid permutation of [" <> show i₀ <> ".." <> show (i₀ + n - 1) <> "]"
  return $ fmap (\x -> x - i₀) indices

validateSector :: MonadThrow m => Int -> m Int
validateSector sector
  | sector >= 0 = return sector
  | otherwise = throw . SpinEDException $ "invalid sector: " <> show sector <> "; expected a non-negative number"

toSymmetry :: (MonadIO m, MonadThrow m) => SymmetrySpec -> m Symmetry
toSymmetry (SymmetrySpec p f s) = do
  p' <- validatePermutation p
  s' <- validateSector s
  mkSymmetry p' f s'

data BasisSpec = BasisSpec !Int !(Maybe Int) ![SymmetrySpec]
  deriving (Read, Show, Eq)

instance FromJSON BasisSpec where
  parseJSON = withObject "basis" $ \v ->
    BasisSpec
      <$> v .: "number_spins"
      <*> v .:? "hamming_weight"
      <*> v .: "symmetries"

toBasis :: (MonadIO m, MonadThrow m) => BasisSpec -> m SpinBasis
toBasis (BasisSpec numberSpins hammingWeight symmetries) = do
  when (numberSpins <= 0) . throw . SpinEDException $
    "invalid number_spins: " <> show numberSpins <> "; expected a positive number"
  when (numberSpins > 64) . throw . SpinEDException $
    "invalid number_spins: " <> show numberSpins <> "; exact diagonalization is not feasible "
      <> "for systems larger than 64 spins"
  case hammingWeight of
    Just m -> do
      when (m < 0) . throw . SpinEDException $
        "invalid hamming_weight: " <> show m <> "; expected a non-negative number"
      when (m > numberSpins) . throw . SpinEDException $
        "invalid hamming_weight: " <> show m <> "; Hamming weight cannot exceed the number of spins"
    Nothing -> return ()
  symmetryGroup <- mkGroup =<< mapM toSymmetry symmetries
  mkBasis symmetryGroup numberSpins hammingWeight

fromReal :: Num a => a -> Complex a
fromReal x = x :+ 0

instance FromJSON (Complex Double) where
  parseJSON (Number x) = pure . fromReal . toRealFloat $ x
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

validateMatrix :: MonadThrow m => Int -> [[Complex Double]] -> m (Vector (Complex Double))
validateMatrix dim rows = do
  when (length rows /= dim) . throw . SpinEDException $
    "invalid matrix: " <> show rows <> "; expected a square matrix of dimension " <> show dim
  when (any ((/= dim) . length) rows) . throw . SpinEDException $
    "invalid matrix: " <> show rows <> "; expected a square matrix of dimension " <> show dim
  return . V.fromList . concat $ rows

validateSites :: MonadThrow m => [[Int]] -> m (Int, Vector Int)
validateSites [] =
  throw . SpinEDException $
    "sites array should not be empty, if you do not wish to apply this "
      <> "interaction to any sites, then do not include it at all"
validateSites rows@(r : rs) = do
  let !dim = length r
  when (any ((/= dim) . length) rs) . throw . SpinEDException $
    "invalid sites: " <> show rows <> "; expected an array of length-" <> show dim <> " tuples"
  return (dim, V.fromList (concat rows))

toInteraction :: (MonadIO m, MonadThrow m) => InteractionSpec -> m Interaction
toInteraction (InteractionSpec matrix sites) = do
  (n, sites') <- validateSites sites
  when (n /= 1 && n /= 2 && n /= 3 && n /= 4) . throw . SpinEDException $
    "currently only 1-, 2-, 3-, and 4-point interactions are supported, but received n=" <> show n
  matrix' <- validateMatrix (2 ^ n) matrix
  mkInteraction n matrix' sites'

data OperatorSpec = OperatorSpec !Text ![InteractionSpec]
  deriving (Read, Show)

data ConfigSpec = ConfigSpec !BasisSpec !OperatorSpec ![OperatorSpec]
  deriving (Read, Show)

data UserConfig = UserConfig
  { cBasis :: !SpinBasis,
    cHamiltonian :: !Operator,
    cObservables :: ![Operator]
  }
