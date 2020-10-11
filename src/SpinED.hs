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
    Symmetry (..),
    toSymmetry,
    getSector,
    getPeriodicity,
    getPhase,
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import Data.Aeson
import SpinED.Internal

data SpinEDException = SpinEDException Text
  deriving (Show)

instance Exception SpinEDException

data SymmetrySpec = SymmetrySpec
  { sPermutation :: [Int],
    sFlip :: Bool,
    sSector :: Int
  }
  deriving (Read, Show, Eq)

instance FromJSON SymmetrySpec where
  parseJSON = withObject "Symmetry" $ \v ->
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
