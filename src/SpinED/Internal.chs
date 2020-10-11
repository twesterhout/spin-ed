-- |
-- Copyright: (c) 2020 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
module SpinED.Internal where

import Control.Exception.Safe (MonadThrow, throw, bracket)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool)
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable (Storable (..))
import System.IO.Unsafe (unsafePerformIO)

#include <lattice_symmetries/lattice_symmetries.h>


data LatticeSymmetriesException = LatticeSymmetriesException { eCode :: Int, eMessage :: Text }
  deriving (Show)

instance Exception LatticeSymmetriesException

foreign import ccall unsafe "ls_error_to_string" ls_error_to_string :: CInt -> IO CString
foreign import ccall unsafe "ls_destroy_string" ls_destroy_string :: CString -> IO ()

getErrorMessage :: Int -> IO Text
getErrorMessage c = bracket (ls_error_to_string (fromIntegral c)) ls_destroy_string $ \s ->
  toText <$> peekCString s

checkStatus :: (MonadIO m, MonadThrow m, Integral a) => a -> m ()
checkStatus c
  | c == 0 = return ()
  | otherwise = let c' = fromIntegral c
                 in throw . LatticeSymmetriesException c' =<< liftIO (getErrorMessage c')

newtype Symmetry = Symmetry (ForeignPtr ())

foreign import ccall unsafe "ls_create_symmetry"
  ls_create_symmetry :: Ptr (Ptr ()) -> CUInt -> Ptr CUInt -> CBool -> CUInt -> IO CInt

foreign import ccall unsafe "&ls_destroy_symmetry"
  ls_destroy_symmetry :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "ls_get_sector" ls_get_sector :: Ptr () -> IO CUInt
foreign import ccall unsafe "ls_get_flip" ls_get_flip :: Ptr () -> IO CBool
foreign import ccall unsafe "ls_get_phase" ls_get_phase :: Ptr () -> IO CDouble
foreign import ccall unsafe "ls_get_periodicity" ls_get_periodicity :: Ptr () -> IO CUInt

getSector :: Symmetry -> Int
getSector (Symmetry p) = unsafePerformIO $! withForeignPtr p $ \p' ->
  fromIntegral <$> ls_get_sector p'
{-# NOINLINE getSector #-}

getPeriodicity :: Symmetry -> Int
getPeriodicity (Symmetry p) = unsafePerformIO $! withForeignPtr p $ \p' ->
  fromIntegral <$> ls_get_periodicity p'
{-# NOINLINE getPeriodicity #-}

getPhase :: Symmetry -> Double
getPhase (Symmetry p) = unsafePerformIO $! withForeignPtr p $ \p' ->
  coerce <$> ls_get_phase p'
{-# NOINLINE getPhase #-}

mkSymmetry :: (MonadIO m, MonadThrow m) => [Int] -> Bool -> Int -> m Symmetry
mkSymmetry permutation invert sector = do
  (code, ptr) <- liftIO $
    alloca $ \ptrPtr -> do
      c <- withArrayLen (fromIntegral <$> permutation) $ \n permutationPtr ->
        ls_create_symmetry ptrPtr (fromIntegral n) permutationPtr (fromBool invert) (fromIntegral sector)
      if c == 0 then (,) <$> pure c <*> peek ptrPtr
                else pure (c, nullPtr)
  checkStatus code
  fmap Symmetry . liftIO $ newForeignPtr ls_destroy_symmetry ptr


newtype SymmetryGroup = SymmetryGroup (ForeignPtr ())

mkGroup :: (MonadIO m, MonadThrow m) => [Symmetry] -> m Group
mkGroup = undefined

newtype SpinBasis = SpinBasis (ForeignPtr ())

mkBasis :: (MonadIO m, MonadThrow m) => Group -> Int -> Maybe Int -> m SpinBasis
mkBasis = undefined
