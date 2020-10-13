-- |
-- Copyright: (c) 2020 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
module SpinED.Internal where

import Control.Exception.Safe (MonadThrow, bracket, throw)
import Data.Complex
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr
import Foreign.Storable (Storable (..))
import System.IO.Unsafe (unsafePerformIO)

#include <lattice_symmetries/lattice_symmetries.h>

data LatticeSymmetriesException = LatticeSymmetriesException {eCode :: Int, eMessage :: Text}
  deriving (Show)

instance Exception LatticeSymmetriesException

data SpinEDException = SpinEDException Text
  deriving (Show)

instance Exception SpinEDException

foreign import ccall unsafe "ls_error_to_string" ls_error_to_string :: CInt -> IO CString

foreign import ccall unsafe "ls_destroy_string" ls_destroy_string :: CString -> IO ()

getErrorMessage :: Int -> IO Text
getErrorMessage c = bracket (ls_error_to_string (fromIntegral c)) ls_destroy_string $ \s ->
  toText <$> peekCString s

checkStatus :: (MonadIO m, MonadThrow m, Integral a) => a -> m ()
checkStatus c
  | c == 0 = return ()
  | otherwise =
    let c' = fromIntegral c
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
      if c == 0
        then (,) <$> pure c <*> peek ptrPtr
        else pure (c, nullPtr)
  checkStatus code
  fmap Symmetry . liftIO $ newForeignPtr ls_destroy_symmetry ptr

foreign import ccall unsafe "ls_create_group"
  ls_create_group :: Ptr (Ptr ()) -> CUInt -> Ptr (Ptr ()) -> IO CInt

foreign import ccall unsafe "&ls_destroy_group"
  ls_destroy_group :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "ls_get_group_size"
  ls_get_group_size :: Ptr () -> IO CUInt

newtype SymmetryGroup = SymmetryGroup (ForeignPtr ())

withSymmetries :: [Symmetry] -> (Int -> Ptr (Ptr ()) -> IO a) -> IO a
withSymmetries xs func = loop [] xs
  where
    withSymmetry (Symmetry p) = withForeignPtr p
    loop acc (y : ys) = withSymmetry y $ \y' -> loop (y' : acc) ys
    loop acc [] = withArrayLen (reverse acc) func

mkGroup :: (MonadIO m, MonadThrow m) => [Symmetry] -> m SymmetryGroup
mkGroup xs = do
  (code, ptr) <- liftIO $
    alloca $ \ptrPtr -> do
      c <- withSymmetries xs $ \n xsPtr ->
        ls_create_group ptrPtr (fromIntegral n) xsPtr
      if c == 0
        then (,) <$> pure c <*> peek ptrPtr
        else pure (c, nullPtr)
  checkStatus code
  fmap SymmetryGroup . liftIO $ newForeignPtr ls_destroy_group ptr

getGroupSize :: SymmetryGroup -> Int
getGroupSize (SymmetryGroup p) = unsafePerformIO $! withForeignPtr p $ \p' ->
  fromIntegral <$> ls_get_group_size p'
{-# NOINLINE getGroupSize #-}

foreign import ccall unsafe "ls_create_spin_basis"
  ls_create_spin_basis :: Ptr (Ptr ()) -> Ptr () -> CUInt -> CInt -> IO CInt

foreign import ccall unsafe "&ls_destroy_spin_basis"
  ls_destroy_spin_basis :: FunPtr (Ptr () -> IO ())

newtype SpinBasis = SpinBasis (ForeignPtr ())

mkBasis :: (MonadIO m, MonadThrow m) => SymmetryGroup -> Int -> Maybe Int -> m SpinBasis
mkBasis (SymmetryGroup group) numberSpins hammingWeight = do
  let hammingWeight' = case hammingWeight of
        Just x -> fromIntegral x
        Nothing -> (-1)
  (code, ptr) <- liftIO $
    alloca $ \ptrPtr -> do
      c <- withForeignPtr group $ \groupPtr ->
        ls_create_spin_basis ptrPtr groupPtr (fromIntegral numberSpins) hammingWeight'
      if c == 0
        then (,) <$> pure c <*> peek ptrPtr
        else pure (c, nullPtr)
  checkStatus code
  fmap SpinBasis . liftIO $ newForeignPtr ls_destroy_spin_basis ptr

foreign import ccall unsafe "ls_create_interaction1"
  ls_create_interaction1 :: Ptr (Ptr ()) -> Ptr (Complex Double) -> CUInt -> Ptr CUShort -> IO CInt

foreign import ccall unsafe "ls_create_interaction2"
  ls_create_interaction2 :: Ptr (Ptr ()) -> Ptr (Complex Double) -> CUInt -> Ptr CUShort -> IO CInt

foreign import ccall unsafe "ls_create_interaction3"
  ls_create_interaction3 :: Ptr (Ptr ()) -> Ptr (Complex Double) -> CUInt -> Ptr CUShort -> IO CInt

foreign import ccall unsafe "ls_create_interaction4"
  ls_create_interaction4 :: Ptr (Ptr ()) -> Ptr (Complex Double) -> CUInt -> Ptr CUShort -> IO CInt

foreign import ccall unsafe "&ls_destroy_interaction"
  ls_destroy_interaction :: FunPtr (Ptr () -> IO ())

newtype Interaction = Interaction (ForeignPtr ())

mkInteraction :: (MonadIO m, MonadThrow m) => Int -> Vector (Complex Double) -> Vector Int -> m Interaction
mkInteraction n matrix sites = do
  (code, ptr) <- liftIO $
    alloca $ \ptrPtr -> do
      c <- V.unsafeWith matrix $ \matrixPtr ->
        V.unsafeWith (V.map fromIntegral sites) $ \sitesPtr ->
          create ptrPtr matrixPtr numberSites sitesPtr
      if c == 0
        then (,) <$> pure c <*> peek ptrPtr
        else pure (c, nullPtr)
  checkStatus code
  fmap Interaction . liftIO $ newForeignPtr ls_destroy_interaction ptr
  where
    numberSites = fromIntegral $ V.length sites `div` n
    create =
      case n of
        1 -> ls_create_interaction1
        2 -> ls_create_interaction2
        3 -> ls_create_interaction3
        4 -> ls_create_interaction4
        _ -> error $ "invalid n: " <> show n
