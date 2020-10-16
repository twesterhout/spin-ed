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

-- | Exceptions thrown when an error occurs in @liblattice_symmetries@.
data LatticeSymmetriesException = LatticeSymmetriesException {eCode :: Int, eMessage :: Text}
  deriving (Show)

instance Exception LatticeSymmetriesException

data SpinEDException = SpinEDException Text
  deriving (Show)

instance Exception SpinEDException

foreign import ccall unsafe "ls_error_to_string" ls_error_to_string :: CInt -> IO CString

foreign import ccall unsafe "ls_destroy_string" ls_destroy_string :: CString -> IO ()

-- | Retrieve textual representation of an error
getErrorMessage ::
  -- | Error code returned by @lattice_symmetries@ library
  Int ->
  -- | Explanation of the error
  IO Text
getErrorMessage c = bracket (ls_error_to_string (fromIntegral c)) ls_destroy_string $ \s ->
  toText <$> peekCString s

-- | Check the status code returned by @lattice_symmetries@ library. If it
-- indicates an error, 'LatticeSymmetriesException' is thrown.
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

mkSymmetry ::
  (MonadIO m, MonadThrow m) =>
  -- | Permutation
  [Int] ->
  -- | Whether to apply spin inversion
  Bool ->
  -- | Symmetry sector
  Int ->
  m Symmetry
mkSymmetry !permutation !invert !sector = do
  -- Make sure permutation and sector can be safely converted to unsigned
  -- representations. Everything else is checked by ls_create_symmetry
  when (any (< 0) permutation) . throw . SpinEDException $
    "invalid permutation: " <> show permutation <> "; indices must be non-negative"
  when (sector < 0) . throw . SpinEDException $
    "invalid sector: " <> show sector <> "; expected a non-negative number"
  (code, ptr) <- liftIO $
    alloca $ \ptrPtr -> do
      c <- withArrayLen (fromIntegral <$> permutation) $ \n permutationPtr ->
        ls_create_symmetry ptrPtr (fromIntegral n) permutationPtr (fromBool invert) (fromIntegral sector)
      if c == 0
        then (,) <$> pure c <*> peek ptrPtr
        else pure (c, nullPtr)
  checkStatus code
  fmap Symmetry . liftIO $ newForeignPtr ls_destroy_symmetry ptr

newtype SymmetryGroup = SymmetryGroup (ForeignPtr ())

foreign import ccall unsafe "ls_create_group"
  ls_create_group :: Ptr (Ptr ()) -> CUInt -> Ptr (Ptr ()) -> IO CInt

foreign import ccall unsafe "&ls_destroy_group"
  ls_destroy_group :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "ls_get_group_size"
  ls_get_group_size :: Ptr () -> IO CUInt

-- | Extension of 'withForeignPtr' to lists of 'ForeignPtr's.
withManyForeignPtr :: [ForeignPtr a] -> (Int -> Ptr (Ptr a) -> IO b) -> IO b
withManyForeignPtr xs func = loop [] xs
  where
    loop acc (y : ys) = withForeignPtr y $ \y' -> loop (y' : acc) ys
    loop acc [] = withArrayLen (reverse acc) func

withSymmetries :: [Symmetry] -> (Int -> Ptr (Ptr ()) -> IO a) -> IO a
withSymmetries xs func = withManyForeignPtr pointers func
  where
    pointers = (\(Symmetry p) -> p) <$> xs

mkGroup :: (MonadIO m, MonadThrow m) => [Symmetry] -> m SymmetryGroup
mkGroup !xs = do
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

newtype SpinBasis = SpinBasis (ForeignPtr ())

foreign import ccall unsafe "ls_create_spin_basis"
  ls_create_spin_basis :: Ptr (Ptr ()) -> Ptr () -> CUInt -> CInt -> IO CInt

foreign import ccall unsafe "&ls_destroy_spin_basis"
  ls_destroy_spin_basis :: FunPtr (Ptr () -> IO ())

mkBasis :: (MonadIO m, MonadThrow m) => SymmetryGroup -> Int -> Maybe Int -> m SpinBasis
mkBasis !(SymmetryGroup group) !numberSpins !hammingWeight = do
  when (numberSpins <= 0) . throw . SpinEDException $
    "invalid number of spins: " <> show numberSpins <> "; expected a positive number"
  hammingWeight' <- case hammingWeight of
    Just x -> do
      when (x < 0) . throw . SpinEDException $
        "invalid Hamming weight: " <> show x <> "; expected a non-negative number"
      return $ fromIntegral x
    Nothing -> return (-1)
  (code, ptr) <- liftIO $
    alloca $ \ptrPtr -> do
      c <- withForeignPtr group $ \groupPtr ->
        ls_create_spin_basis ptrPtr groupPtr (fromIntegral numberSpins) hammingWeight'
      if c == 0
        then (,) <$> pure c <*> peek ptrPtr
        else pure (c, nullPtr)
  checkStatus code
  fmap SpinBasis . liftIO $ newForeignPtr ls_destroy_spin_basis ptr

newtype Interaction = Interaction (ForeignPtr ())

type CreateInteraction = Ptr (Ptr ()) -> Ptr (Complex Double) -> CUInt -> Ptr CUShort -> IO CInt

foreign import ccall unsafe "ls_create_interaction1"
  ls_create_interaction1 :: Ptr (Ptr ()) -> Ptr (Complex Double) -> CUInt -> Ptr CUShort -> IO CInt

foreign import ccall unsafe "ls_create_interaction2"
  ls_create_interaction2 :: Ptr (Ptr ()) -> Ptr (Complex Double) -> CUInt -> Ptr CUShort -> IO CInt

foreign import ccall unsafe "ls_create_interaction3"
  ls_create_interaction3 :: Ptr (Ptr ()) -> Ptr (Complex Double) -> CUInt -> Ptr CUShort -> IO CInt

foreign import ccall unsafe "ls_create_interaction4"
  ls_create_interaction4 :: Ptr (Ptr ()) -> Ptr (Complex Double) -> CUInt -> Ptr CUShort -> IO CInt

foreign import ccall unsafe "ls_interaction_is_real"
  ls_interaction_is_real :: Ptr () -> IO CBool

foreign import ccall unsafe "&ls_destroy_interaction"
  ls_destroy_interaction :: FunPtr (Ptr () -> IO ())

toMatrix ::
  (MonadThrow m, Show r, Real r) =>
  -- | Expected dimension @n@ of the matrix
  Int ->
  -- | Square @matrix@ of dimension @n@
  [[Complex r]] ->
  -- | Row-major representation of @matrix@
  m (Vector (Complex Double))
toMatrix !dim !rows = do
  when (length rows /= dim) . throw . SpinEDException $
    "invalid matrix: " <> show rows <> "; expected a square matrix of dimension " <> show dim
  when (any ((/= dim) . length) rows) . throw . SpinEDException $
    "invalid matrix: " <> show rows <> "; expected a square matrix of dimension " <> show dim
  return . V.fromList . fmap (fmap (fromRational . toRational)) . concat $ rows

class MakeInteraction a where
  mkInteraction' :: (MonadIO m, MonadThrow m, Show r, Real r) => [[Complex r]] -> [a] -> m Interaction

instance MakeInteraction Int where
  mkInteraction' matrix sites =
    toMatrix 2 matrix >>= \matrix' ->
      unsafeMkInteraction ls_create_interaction1 (length sites) matrix' sites'
    where
      sites' = V.fromList sites

instance MakeInteraction (Int, Int) where
  mkInteraction' matrix sites =
    toMatrix 4 matrix >>= \matrix' ->
      unsafeMkInteraction ls_create_interaction2 (length sites) matrix' sites'
    where
      sites' = V.fromList $ sites >>= \(x₁, x₂) -> [x₁, x₂]

instance MakeInteraction (Int, Int, Int) where
  mkInteraction' matrix sites =
    toMatrix 8 matrix >>= \matrix' ->
      unsafeMkInteraction ls_create_interaction3 (length sites) matrix' sites'
    where
      sites' = V.fromList $ sites >>= \(x₁, x₂, x₃) -> [x₁, x₂, x₃]

instance MakeInteraction (Int, Int, Int, Int) where
  mkInteraction' matrix sites =
    toMatrix 16 matrix >>= \matrix' ->
      unsafeMkInteraction ls_create_interaction4 (length sites) matrix' sites'
    where
      sites' = V.fromList $ sites >>= \(x₁, x₂, x₃, x₄) -> [x₁, x₂, x₃, x₄]

instance MakeInteraction [Int] where
  mkInteraction' matrix rows@(r : _) = case n of
    1 -> mkInteraction' matrix =<< mapM match1 rows
    2 -> mkInteraction' matrix =<< mapM match2 rows
    3 -> mkInteraction' matrix =<< mapM match3 rows
    4 -> mkInteraction' matrix =<< mapM match4 rows
    _ ->
      throw . SpinEDException $
        "currently only 1-, 2-, 3-, and 4-point interactions are supported, but received n=" <> show n
    where
      n = length r
      match1 [x₁] = return x₁
      match1 _ = throw failure
      match2 [x₁, x₂] = return (x₁, x₂)
      match2 _ = throw failure
      match3 [x₁, x₂, x₃] = return (x₁, x₂, x₃)
      match3 _ = throw failure
      match4 [x₁, x₂, x₃, x₄] = return (x₁, x₂, x₃, x₄)
      match4 _ = throw failure
      failure =
        SpinEDException $
          "invalid sites: " <> show rows <> "; expected an array of length-" <> show n <> " tuples"

unsafeMkInteraction :: (MonadIO m, MonadThrow m) => CreateInteraction -> Int -> Vector (Complex Double) -> Vector Int -> m Interaction
unsafeMkInteraction ffiCreate numberSites matrix sites = do
  when (V.any (< 0) sites) . throw . SpinEDException $ "site indices must be all non-negative numbers"
  (code, ptr) <- liftIO $
    alloca $ \ptrPtr -> do
      c <- V.unsafeWith matrix $ \matrixPtr ->
        V.unsafeWith (V.map fromIntegral sites) $ \sitesPtr ->
          ffiCreate ptrPtr matrixPtr (fromIntegral numberSites) sitesPtr
      if c == 0
        then (,) <$> pure c <*> peek ptrPtr
        else pure (c, nullPtr)
  checkStatus code
  fmap Interaction . liftIO $ newForeignPtr ls_destroy_interaction ptr

isRealInteraction :: Interaction -> Bool
isRealInteraction (Interaction p) = unsafePerformIO $! withForeignPtr p $ \p' ->
  toEnum . fromIntegral <$> ls_interaction_is_real p'
{-# NOINLINE isRealInteraction #-}

newtype Operator = Operator (ForeignPtr ())

foreign import ccall unsafe "ls_create_operator"
  ls_create_operator :: Ptr (Ptr ()) -> Ptr () -> CUInt -> Ptr (Ptr ()) -> IO CInt

foreign import ccall unsafe "&ls_destroy_operator"
  ls_destroy_operator :: FunPtr (Ptr () -> IO ())

withInteractions :: [Interaction] -> (Int -> Ptr (Ptr ()) -> IO a) -> IO a
withInteractions xs func = withManyForeignPtr pointers func
  where
    pointers = (\(Interaction p) -> p) <$> xs

mkOperator :: (MonadIO m, MonadThrow m) => SpinBasis -> [Interaction] -> m Operator
mkOperator (SpinBasis basis) terms = do
  (code, ptr) <- liftIO $
    alloca $ \ptrPtr -> do
      c <- withInteractions terms $ \n interactionsPtr ->
        withForeignPtr basis $ \basisPtr ->
          ls_create_operator ptrPtr basisPtr (fromIntegral n) interactionsPtr
      if c == 0
        then (,) <$> pure c <*> peek ptrPtr
        else pure (c, nullPtr)
  checkStatus code
  fmap Operator . liftIO $ newForeignPtr ls_destroy_operator ptr
