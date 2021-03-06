-- |
-- Copyright: (c) 2020 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
module SpinED.Internal where

import Control.Monad.ST (RealWorld)
import Data.Complex
import Data.Vector.Storable (MVector, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr
import Foreign.Storable (Storable (..))
import GHC.ForeignPtr (newConcForeignPtr)
import Numeric.PRIMME
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (bracket, impureThrow, throwIO)
import Prelude hiding (group)

-- #include <lattice_symmetries/lattice_symmetries.h>

-- | Exceptions thrown when an error occurs in @liblattice_symmetries@.
data LatticeSymmetriesException = LatticeSymmetriesException {eCode :: Int, eMessage :: Text}
  deriving stock (Show)

instance Exception LatticeSymmetriesException

data SpinEDException = SpinEDException Text
  deriving stock (Show)

instance Exception SpinEDException

foreign import ccall unsafe "ls_error_to_string" ls_error_to_string :: CInt -> IO CString

foreign import ccall unsafe "ls_destroy_string" ls_destroy_string :: CString -> IO ()

foreign import ccall unsafe "ls_enable_logging" ls_enable_logging :: IO ()

foreign import ccall unsafe "ls_disable_logging" ls_disable_logging :: IO ()

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
checkStatus :: (MonadIO m, Integral a) => a -> m ()
checkStatus c
  | c == 0 = return ()
  | otherwise = do
    print =<< liftIO (getErrorMessage c')
    throwIO . LatticeSymmetriesException c' =<< liftIO (getErrorMessage c')
  where
    c' = fromIntegral c

newtype Symmetry = Symmetry (ForeignPtr ())

foreign import ccall unsafe "ls_create_symmetry"
  ls_create_symmetry :: Ptr (Ptr ()) -> CUInt -> Ptr CUInt -> CUInt -> IO CInt

foreign import ccall unsafe "&ls_destroy_symmetry"
  ls_destroy_symmetry :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "ls_get_sector" ls_get_sector :: Ptr () -> IO CUInt

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

mkObject :: (Ptr (Ptr ()) -> IO CInt) -> IO (Ptr ())
mkObject f = alloca $ \ptrPtr -> f ptrPtr >>= checkStatus >> peek ptrPtr

mkSymmetry ::
  MonadIO m =>
  -- | Permutation
  [Int] ->
  -- | Symmetry sector
  Int ->
  m Symmetry
mkSymmetry !permutation !sector = do
  -- Make sure permutation and sector can be safely converted to unsigned
  -- representations. Everything else is checked by ls_create_symmetry
  when (any (< 0) permutation) . throwIO . SpinEDException $
    "invalid permutation: " <> show permutation <> "; indices must be non-negative"
  when (sector < 0) . throwIO . SpinEDException $
    "invalid sector: " <> show sector <> "; expected a non-negative number"
  ptr <- liftIO . mkObject $ \ptrPtr ->
    withArrayLen (fromIntegral <$> permutation) $ \n permutationPtr ->
      ls_create_symmetry ptrPtr (fromIntegral n) permutationPtr (fromIntegral sector)
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

mkGroup ::
  MonadIO m =>
  -- | Symmetry generators
  [Symmetry] ->
  m SymmetryGroup
mkGroup !xs = do
  ptr <- liftIO . mkObject $ \ptrPtr ->
    withSymmetries xs $ \n xsPtr ->
      ls_create_group ptrPtr (fromIntegral n) xsPtr
  fmap SymmetryGroup . liftIO $ newForeignPtr ls_destroy_group ptr

getGroupSize :: SymmetryGroup -> Int
getGroupSize (SymmetryGroup p) = unsafePerformIO $! withForeignPtr p $ \p' ->
  fromIntegral <$> ls_get_group_size p'
{-# NOINLINE getGroupSize #-}

data SpinBasisType = Thin | Full

data SpinBasis' (t :: SpinBasisType) where
  ThinSpinBasis :: ForeignPtr () -> SpinBasis' 'Thin
  FullSpinBasis :: ForeignPtr () -> SpinBasis' 'Full

getBasisPtr :: SpinBasis' t -> ForeignPtr ()
getBasisPtr (ThinSpinBasis p) = p
getBasisPtr (FullSpinBasis p) = p

withBasis :: SpinBasis' t -> (Ptr () -> IO a) -> IO a
withBasis x = withForeignPtr (getBasisPtr x)

newtype SpinBasis = SpinBasis (ForeignPtr ())

foreign import ccall unsafe "ls_create_spin_basis"
  ls_create_spin_basis :: Ptr (Ptr ()) -> Ptr () -> CUInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "&ls_destroy_spin_basis"
  ls_destroy_spin_basis :: FunPtr (Ptr () -> IO ())

foreign import ccall safe "ls_build"
  ls_build :: Ptr () -> IO CInt

foreign import ccall safe "ls_build_unsafe"
  ls_build_unsafe :: Ptr () -> Word64 -> Ptr Word64 -> IO CInt

foreign import ccall unsafe "ls_get_number_states"
  ls_get_number_states :: Ptr () -> Ptr Word64 -> IO CInt

foreign import ccall unsafe "ls_get_states"
  ls_get_states :: Ptr (Ptr ()) -> Ptr () -> IO CInt

foreign import ccall unsafe "ls_states_get_data"
  ls_states_get_data :: Ptr () -> Ptr Word64

foreign import ccall unsafe "ls_states_get_size"
  ls_states_get_size :: Ptr () -> Word64

foreign import ccall unsafe "ls_destroy_states"
  ls_destroy_states :: Ptr () -> IO ()

mkBasis ::
  MonadIO m =>
  -- | Symmetry group
  SymmetryGroup ->
  -- | Number of spins
  Int ->
  -- | Hamming weight
  Maybe Int ->
  -- | Spin inversion
  Maybe Int ->
  m SpinBasis
mkBasis !(SymmetryGroup group) !numberSpins !hammingWeight !spinInversion = do
  when (numberSpins <= 0) . throwIO . SpinEDException $
    "invalid number of spins: " <> show numberSpins <> "; expected a positive number"
  hammingWeight' <- case hammingWeight of
    Just x -> do
      when (x < 0) . throwIO . SpinEDException $
        "invalid Hamming weight: " <> show x <> "; expected a non-negative number"
      return $ fromIntegral x
    Nothing -> return (-1)
  spinInversion' <- case spinInversion of
    Just x -> do
      when (x /= 1 && x /= -1) . throwIO . SpinEDException $
        "invalid value for spin inversion: " <> show x <> "; expected either -1 or +1"
      return $ fromIntegral x
    Nothing -> return 0
  ptr <- liftIO . mkObject $ \ptrPtr ->
    withForeignPtr group $ \groupPtr ->
      ls_create_spin_basis ptrPtr groupPtr (fromIntegral numberSpins) hammingWeight' spinInversion'
  fmap SpinBasis . liftIO $ newForeignPtr ls_destroy_spin_basis ptr

buildBasis :: MonadIO m => SpinBasis -> Maybe (Vector Word64) -> m ()
buildBasis (SpinBasis basis) representatives = do
  status <- liftIO $ case representatives of
    (Just v) -> withForeignPtr basis $ \basisPtr -> V.unsafeWith v $ \vPtr ->
      ls_build_unsafe basisPtr (fromIntegral $ V.length v) vPtr
    Nothing -> withForeignPtr basis ls_build
  checkStatus status

basisGetStates :: MonadIO m => SpinBasis -> m (Vector Word64)
basisGetStates (SpinBasis basis) = liftIO $
  withForeignPtr basis $ \basisPtr -> do
    rawPtr <- mkObject $ \ptrPtr -> ls_get_states ptrPtr basisPtr
    V.unsafeFreeze =<< MV.unsafeFromForeignPtr0
      <$> newConcForeignPtr (ls_states_get_data rawPtr) (ls_destroy_states rawPtr)
      <*> pure (fromIntegral . ls_states_get_size $ rawPtr)

-- Unsafe for now, but fixable with SpinBasis' GADT later on
getNumberStates :: MonadIO m => SpinBasis -> m Int
getNumberStates (SpinBasis p) = do
  count <- liftIO $
    withForeignPtr p $ \p' ->
      alloca $ \numberStates' -> do
        checkStatus =<< ls_get_number_states p' numberStates'
        peek numberStates'
  return (fromIntegral count)

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
  (Monad m, Show r, Real r) =>
  -- | Expected dimension @n@ of the matrix
  Int ->
  -- | Square @matrix@ of dimension @n@
  [[Complex r]] ->
  -- | Row-major representation of @matrix@
  m (Vector (Complex Double))
toMatrix !dim !rows = do
  when (length rows /= dim) . impureThrow . SpinEDException $
    "invalid matrix: " <> show rows <> "; expected a square matrix of dimension " <> show dim
  when (any ((/= dim) . length) rows) . impureThrow . SpinEDException $
    "invalid matrix: " <> show rows <> "; expected a square matrix of dimension " <> show dim
  return . V.fromList . fmap (fmap (fromRational . toRational)) . concat $ rows

class MakeInteraction a where
  mkInteraction' :: (MonadIO m, Show r, Real r) => [[Complex r]] -> [a] -> m Interaction

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
  mkInteraction' _ [] =
    throwIO . SpinEDException $
      "zero-point interactions (i.e. constant factors) are not supported"
  mkInteraction' matrix rows@(r : _) = case n of
    1 -> mkInteraction' matrix =<< mapM match1 rows
    2 -> mkInteraction' matrix =<< mapM match2 rows
    3 -> mkInteraction' matrix =<< mapM match3 rows
    4 -> mkInteraction' matrix =<< mapM match4 rows
    _ ->
      throwIO . SpinEDException $
        "currently only 1-, 2-, 3-, and 4-point interactions are supported, but received n=" <> show n
    where
      n = length r
      match1 [x₁] = return x₁
      match1 _ = throwIO failure
      match2 [x₁, x₂] = return (x₁, x₂)
      match2 _ = throwIO failure
      match3 [x₁, x₂, x₃] = return (x₁, x₂, x₃)
      match3 _ = throwIO failure
      match4 [x₁, x₂, x₃, x₄] = return (x₁, x₂, x₃, x₄)
      match4 _ = throwIO failure
      failure =
        SpinEDException $
          "invalid sites: " <> show rows <> "; expected an array of length-" <> show n <> " tuples"

unsafeMkInteraction :: MonadIO m => CreateInteraction -> Int -> Vector (Complex Double) -> Vector Int -> m Interaction
unsafeMkInteraction ffiCreate numberSites matrix sites = do
  when (V.any (< 0) sites) . throwIO . SpinEDException $ "site indices must be all non-negative numbers"
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

newtype Operator' = Operator' (ForeignPtr ())

foreign import ccall unsafe "ls_create_operator"
  ls_create_operator :: Ptr (Ptr ()) -> Ptr () -> CUInt -> Ptr (Ptr ()) -> IO CInt

foreign import ccall unsafe "&ls_destroy_operator"
  ls_destroy_operator :: FunPtr (Ptr () -> IO ())

foreign import ccall safe "ls_operator_matmat"
  ls_operator_matmat :: Ptr () -> CInt -> Word64 -> Word64 -> Ptr () -> Word64 -> Ptr () -> Word64 -> IO CInt

foreign import ccall safe "ls_operator_expectation"
  ls_operator_expectation :: Ptr () -> CInt -> Word64 -> Word64 -> Ptr () -> Word64 -> Ptr (Complex Double) -> IO CInt

foreign import ccall unsafe "ls_operator_is_real"
  ls_operator_is_real :: Ptr () -> IO CBool

withInteractions :: [Interaction] -> (Int -> Ptr (Ptr ()) -> IO a) -> IO a
withInteractions xs func = withManyForeignPtr pointers func
  where
    pointers = (\(Interaction p) -> p) <$> xs

mkOperator :: MonadIO m => SpinBasis -> [Interaction] -> m Operator'
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
  fmap Operator' . liftIO $ newForeignPtr ls_destroy_operator ptr

toCdatatype :: BlasDatatypeTag t -> CInt
toCdatatype x = case x of
  FloatTag -> 0
  DoubleTag -> 1
  ComplexFloatTag -> 2
  ComplexDoubleTag -> 3

inplaceApply :: forall a. BlasDatatype a => Operator' -> Block a -> MBlock RealWorld a -> IO ()
inplaceApply (Operator' op) (Block (size, blockSize) xStride x) (MBlock (size', blockSize') yStride y)
  | size /= size' || blockSize /= blockSize' =
    throwIO . SpinEDException $
      "dimensions of x and y do not match: " <> show (size, blockSize) <> " != " <> show (size', blockSize')
  | otherwise =
    withForeignPtr op $ \opPtr ->
      V.unsafeWith x $ \xPtr ->
        MV.unsafeWith y $ \yPtr ->
          checkStatus
            =<< ls_operator_matmat
              opPtr
              (toCdatatype . blasTag $ Proxy @a)
              (fromIntegral size)
              (fromIntegral blockSize)
              (castPtr xPtr)
              (fromIntegral xStride)
              (castPtr yPtr)
              (fromIntegral yStride)

apply :: forall a. BlasDatatype a => Operator' -> Block a -> IO (Block a)
apply operator x@(Block dims@(rows, cols) _ _) = do
  (y :: MVector RealWorld a) <- MV.new (rows * cols)
  inplaceApply operator x $ MBlock dims rows y
  Block dims rows <$> V.unsafeFreeze y

expectation :: forall a. BlasDatatype a => Operator' -> Block a -> IO (Vector (Complex Double))
expectation (Operator' op) (Block (size, blockSize) xStride x) = do
  (out :: MVector RealWorld (Complex Double)) <- MV.new blockSize
  withForeignPtr op $ \opPtr ->
    V.unsafeWith x $ \xPtr ->
      MV.unsafeWith out $ \outPtr ->
        checkStatus
          =<< ls_operator_expectation
            opPtr
            (toCdatatype . blasTag $ Proxy @a)
            (fromIntegral size)
            (fromIntegral blockSize)
            (castPtr xPtr)
            (fromIntegral xStride)
            outPtr
  V.unsafeFreeze out

isOperatorReal' :: Operator' -> IO Bool
isOperatorReal' (Operator' op) = withForeignPtr op (ls_operator_is_real >=> return . toBool)

fromOperator' :: Operator' -> PrimmeOperator Double
fromOperator' = inplaceApply
