module Main (main) where

import Data.Yaml
import SpinED
import Test.Hspec

anySpinEDException :: Selector SpinEDException
anySpinEDException = const True

anyLatticeSymmetriesException :: Selector LatticeSymmetriesException
anyLatticeSymmetriesException = const True

main :: IO ()
main = hspec $ do
  describe "instance fromJSON SymmetrySpec" $ do
    it "parses JSON specifications" $ do
      let (s :: Either ParseException SymmetrySpec) =
            decodeEither' . encodeUtf8 . unlines $
              [ "permutation: [3, 1, 2, 4]",
                "flip: true",
                "sector: 8"
              ]
      case s of
        Left _ -> s `shouldSatisfy` isRight
        Right x -> x `shouldBe` SymmetrySpec [3, 1, 2, 4] True 8
      let (s₂ :: Either ParseException SymmetrySpec) =
            decodeEither' . encodeUtf8 . unlines $
              [ "permutation: [1, 1, 2, 4]",
                "sector: -5"
              ]
      case s₂ of
        Left _ -> s₂ `shouldSatisfy` isRight
        Right x -> x `shouldBe` SymmetrySpec [1, 1, 2, 4] False (-5)
  describe "toSymmetry" $ do
    it "converts SymmetrySpec to Symmetry" $ do
      s₁ <- toSymmetry $ SymmetrySpec [4, 3, 2, 1] False 0
      getSector s₁ `shouldBe` 0
      getPeriodicity s₁ `shouldBe` 2
      toSymmetry (SymmetrySpec [4, 3, 4, 1] False 0) `shouldThrow` anySpinEDException
      toSymmetry (SymmetrySpec [5, 4, 3, 2, 1] False 3) `shouldThrow` anyLatticeSymmetriesException
