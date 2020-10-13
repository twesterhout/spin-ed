module Main (main) where

import Data.Complex
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
  describe "instance fromJSON BasisSpec" $ do
    it "parses JSON specifications" $ do
      let (s₁ :: Either ParseException BasisSpec) =
            decodeEither' . encodeUtf8 . unlines $
              [ "number_spins: 4",
                "hamming_weight: 2",
                "symmetries:",
                "  - permutation: [2, 3, 4, 1]",
                "    sector: 0",
                "  - permutation: [4, 3, 2, 1]",
                "    sector: 0"
              ]
      s₁ `shouldSatisfy` isRight
  describe "mkGroup" $ do
    it "creates SymmetryGroup" $ do
      s₁ <- toSymmetry $ SymmetrySpec [4, 3, 2, 1] False 0
      s₂ <- toSymmetry $ SymmetrySpec [2, 3, 4, 1] False 0
      s₃ <- toSymmetry $ SymmetrySpec [1, 2, 3, 4] True 0
      g <- mkGroup [s₁, s₂, s₃]
      getGroupSize g `shouldBe` 16
  describe "instance fromJSON InteractionSpec" $ do
    it "parses JSON specifications" $ do
      let (s₁ :: Either ParseException InteractionSpec) =
            decodeEither' . encodeUtf8 . unlines $
              [ "matrix: [[1, 2,  1],",
                "         [4, 2,  1],",
                "         [1, -2, 3]]",
                "sites: [[0, 0, 0], [1, 1, 1]]"
              ]
      s₁ `shouldSatisfy` isRight
      print s₁
  describe "toInteraction" $ do
    it "creates Interaction from InteractionSpec" $ do
      let matrix =
            [ [1.0 :+ 0.0, 0.0 :+ 0.0],
              [0.0 :+ 0.0, (-1.0) :+ 0.0]
            ]
          sites = [[0], [2]]
          spec = InteractionSpec matrix sites
      x <- toInteraction spec
      isRealInteraction x `shouldBe` True
