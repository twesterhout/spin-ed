module Main (main) where

import Data.Complex
import Data.Yaml
import SpinED
import Test.Hspec

anySpinEDException :: Selector SpinEDException
anySpinEDException = const True

anyLatticeSymmetriesException :: Selector LatticeSymmetriesException
anyLatticeSymmetriesException = const True

parseLines :: FromJSON a => [Text] -> Either ParseException a
parseLines xs = decodeEither' . encodeUtf8 . unlines $ xs

main :: IO ()
main = hspec $ do
  describe "SymmetrySpec" $ do
    it "parses JSON specifications" $ do
      let s₁ =
            parseLines @SymmetrySpec
              [ "permutation: [3, 1, 2, 4]",
                "flip: true",
                "sector: 8"
              ]
          s₂ =
            parseLines @SymmetrySpec
              [ "permutation: [1, 1, 2, 4]",
                "sector: -5"
              ]
      case s₁ of
        Left _ -> s₁ `shouldSatisfy` isRight
        Right x -> x `shouldBe` SymmetrySpec [3, 1, 2, 4] True 8
      case s₂ of
        Left _ -> s₂ `shouldSatisfy` isRight
        Right x -> x `shouldBe` SymmetrySpec [1, 1, 2, 4] False (-5)
    it "converts SymmetrySpec to Symmetry" $ do
      s₁ <- toSymmetry $ SymmetrySpec [3, 2, 1, 0] False 0
      getSector s₁ `shouldBe` 0
      getPeriodicity s₁ `shouldBe` 2
      toSymmetry (SymmetrySpec [4, 3, 4, 1] False 0) `shouldThrow` anyLatticeSymmetriesException
      toSymmetry (SymmetrySpec [4, 3, 2, 1, 0] False 3) `shouldThrow` anyLatticeSymmetriesException

  describe "BasisSpec" $ do
    it "parses JSON specifications" $ do
      let s₁ =
            parseLines @BasisSpec $
              [ "number_spins: 4",
                "hamming_weight: 2",
                "symmetries:",
                "  - permutation: [1, 2, 3, 0]",
                "    sector: 0",
                "  - permutation: [3, 2, 1, 0]",
                "    sector: 0"
              ]
          expected₁ = BasisSpec 4 (Just 2) [SymmetrySpec [1, 2, 3, 0] False 0, SymmetrySpec [3, 2, 1, 0] False 0]
      case s₁ of
        Left _ -> s₁ `shouldSatisfy` isRight
        Right x -> x `shouldBe` expected₁

      let s₂ =
            parseLines @BasisSpec $
              [ "number_spins: 100",
                "hamming_weight: null",
                "symmetries: []"
              ]
          expected₂ = BasisSpec 100 Nothing []
      case s₂ of
        Left _ -> s₂ `shouldSatisfy` isRight
        Right x -> x `shouldBe` expected₂
    it "creates SymmetryGroup" $ do
      s₁ <- toSymmetry $ SymmetrySpec [3, 2, 1, 0] False 0
      s₂ <- toSymmetry $ SymmetrySpec [1, 2, 3, 0] False 0
      s₂' <- toSymmetry $ SymmetrySpec [1, 2, 3, 0] False 1
      s₃ <- toSymmetry $ SymmetrySpec [0, 1, 2, 3] True 0
      g <- mkGroup [s₁, s₂, s₃]
      getGroupSize g `shouldBe` 16
      mkGroup [s₁, s₂', s₃] `shouldThrow` anyLatticeSymmetriesException
    it "creates SpinBasis" $ do
      let s₁ = BasisSpec 4 (Just 2) [SymmetrySpec [1, 2, 3, 0] False 0, SymmetrySpec [3, 2, 1, 0] False 0]
      _ <- toBasis s₁
      let s₂ = BasisSpec (-2) Nothing [SymmetrySpec [1, 2, 3, 0] False 0]
      toBasis s₂ `shouldThrow` anySpinEDException

  describe "InteractionSpec" $ do
    it "parses JSON specifications" $ do
      let (s₁ :: Either ParseException InteractionSpec) =
            decodeEither' . encodeUtf8 . unlines $
              [ "matrix: [[1, 2,  1],",
                "         [4, 2,  1],",
                "         [1, -2, 3]]",
                "sites: [[0, 0, 0], [1, 1, 1]]"
              ]
      s₁ `shouldSatisfy` isRight
    it "creates Interaction from InteractionSpec" $ do
      let matrix =
            [ [1.0 :+ 0.0, 0.0 :+ 0.0],
              [0.0 :+ 0.0, (-1.0) :+ 0.0]
            ]
          sites = [[0], [2]]
          spec = InteractionSpec matrix sites
      x <- toInteraction spec
      isRealInteraction x `shouldBe` True
