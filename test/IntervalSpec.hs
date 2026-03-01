module IntervalSpec (spec) where

import Interval
import Test.Hspec

spec :: Spec
spec = describe "Interval" $ do
  describe "clamp" $ do
    it "clamps below min" $
      clamp (Interval 0 1) (-0.5) `shouldBe` 0

    it "clamps above max" $
      clamp (Interval 0 1) 1.5 `shouldBe` 1

    it "keeps value in range" $
      clamp (Interval 0 1) 0.5 `shouldBe` 0.5

  describe "contains" $ do
    it "returns True for value inside" $
      contains (Interval 0 1) 0.5 `shouldBe` True

    it "returns True on boundary" $
      contains (Interval 0 1) 0 `shouldBe` True

  describe "surrounds" $ do
    it "returns True for value strictly inside" $
      surrounds (Interval 0 1) 0.5 `shouldBe` True

    it "returns False on boundary" $
      surrounds (Interval 0 1) 0 `shouldBe` False
