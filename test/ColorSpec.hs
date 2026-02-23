module ColorSpec (spec) where

import Color
import Test.Hspec
import Vec3 (Vec3 (..))

spec :: Spec
spec = do
  describe "linearToGamma" $ do
    it "returns sqrt for positive values" $
      linearToGamma 0.25 `shouldBe` 0.5

    it "returns 0 for zero" $
      linearToGamma 0 `shouldBe` 0

    it "returns 0 for negative values" $
      linearToGamma (-1.0) `shouldBe` 0

  describe "formatColor" $ do
    it "formats normal color with gamma correction" $
      formatColor (Vec3 1 0 0.5) `shouldBe` "255 0 181"

    it "clamps values over 1" $
      formatColor (Vec3 1.5 0 0) `shouldBe` "255 0 0"

    it "clamps negative values" $
      formatColor (Vec3 (-0.5) 0 0) `shouldBe` "0 0 0"
