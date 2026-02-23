module ColorSpec (spec) where

import Color
import Test.Hspec
import Vec3 (Vec3 (..))

spec :: Spec
spec = describe "formatColor" $ do
  it "formats normal color" $
    formatColor (Vec3 1 0 0.5) `shouldBe` "255 0 127"

  it "clamps values over 1" $
    formatColor (Vec3 1.5 0 0) `shouldBe` "255 0 0"

  it "clamps negative values" $
    formatColor (Vec3 (-0.5) 0 0) `shouldBe` "0 0 0"
