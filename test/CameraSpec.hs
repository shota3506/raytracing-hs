module CameraSpec (spec) where

import Camera
import Test.Hspec

spec :: Spec
spec = describe "mkCamera" $ do
  it "computes imageHeight from aspect ratio" $
    imageHeight (mkCamera (CameraConfig 2.0 200 1 10)) `shouldBe` 100

  it "imageHeight is at least 1" $
    imageHeight (mkCamera (CameraConfig 1000.0 1 1 10)) `shouldBe` 1
