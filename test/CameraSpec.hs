module CameraSpec (spec) where

import Camera
import Test.Hspec
import Vec3 (Vec3 (..))

defaultConfig :: CameraConfig
defaultConfig =
  CameraConfig
    { aspectRatio = 2.0,
      imageWidth = 200,
      samplesPerPixel = 1,
      maxDepth = 10,
      vFov = 90,
      lookFrom = Vec3 0 0 0,
      lookAt = Vec3 0 0 (-1),
      vUp = Vec3 0 1 0,
      defocusAngle = 0,
      focusDist = 1
    }

spec :: Spec
spec = describe "mkCamera" $ do
  it "computes imageHeight from aspect ratio" $
    imageHeight (mkCamera defaultConfig) `shouldBe` 100

  it "imageHeight is at least 1" $
    imageHeight (mkCamera defaultConfig {aspectRatio = 1000.0, imageWidth = 1}) `shouldBe` 1
