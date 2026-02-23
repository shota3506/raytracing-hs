{-# LANGUAGE LambdaCase #-}

module SphereSpec (spec) where

import Ray
import Shape
import Sphere
import Test.Hspec
import Vec3

spec :: Spec
spec = describe "Sphere" $ do
  let sphere = toShape (Sphere (Vec3 0 0 (-1)) 0.5)

  it "hits sphere at front" $ do
    let r = Ray (Vec3 0 0 0) (Vec3 0 0 (-1))
        result = hit sphere r 0.001 1000
    result `shouldSatisfy` \case
      Just i -> abs (t i - 0.5) < 1e-9
      Nothing -> False

  it "misses sphere" $ do
    let r = Ray (Vec3 0 0 0) (Vec3 0 1 0)
    hit sphere r 0.001 1000 `shouldBe` Nothing

  it "ignores hits outside t range" $ do
    let r = Ray (Vec3 0 0 0) (Vec3 0 0 (-1))
    hit sphere r 0.001 0.1 `shouldBe` Nothing

  it "returns correct normal at hit point" $ do
    let r = Ray (Vec3 0 0 0) (Vec3 0 0 (-1))
        result = hit sphere r 0.001 1000
    result `shouldSatisfy` \case
      Just i -> vLength (normal i `vSub` Vec3 0 0 1) < 1e-9
      Nothing -> False
