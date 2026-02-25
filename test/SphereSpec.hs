{-# LANGUAGE LambdaCase #-}

module SphereSpec (spec) where

import Intersection
import Interval
import Lambertian
import Ray
import Shape
import Sphere
import Test.Hspec
import Vec3 (Vec3 (..))
import Vec3 qualified as V

spec :: Spec
spec = describe "Sphere" $ do
  let mat = mkLambertian (Vec3 0.5 0.5 0.5)
      sphere = toShape (Sphere (Vec3 0 0 (-1)) 0.5 mat)

  it "hits sphere at front" $ do
    let r = Ray (Vec3 0 0 0) (Vec3 0 0 (-1))
        result = fmap fst (hit sphere r (Interval 0.001 1000))
    result `shouldSatisfy` \case
      Just i -> abs (t i - 0.5) < 1e-9
      Nothing -> False

  it "misses sphere" $ do
    let r = Ray (Vec3 0 0 0) (Vec3 0 1 0)
    fmap fst (hit sphere r (Interval 0.001 1000)) `shouldBe` Nothing

  it "ignores hits outside t range" $ do
    let r = Ray (Vec3 0 0 0) (Vec3 0 0 (-1))
    fmap fst (hit sphere r (Interval 0.001 0.1)) `shouldBe` Nothing

  it "returns correct normal at hit point" $ do
    let r = Ray (Vec3 0 0 0) (Vec3 0 0 (-1))
        result = fmap fst (hit sphere r (Interval 0.001 1000))
    result `shouldSatisfy` \case
      Just i -> V.length (V.sub (normal i) (Vec3 0 0 1)) < 1e-9
      Nothing -> False
