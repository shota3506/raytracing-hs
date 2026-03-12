{-# LANGUAGE LambdaCase #-}

module BVHSpec (spec) where

import BVH
import Data.Maybe (isNothing)
import Intersection
import Interval
import Lambertian
import Ray
import Shape
import Sphere
import Test.Hspec
import Vec3 (Vec3 (..))

spec :: Spec
spec = describe "BVH" $ do
  describe "buildBVH" $ do
    it "returns Nothing for empty BVH" $
      let bvh = buildBVH []
          r = Ray (Vec3 0 0 0) (Vec3 0 0 (-1)) 0
       in isNothing (hit bvh r (Interval 0.001 1000)) `shouldBe` True

    it "finds closest hit among multiple shapes" $ do
      let mat = mkLambertian (Vec3 0.5 0.5 0.5)
          s1 = toShape (Sphere (Vec3 0 0 (-2)) (Vec3 0 0 0) 0.5 mat)
          s2 = toShape (Sphere (Vec3 0 0 (-4)) (Vec3 0 0 0) 0.5 mat)
          bvh = buildBVH [s1, s2]
          r = Ray (Vec3 0 0 0) (Vec3 0 0 (-1)) 0
          result = fmap fst (hit bvh r (Interval 0.001 1000))
      result `shouldSatisfy` \case
        Just isec -> abs (t isec - 1.5) < 1e-9
        Nothing -> False
