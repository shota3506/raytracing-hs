module AABBSpec (spec) where

import AABB
import Interval
import Ray
import Test.Hspec
import Vec3 (Vec3 (..))

spec :: Spec
spec = describe "AABB" $ do
  let box = AABB (Interval 1 3) (Interval 1 3) (Interval 1 3)

  describe "intersects" $ do
    it "returns True when ray hits the box" $
      let r = Ray (Vec3 0 2 2) (Vec3 1 0 0) 0
       in intersects box r (Interval 0.001 1000) `shouldBe` True

    it "returns False when ray misses the box" $
      let r = Ray (Vec3 0 0 0) (Vec3 0 1 0) 0
       in intersects box r (Interval 0.001 1000) `shouldBe` False
