module Main (main) where

import CameraSpec qualified
import ColorSpec qualified
import IntervalSpec qualified
import Ray
import SphereSpec qualified
import Test.Hspec
import Vec3 (Vec3 (..))
import Vec3 qualified as V

main :: IO ()
main = hspec $ do
  SphereSpec.spec
  IntervalSpec.spec
  ColorSpec.spec
  CameraSpec.spec

  describe "V.add" $ do
    it "adds two vectors" $ do
      V.add (Vec3 1 2 3) (Vec3 4 5 6) `shouldBe` Vec3 5 7 9

    it "adds with zero vector" $ do
      V.add (Vec3 1 2 3) (Vec3 0 0 0) `shouldBe` Vec3 1 2 3

  describe "V.sub" $ do
    it "subtracts two vectors" $ do
      V.sub (Vec3 4 5 6) (Vec3 1 2 3) `shouldBe` Vec3 3 3 3

    it "subtracts with zero vector" $ do
      V.sub (Vec3 1 2 3) (Vec3 0 0 0) `shouldBe` Vec3 1 2 3

  describe "V.mul" $ do
    it "multiplies two vectors element-wise" $ do
      V.mul (Vec3 2 3 4) (Vec3 5 6 7) `shouldBe` Vec3 10 18 28

  describe "V.scale" $ do
    it "scales a vector" $ do
      V.scale 2 (Vec3 1 2 3) `shouldBe` Vec3 2 4 6

  describe "V.div" $ do
    it "divides a vector by scalar" $ do
      V.div 2 (Vec3 2 4 6) `shouldBe` Vec3 1 2 3

  describe "V.dot" $ do
    it "computes dot product" $ do
      V.dot (Vec3 1 2 3) (Vec3 4 5 6) `shouldBe` 32

  describe "V.cross" $ do
    it "computes cross product" $ do
      V.cross (Vec3 1 0 0) (Vec3 0 1 0) `shouldBe` Vec3 0 0 1

  describe "V.length" $ do
    it "computes length" $ do
      V.length (Vec3 3 4 0) `shouldBe` 5

  describe "V.negate" $ do
    it "negates a vector" $ do
      V.negate (Vec3 1 (-2) 3) `shouldBe` Vec3 (-1) 2 (-3)

  describe "V.reflect" $ do
    it "reflects off a horizontal surface" $
      V.reflect (Vec3 1 (-1) 0) (Vec3 0 1 0) `shouldBe` Vec3 1 1 0

    it "reflects straight back on head-on" $
      V.reflect (Vec3 0 0 (-1)) (Vec3 0 0 1) `shouldBe` Vec3 0 0 1

  describe "V.isNearZero" $ do
    it "returns True for near-zero vector" $
      V.isNearZero (Vec3 1e-9 1e-9 1e-9) `shouldBe` True

    it "returns False for non-zero vector" $
      V.isNearZero (Vec3 0.1 0 0) `shouldBe` False

  describe "rayAt" $ do
    it "computes point at t=0" $ do
      let r = Ray (Vec3 0 0 0) (Vec3 1 2 3)
      rayAt r 0 `shouldBe` Vec3 0 0 0

    it "computes point at t=1" $ do
      let r = Ray (Vec3 1 0 0) (Vec3 0 1 0)
      rayAt r 1 `shouldBe` Vec3 1 1 0

    it "computes point at t=2" $ do
      let r = Ray (Vec3 1 2 3) (Vec3 1 1 1)
      rayAt r 2 `shouldBe` Vec3 3 4 5
