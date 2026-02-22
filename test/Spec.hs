module Main (main) where

import Ray
import Test.Hspec
import Vec3

main :: IO ()
main = hspec $ do
  describe "vAdd" $ do
    it "adds two vectors" $ do
      vAdd (Vec3 1 2 3) (Vec3 4 5 6) `shouldBe` Vec3 5 7 9

    it "adds with zero vector" $ do
      vAdd (Vec3 1 2 3) (Vec3 0 0 0) `shouldBe` Vec3 1 2 3

  describe "vSub" $ do
    it "subtracts two vectors" $ do
      vSub (Vec3 4 5 6) (Vec3 1 2 3) `shouldBe` Vec3 3 3 3

    it "subtracts with zero vector" $ do
      vSub (Vec3 1 2 3) (Vec3 0 0 0) `shouldBe` Vec3 1 2 3

  describe "vMul" $ do
    it "multiplies two vectors element-wise" $ do
      vMul (Vec3 2 3 4) (Vec3 5 6 7) `shouldBe` Vec3 10 18 28

  describe "vScale" $ do
    it "scales a vector" $ do
      vScale 2 (Vec3 1 2 3) `shouldBe` Vec3 2 4 6

  describe "vDiv" $ do
    it "divides a vector by scalar" $ do
      vDiv 2 (Vec3 2 4 6) `shouldBe` Vec3 1 2 3

  describe "dot" $ do
    it "computes dot product" $ do
      dot (Vec3 1 2 3) (Vec3 4 5 6) `shouldBe` 32

  describe "cross" $ do
    it "computes cross product" $ do
      cross (Vec3 1 0 0) (Vec3 0 1 0) `shouldBe` Vec3 0 0 1

  describe "vLength" $ do
    it "computes length" $ do
      vLength (Vec3 3 4 0) `shouldBe` 5

  describe "vNegate" $ do
    it "negates a vector" $ do
      vNegate (Vec3 1 (-2) 3) `shouldBe` Vec3 (-1) 2 (-3)

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
