module Main (main) where

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
