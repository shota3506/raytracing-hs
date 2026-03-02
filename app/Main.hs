module Main (main) where

import Camera
import Color
import Control.Monad (forM_)
import Dielectric
import Lambertian
import Metal
import Scene
import Shape (Shape)
import Sphere
import System.IO (hPutStrLn, stderr)
import System.Random (StdGen, mkStdGen, uniformR)
import Vec3 (Vec3 (..))
import Vec3 qualified as V

randomScene :: StdGen -> ([Shape], StdGen)
randomScene gen =
  let groundMaterial = mkLambertian (Vec3 0.5 0.5 0.5)
      ground = toShape (Sphere (Vec3 0 (-1000) 0) 1000 groundMaterial)
      (smallSpheres, gen1) = generateSmallSpheres gen
      material1 = mkDielectric 1.5
      material2 = mkLambertian (Vec3 0.4 0.2 0.1)
      material3 = mkMetal (Vec3 0.7 0.6 0.5) 0.0
      bigSpheres =
        [ toShape (Sphere (Vec3 0 1 0) 1.0 material1),
          toShape (Sphere (Vec3 (-4) 1 0) 1.0 material2),
          toShape (Sphere (Vec3 4 1 0) 1.0 material3)
        ]
   in (ground : smallSpheres ++ bigSpheres, gen1)

generateSmallSpheres :: StdGen -> ([Shape], StdGen)
generateSmallSpheres gen = foldr go ([], gen) [(a, b) | a <- [-11 .. 10 :: Int], b <- [-11 .. 10 :: Int]]
  where
    go (a, b) (spheres, g) =
      let (chooseMat, g1) = uniformR (0.0 :: Double, 1.0) g
          (rx, g2) = uniformR (0.0 :: Double, 1.0) g1
          (rz, g3) = uniformR (0.0 :: Double, 1.0) g2
          center = Vec3 (fromIntegral a + 0.9 * rx) 0.2 (fromIntegral b + 0.9 * rz)
       in if V.length (V.sub center (Vec3 4 0.2 0)) > 0.9
            then
              let (sphere, g4) = makeSphere chooseMat center g3
               in (sphere : spheres, g4)
            else (spheres, g3)
    makeSphere chooseMat center g
      | chooseMat < 0.8 =
          let (r1, g1') = uniformR (0.0 :: Double, 1.0) g
              (g1, g2') = uniformR (0.0 :: Double, 1.0) g1'
              (b1, g3') = uniformR (0.0 :: Double, 1.0) g2'
              (r2, g4') = uniformR (0.0 :: Double, 1.0) g3'
              (g2, g5') = uniformR (0.0 :: Double, 1.0) g4'
              (b2, g6') = uniformR (0.0 :: Double, 1.0) g5'
              albedo = V.mul (Vec3 r1 g1 b1) (Vec3 r2 g2 b2)
              mat = mkLambertian albedo
           in (toShape (Sphere center 0.2 mat), g6')
      | chooseMat < 0.95 =
          let (albedo, g1') = V.random 0.5 1.0 g
              (fuzz, g2') = uniformR (0.0 :: Double, 0.5) g1'
              mat = mkMetal albedo fuzz
           in (toShape (Sphere center 0.2 mat), g2')
      | otherwise =
          let mat = mkDielectric 1.5
           in (toShape (Sphere center 0.2 mat), g)

main :: IO ()
main = do
  let aspectRatio :: Double = 16.0 / 9.0
  let imageWidth :: Int = 1200
  let imageHeight :: Int = max 1 (floor (fromIntegral imageWidth / aspectRatio))

  let gen = mkStdGen 42
  let (spheres, gen1) = randomScene gen

  let scene = Scene spheres

  let cam =
        mkCamera
          CameraConfig
            { aspectRatio,
              imageWidth,
              samplesPerPixel = 500,
              maxDepth = 50,
              vFov = 20,
              lookFrom = Vec3 13 2 3,
              lookAt = Vec3 0 0 0,
              vUp = Vec3 0 1 0,
              defocusAngle = 0.6,
              focusDist = 10.0
            }

  putStrLn ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255")

  let (rows, _) = render cam scene gen1
  forM_ (zip [0 ..] rows) $ \(j, row) -> do
    hPutStrLn stderr ("Scanlines remaining: " ++ show (imageHeight - j))
    mapM_ (putStrLn . formatColor) row

  hPutStrLn stderr "Done."
