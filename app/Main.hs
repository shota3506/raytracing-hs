module Main (main) where

import Color
import Control.Monad (forM_)
import Ray
import Scene
import Shape qualified
import Sphere
import System.IO (hPutStrLn, stderr)
import Vec3

rayColor :: Ray -> Scene -> Color
rayColor r scene
  | Just isec <- hit scene r 0.0 (1 / 0) =
      let Vec3 x y z = Shape.normal isec
       in vScale 0.5 (Vec3 (x + 1) (y + 1) (z + 1))
  | otherwise =
      let unitDirection = vUnit (rayDirection r)
          (Vec3 _ y _) = unitDirection
          a = 0.5 * (y + 1.0)
       in vScale (1.0 - a) (Vec3 1.0 1.0 1.0) `vAdd` vScale a (Vec3 0.5 0.7 1.0)

main :: IO ()
main = do
  -- Image
  let aspectRatio :: Double = 16.0 / 9.0

  let imageWidth :: Int = 400
  let imageHeight :: Int = max 1 (floor (fromIntegral imageWidth / aspectRatio))

  -- Scene

  let scene =
        Scene
          [ toShape (Sphere (Vec3 0 0 (-1)) 0.5),
            toShape (Sphere (Vec3 0 (-100.5) (-1)) 100)
          ]

  -- Camera
  let focalLength :: Double = 1.0
  let viewportHeight :: Double = 2.0
  let viewportWidth :: Double = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)
  let cameraCenter :: Vec3 = Vec3 0 0 0

  let viewportU = Vec3 viewportWidth 0 0
  let viewportV = Vec3 0 (-(1 * viewportHeight)) 0

  let pixelDeltaU = vDiv (fromIntegral imageWidth) viewportU
  let pixelDeltaV = vDiv (fromIntegral imageHeight) viewportV

  let viewportUpperLeft = cameraCenter `vSub` Vec3 0 0 focalLength `vSub` vDiv 2 viewportU `vSub` vDiv 2 viewportV
  let pixel00Loc = viewportUpperLeft `vAdd` vDiv 2 pixelDeltaU `vAdd` vDiv 2 pixelDeltaV

  putStrLn ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255")

  forM_ [0 .. imageHeight - 1] $ \j -> do
    hPutStrLn stderr ("Scanlines remaining: " ++ show (imageHeight - j))
    forM_ [0 .. imageWidth - 1] $ \i -> do
      let pixelCenter = pixel00Loc `vAdd` vScale (fromIntegral i) pixelDeltaU `vAdd` vScale (fromIntegral j) pixelDeltaV
      let rayDirection = vSub pixelCenter cameraCenter
      let r = Ray cameraCenter rayDirection
      putStrLn $ formatColor (rayColor r scene)

  hPutStrLn stderr "Done."
