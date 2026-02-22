module Main (main) where

import Color
import Control.Monad (forM_)
import Ray
import System.IO (hPutStrLn, stderr)
import Vec3

hitSphere :: Vec3 -> Double -> Ray -> Double
hitSphere center radius r =
  let oc = vSub center (rayOrigin r)
      a = dot (rayDirection r) (rayDirection r)
      b = -(2.0 * dot oc (rayDirection r))
      c = dot oc oc - radius * radius
      discriminant = b * b - 4 * a * c
   in if discriminant < 0 then -1.0 else (-b - sqrt discriminant) / (2.0 * a)

rayColor :: Ray -> Color
rayColor r
  | t > 0 =
      let n = vUnit (rayAt r t `vSub` sphereCenter)
          Vec3 x y z = n
       in vScale 0.5 (Vec3 (x + 1) (y + 1) (z + 1))
  | otherwise =
      let unitDirection = vUnit (rayDirection r)
          (Vec3 _ y _) = unitDirection
          a = 0.5 * (y + 1.0)
       in vScale (1.0 - a) (Vec3 1.0 1.0 1.0) `vAdd` vScale a (Vec3 0.5 0.7 1.0)
  where
    sphereCenter = Vec3 0 0 (-1)
    t = hitSphere sphereCenter 0.5 r

main :: IO ()
main = do
  let aspectRatio :: Double = 16.0 / 9.0

  let imageWidth :: Int = 400
  let imageHeight :: Int = max 1 (floor (fromIntegral imageWidth / aspectRatio))

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
      putStrLn $ formatColor (rayColor r)

  hPutStrLn stderr "Done."
