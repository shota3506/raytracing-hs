module Main (main) where

import Camera
import Color
import Control.Monad (forM_)
import Scene
import Sphere
import System.IO (hPutStrLn, stderr)
import Vec3 (Vec3 (..))

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
  let cam = mkCamera CameraConfig {aspectRatio, imageWidth}

  putStrLn ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255")

  forM_ (zip [0 ..] (render cam scene)) $ \(j, row) -> do
    hPutStrLn stderr ("Scanlines remaining: " ++ show (imageHeight - j))
    mapM_ (putStrLn . formatColor) row

  hPutStrLn stderr "Done."
