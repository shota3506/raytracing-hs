module Main (main) where

import Color (formatColor)
import Control.Monad (forM_)
import System.IO (hPutStrLn, stderr)
import Vec3 (Vec3 (Vec3))

main :: IO ()
main = do
  let imageWidth :: Int = 256
      imageHeight :: Int = 256

  putStrLn ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255")

  forM_ [0 .. imageHeight - 1] $ \j -> do
    hPutStrLn stderr ("Scanlines remaining: " ++ show (imageHeight - j))
    forM_ [0 .. imageWidth - 1] $ \i -> do
      let r :: Double = fromIntegral i / fromIntegral (imageWidth - 1)
          g :: Double = fromIntegral j / fromIntegral (imageHeight - 1)
          b :: Double = 0.0
      putStrLn $ formatColor (Vec3 r g b)

  hPutStrLn stderr "Done."
