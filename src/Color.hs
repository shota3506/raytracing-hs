module Color where

import Interval
import Vec3

type Color = Vec3

linearToGamma :: Double -> Double
linearToGamma x
  | x > 0 = sqrt x
  | otherwise = 0

formatColor :: Color -> String
formatColor (Vec3 r g b) =
  show rbyte ++ " " ++ show gbyte ++ " " ++ show bbyte
  where
    intensity = Interval 0.0 0.999
    rbyte = floor (255.999 * clamp intensity (linearToGamma r)) :: Int
    gbyte = floor (255.999 * clamp intensity (linearToGamma g)) :: Int
    bbyte = floor (255.999 * clamp intensity (linearToGamma b)) :: Int
