module Color where

import Interval
import Vec3

type Color = Vec3

formatColor :: Color -> String
formatColor (Vec3 r g b) =
  show rbyte ++ " " ++ show gbyte ++ " " ++ show bbyte
  where
    intensity = Interval 0.0 0.999
    rbyte = floor (255.999 * clamp intensity r) :: Int
    gbyte = floor (255.999 * clamp intensity g) :: Int
    bbyte = floor (255.999 * clamp intensity b) :: Int
