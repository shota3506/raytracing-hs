module Color where

import Vec3

type Color = Vec3

formatColor :: Color -> String
formatColor (Vec3 r g b) =
  show rbyte ++ " " ++ show gbyte ++ " " ++ show bbyte
  where
    rbyte = floor (255.999 * r) :: Int
    gbyte = floor (255.999 * g) :: Int
    bbyte = floor (255.999 * b) :: Int
