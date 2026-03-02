module Vec3 where

import System.Random (StdGen, uniformR)

data Vec3 = Vec3 !Double !Double !Double
  deriving (Show, Eq)

type Point3 = Vec3

add :: Vec3 -> Vec3 -> Vec3
add (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

sub :: Vec3 -> Vec3 -> Vec3
sub (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

mul :: Vec3 -> Vec3 -> Vec3
mul (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)

scale :: Double -> Vec3 -> Vec3
scale s (Vec3 x y z) = Vec3 (x * s) (y * s) (z * s)

div :: Double -> Vec3 -> Vec3
div s = scale (1 / s)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

length :: Vec3 -> Double
length v = sqrt (dot v v)

unit :: Vec3 -> Vec3
unit v = Vec3.div (Vec3.length v) v

negate :: Vec3 -> Vec3
negate = scale (-1)

isNearZero :: Vec3 -> Bool
isNearZero (Vec3 x y z) = abs x < 1e-8 && abs y < 1e-8 && abs z < 1e-8

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = sub v (scale (2 * dot v n) n)

refract :: Vec3 -> Vec3 -> Double -> Vec3
refract uv n etaiOverEtat =
  let cosTheta = min (dot (Vec3.negate uv) n) 1.0
      rPerp = scale etaiOverEtat (add uv (scale cosTheta n))
      rParallel = Vec3.negate (scale (sqrt (max 0.0 (1.0 - dot rPerp rPerp))) n)
   in add rPerp rParallel

random :: Double -> Double -> StdGen -> (Vec3, StdGen)
random vmin vmax gen =
  let (x, gn1) = uniformR (vmin, vmax) gen
      (y, gn2) = uniformR (vmin, vmax) gn1
      (z, gn3) = uniformR (vmin, vmax) gn2
   in (Vec3 x y z, gn3)

uniformDisk :: StdGen -> (Vec3, StdGen)
uniformDisk gen =
  let (theta, gen1) = uniformR (0, 2 * pi) gen
      (u, gen2) = uniformR (0.0, 1.0) gen1
      r = sqrt u
   in (Vec3 (r * cos theta) (r * sin theta) 0, gen2)

uniformSphere :: StdGen -> (Vec3, StdGen)
uniformSphere gen =
  let (z, gen1) = uniformR (-1.0, 1.0) gen
      (theta, gen2) = uniformR (0, 2 * pi) gen1
      r = sqrt (1 - z * z)
   in (Vec3 (r * cos theta) (r * sin theta) z, gen2)
