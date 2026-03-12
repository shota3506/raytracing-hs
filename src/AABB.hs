module AABB where

import Interval (Interval (..))
import Interval qualified as I
import Ray
import Vec3 (Vec3 (..))

data AABB = AABB
  { x :: Interval,
    y :: Interval,
    z :: Interval
  }

empty :: AABB
empty = AABB {x = I.empty, y = I.empty, z = I.empty}

enclose :: AABB -> AABB -> AABB
enclose (AABB x1 y1 z1) (AABB x2 y2 z2) =
  AABB {x = I.enclose x1 x2, y = I.enclose y1 y2, z = I.enclose z1 z2}

intersects :: AABB -> Ray -> Interval -> Bool
intersects (AABB x y z) (Ray origin dir _) (Interval tmin tmax) =
  let (Vec3 ox oy oz) = origin
      (Vec3 dx dy dz) = dir
      axes = [(x, ox, dx), (y, oy, dy), (z, oz, dz)]
      (tmin', tmax') = foldl updateInterval (tmin, tmax) axes
   in tmin' < tmax'
  where
    updateInterval (lo, hi) (Interval amin amax, o, d)
      | d == 0 = if amin <= o && o <= amax then (lo, hi) else (1 / 0, -(1 / 0))
      | otherwise =
          let inv = 1.0 / d
              t0 = (amin - o) * inv
              t1 = (amax - o) * inv
           in if t0 < t1 then (max t0 lo, min t1 hi) else (max t1 lo, min t0 hi)
