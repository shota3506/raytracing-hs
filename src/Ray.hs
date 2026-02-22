module Ray where

import Vec3

data Ray = Ray
  { origin :: Point3,
    direction :: Vec3
  }

rayAt :: Ray -> Double -> Point3
rayAt (Ray orig dir) t = vAdd orig (vScale t dir)
