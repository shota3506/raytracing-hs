module Ray where

import Vec3

data Ray = Ray
  { rayOrigin :: Point3,
    rayDirection :: Vec3
  }

rayAt :: Ray -> Double -> Point3
rayAt (Ray orig dir) t = vAdd orig (vScale t dir)
