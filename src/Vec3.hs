module Vec3 where

data Vec3 = Vec3 Double Double Double
  deriving (Show, Eq)

vAdd :: Vec3 -> Vec3 -> Vec3
vAdd (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

vSub :: Vec3 -> Vec3 -> Vec3
vSub (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
