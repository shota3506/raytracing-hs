module Vec3 where

data Vec3 = Vec3 Double Double Double
  deriving (Show, Eq)

type Point3 = Vec3

vAdd :: Vec3 -> Vec3 -> Vec3
vAdd (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

vSub :: Vec3 -> Vec3 -> Vec3
vSub (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

vMul :: Vec3 -> Vec3 -> Vec3
vMul (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)

vScale :: Double -> Vec3 -> Vec3
vScale s (Vec3 x y z) = Vec3 (x * s) (y * s) (z * s)

vDiv :: Double -> Vec3 -> Vec3
vDiv s = vScale (1 / s)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

vLength :: Vec3 -> Double
vLength v = sqrt (dot v v)

vUnit :: Vec3 -> Vec3
vUnit v = vDiv (vLength v) v

vNegate :: Vec3 -> Vec3
vNegate = vScale (-1)
