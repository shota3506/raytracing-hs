module Camera where

import Color
import Interval
import Ray
import Scene
import Shape qualified
import Vec3 (Point3, Vec3 (..))
import Vec3 qualified as V

data CameraConfig = CameraConfig
  { aspectRatio :: Double,
    imageWidth :: Int
  }
  deriving (Show, Eq)

data Camera = Camera
  { config :: CameraConfig,
    imageHeight :: Int,
    center :: Point3,
    pixel00Loc :: Point3,
    pixelDeltaU :: Vec3,
    pixelDeltaV :: Vec3
  }
  deriving (Show, Eq)

mkCamera :: CameraConfig -> Camera
mkCamera cfg =
  Camera
    { config = cfg,
      imageHeight = height,
      center = cameraCenter,
      pixel00Loc = V.add viewportUpperLeft (V.div 2 (V.add pdu pdv)),
      pixelDeltaU = pdu,
      pixelDeltaV = pdv
    }
  where
    ratio :: Double = aspectRatio cfg
    width :: Int = imageWidth cfg
    height :: Int = max 1 (floor (fromIntegral width / ratio))
    focalLength :: Double = 1.0
    viewportHeight :: Double = 2.0
    viewportWidth :: Double = viewportHeight * (fromIntegral width / fromIntegral height)
    cameraCenter = Vec3 0 0 0
    viewportU = Vec3 viewportWidth 0 0
    viewportV = Vec3 0 (-(1 * viewportHeight)) 0
    pdu = V.div (fromIntegral width) viewportU
    pdv = V.div (fromIntegral height) viewportV
    viewportUpperLeft = V.sub (V.sub (V.sub cameraCenter (Vec3 0 0 focalLength)) (V.div 2 viewportU)) (V.div 2 viewportV)

rayColor :: Ray -> Scene -> Color
rayColor r scene
  | Just isec <- hit scene r (Interval 0.0 (1 / 0)) =
      let Vec3 x y z = Shape.normal isec
       in V.scale 0.5 (Vec3 (x + 1) (y + 1) (z + 1))
  | otherwise =
      let unitDirection = V.unit (rayDirection r)
          (Vec3 _ y _) = unitDirection
          a = 0.5 * (y + 1.0)
       in V.add (V.scale (1.0 - a) (Vec3 1.0 1.0 1.0)) (V.scale a (Vec3 0.5 0.7 1.0))

render :: Camera -> Scene -> [[Color]]
render cam scene =
  [ [ rayColor (Ray (center cam) rayDirection) scene
    | i <- [0 .. imageWidth (config cam) - 1],
      let pixelCenter = V.add (V.add (pixel00Loc cam) (V.scale (fromIntegral i) (pixelDeltaU cam))) (V.scale (fromIntegral j) (pixelDeltaV cam))
          rayDirection = V.sub pixelCenter (center cam)
    ]
  | j <- [0 .. imageHeight cam - 1]
  ]
