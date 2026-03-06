module Camera where

import Color
import Data.List (foldl')
import Interval
import Material
import Ray
import Scene
import System.Random (StdGen, uniformR)
import Vec3 (Point3, Vec3 (..))
import Vec3 qualified as V

data CameraConfig = CameraConfig
  { aspectRatio :: Double,
    imageWidth :: Int,
    samplesPerPixel :: Int,
    maxDepth :: Int,
    vFov :: Double, -- Visual view angle (field of view)
    lookFrom :: Point3, -- Point the camera is located at
    lookAt :: Point3, -- Point the camera is looking at
    vUp :: Vec3, -- "Up" direction for the camera
    defocusAngle :: Double, -- Angle of defocus blur in degrees
    focusDist :: Double -- Distance to the focal plane for defocus blur
  }
  deriving (Show, Eq)

data Camera = Camera
  { config :: CameraConfig,
    imageHeight :: Int,
    pixelSamplesScale :: Double,
    center :: Point3,
    pixel00Loc :: Point3,
    pixelDeltaU :: Vec3,
    pixelDeltaV :: Vec3,
    defocusDiskU :: Vec3,
    defocusDiskV :: Vec3
  }
  deriving (Show, Eq)

mkCamera :: CameraConfig -> Camera
mkCamera cfg =
  Camera
    { config = cfg,
      imageHeight = height,
      pixelSamplesScale = 1.0 / fromIntegral (samplesPerPixel cfg),
      center = cameraCenter,
      pixel00Loc = V.add viewportUpperLeft (V.div 2 (V.add pdu pdv)),
      pixelDeltaU = pdu,
      pixelDeltaV = pdv,
      defocusDiskU = V.scale defocusRadius u,
      defocusDiskV = V.scale defocusRadius v
    }
  where
    ratio = aspectRatio cfg
    width = imageWidth cfg
    height = max 1 (floor (fromIntegral width / ratio))
    theta = vFov cfg * pi / 180
    h = tan (theta / 2)
    viewportHeight = 2 * h * focusDist cfg
    viewportWidth = viewportHeight * (fromIntegral width / fromIntegral height)
    cameraCenter = lookFrom cfg
    w = V.unit (V.sub (lookFrom cfg) (lookAt cfg))
    u = V.unit (V.cross (vUp cfg) w)
    v = V.cross w u
    viewportU = V.scale viewportWidth u
    viewportV = V.scale viewportHeight (V.negate v)
    pdu = V.div (fromIntegral width) viewportU
    pdv = V.div (fromIntegral height) viewportV
    viewportUpperLeft = V.sub (V.sub (V.sub cameraCenter (V.scale (focusDist cfg) w)) (V.div 2 viewportU)) (V.div 2 viewportV)
    defocusRadius = focusDist cfg * tan (defocusAngle cfg / 2 * pi / 180)

sampleSquare :: StdGen -> (Vec3, StdGen)
sampleSquare gen =
  let (x, gen1) = uniformR (-0.5, 0.5) gen
      (y, gen2) = uniformR (-0.5, 0.5) gen1
   in (Vec3 x y 0, gen2)

sampleRay :: Camera -> Int -> Int -> StdGen -> (Ray, StdGen)
sampleRay cam i j gen =
  let (Vec3 x y _, gen1) = sampleSquare gen
      offsetU = V.scale (fromIntegral i + x) (pixelDeltaU cam)
      offsetV = V.scale (fromIntegral j + y) (pixelDeltaV cam)
      pixelCenter = V.add (V.add (pixel00Loc cam) offsetU) offsetV
      (rayOrigin, gen2) = sampleDefocusDisk cam gen1
      dir = V.sub pixelCenter rayOrigin
      (time, gen3) = uniformR (0.0, 1.0) gen2
   in (Ray rayOrigin dir time, gen3)

sampleDefocusDisk :: Camera -> StdGen -> (Vec3, StdGen)
sampleDefocusDisk cam gen
  | defocusAngle (config cam) <= 0 = (center cam, gen)
  | otherwise =
      let (Vec3 x y _, gen1) = V.uniformDisk gen
          p = V.add (V.add (center cam) (V.scale x (defocusDiskU cam))) (V.scale y (defocusDiskV cam))
       in (p, gen1)

rayColor :: Ray -> Int -> Scene -> StdGen -> (Color, StdGen)
rayColor r depth scene gen
  | depth <= 0 = (Vec3 0 0 0, gen)
  | Just (isec, mat) <- hit scene r (Interval 0.0001 (1 / 0)) =
      case scatter mat r isec gen of
        Just (attenuation, scattered, gen1) ->
          let (color, gen2) = rayColor scattered (depth - 1) scene gen1
           in (V.mul attenuation color, gen2)
        Nothing -> (Vec3 0 0 0, gen)
  | otherwise =
      let unitDirection = V.unit (direction r)
          (Vec3 _ y _) = unitDirection
          a = 0.5 * (y + 1.0)
       in (V.add (V.scale (1.0 - a) (Vec3 1.0 1.0 1.0)) (V.scale a (Vec3 0.5 0.7 1.0)), gen)

render :: Camera -> Scene -> StdGen -> ([[Color]], StdGen)
render cam scene gen =
  let (rows, gen1) = foldl' renderRow ([], gen) [0 .. imageHeight cam - 1]
   in (reverse rows, gen1)
  where
    renderRow (rows, g) j =
      let (revRow, g1) = foldl' renderPixel ([], g) [0 .. imageWidth (config cam) - 1]
          renderPixel (colors, gi) i =
            let (color, go) = samplePixelColor i j gi
             in (color : colors, go)
       in (reverse revRow : rows, g1)
    samplePixelColor i j g =
      let s = pixelSamplesScale cam
          (acc, g1) = accumSamples (samplesPerPixel (config cam)) i j (Vec3 0 0 0) g
       in (V.scale s acc, g1)
    accumSamples 0 _ _ acc g = (acc, g)
    accumSamples n i j acc g =
      let (r, g1) = sampleRay cam i j g
          (color, g2) = rayColor r (maxDepth (config cam)) scene g1
       in accumSamples (n - 1) i j (V.add acc color) g2
