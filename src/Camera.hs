module Camera where

import Color
import Data.List (foldl')
import Interval
import Ray
import Scene
import Shape qualified
import System.Random (StdGen, uniformR)
import Vec3 (Point3, Vec3 (..))
import Vec3 qualified as V

uniformSphere :: StdGen -> (Vec3, StdGen)
uniformSphere gen
  | lensq > 1e-12 && lensq <= 1.0 = (V.div (sqrt lensq) p, gen')
  | otherwise = uniformSphere gen'
  where
    (p, gen') = V.random (-1.0) 1.0 gen
    lensq = V.dot p p

uniformHemisphere :: Vec3 -> StdGen -> (Vec3, StdGen)
uniformHemisphere normal gen =
  let (v, gen') = uniformSphere gen
   in if V.dot v normal > 0 then (v, gen') else (V.negate v, gen')

data CameraConfig = CameraConfig
  { aspectRatio :: Double,
    imageWidth :: Int,
    samplesPerPixel :: Int,
    maxDepth :: Int
  }
  deriving (Show, Eq)

data Camera = Camera
  { config :: CameraConfig,
    imageHeight :: Int,
    pixelSamplesScale :: Double,
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
      pixelSamplesScale = 1.0 / fromIntegral (samplesPerPixel cfg),
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

sampleSquare :: StdGen -> (Vec3, StdGen)
sampleSquare gen =
  let (x, gen1) = uniformR (-0.5, 0.5) gen
      (y, gen2) = uniformR (-0.5, 0.5) gen1
   in (Vec3 x y 0, gen2)

sampleRay :: Camera -> Int -> Int -> StdGen -> (Ray, StdGen)
sampleRay cam i j gen =
  let (Vec3 x y _, gen1) = sampleSquare gen
      pixelCenter = V.add (V.add (pixel00Loc cam) (V.scale (fromIntegral i + x) (pixelDeltaU cam))) (V.scale (fromIntegral j + y) (pixelDeltaV cam))
      rayDirection = V.sub pixelCenter (center cam)
   in (Ray (center cam) rayDirection, gen1)

rayColor :: Ray -> Int -> Scene -> StdGen -> (Color, StdGen)
rayColor r depth scene gen
  | depth <= 0 = (Vec3 0 0 0, gen)
  | Just isec <- hit scene r (Interval 0.0001 (1 / 0)) =
      let (direction, gen') = uniformHemisphere (Shape.normal isec) gen
          (color, gen'') = rayColor (Ray (Shape.point isec) direction) (depth - 1) scene gen'
       in (V.scale 0.5 color, gen'')
  | otherwise =
      let unitDirection = V.unit (rayDirection r)
          (Vec3 _ y _) = unitDirection
          a = 0.5 * (y + 1.0)
       in (V.add (V.scale (1.0 - a) (Vec3 1.0 1.0 1.0)) (V.scale a (Vec3 0.5 0.7 1.0)), gen)

render :: Camera -> Scene -> StdGen -> ([[Color]], StdGen)
render cam scene gen =
  let (rows, gen') = foldl' renderRow ([], gen) [0 .. imageHeight cam - 1]
   in (reverse rows, gen')
  where
    renderRow (rows, g) j =
      let (revRow, g') = foldl' renderPixel ([], g) [0 .. imageWidth (config cam) - 1]
          renderPixel (colors, gi) i =
            let (color, go) = samplePixelColor i j gi
             in (color : colors, go)
       in (reverse revRow : rows, g')
    samplePixelColor i j g =
      let (colors, g') = nSamples (samplesPerPixel (config cam)) i j g
          scale = pixelSamplesScale cam
       in (V.scale scale (foldr V.add (Vec3 0 0 0) colors), g')
    nSamples 0 _ _ g = ([], g)
    nSamples n i j g =
      let (r, g') = sampleRay cam i j g
          (color, g'') = rayColor r (maxDepth (config cam)) scene g'
          (rest, g''') = nSamples (n - 1) i j g''
       in (color : rest, g''')
