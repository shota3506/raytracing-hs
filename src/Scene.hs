module Scene where

import Ray
import Shape qualified

newtype Scene = Scene
  { shapes :: [Shape.Shape]
  }

addShape :: Scene -> Shape.Shape -> Scene
addShape (Scene ss) s = Scene (s : ss)

hit :: Scene -> Ray -> Double -> Double -> Maybe Shape.Intersection
hit (Scene ss) ray tmin tmax = foldl closest Nothing ss
  where
    closest :: Maybe Shape.Intersection -> Shape.Shape -> Maybe Shape.Intersection
    closest Nothing s = Shape.hit s ray tmin tmax
    closest (Just i) s =
      case Shape.hit s ray tmin (Shape.t i) of
        Nothing -> Just i
        Just i' -> Just i'
