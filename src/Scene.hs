module Scene where

import Data.List (foldl')
import Intersection
import Interval
import Material
import Ray
import Shape qualified

newtype Scene = Scene
  { shapes :: [Shape.Shape]
  }

addShape :: Scene -> Shape.Shape -> Scene
addShape (Scene ss) s = Scene (s : ss)

hit :: Scene -> Ray -> Interval -> Maybe (Intersection, Material)
hit (Scene ss) ray iv = foldl' closest Nothing ss
  where
    closest :: Maybe (Intersection, Material) -> Shape.Shape -> Maybe (Intersection, Material)
    closest Nothing s = Shape.hit s ray iv
    closest (Just (i, m)) s =
      case Shape.hit s ray (Interval (imin iv) (t i)) of
        Nothing -> Just (i, m)
        Just im' -> Just im'
