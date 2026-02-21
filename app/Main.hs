module Main (main) where

main :: IO ()
main = do
  let imageWidth :: Int = 256
      imageHeight :: Int = 256

  putStrLn ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255")
  mapM_
    putStrLn
    [ show ir ++ " " ++ show ig ++ " " ++ show ib
    | j <- [0 .. imageHeight - 1],
      i <- [0 .. imageWidth - 1],
      let r :: Double = fromIntegral i / fromIntegral (imageWidth - 1)
          g :: Double = fromIntegral j / fromIntegral (imageHeight - 1)
          b :: Double = 0.0
          ir = round (255.999 * r) :: Int
          ig = round (255.999 * g) :: Int
          ib = round (255.999 * b) :: Int
    ]
