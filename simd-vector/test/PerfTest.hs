{-# LANGUAGE
  BangPatterns
#-}
module Main 
where 



import Data.Vec4

import Criterion.Main 



calc1 :: Float -> Float 
calc1 _ = 
  let v1 = vec4 1 1 1 1 
      v2 = vec4 0.1 0.2 0.3 0.4 

      a = 0.314 

      !e1 = v1 + v2 
      !e2 = v1 - v2 
      !e3 = v1 * v2 

      !e4 = a `mulScalar` v1 
      !e5 = v2 `divScalar` a

      !e6 = scalarp v1 v2 
      !e7 = dotp v1 v2 
  in 
  magn (e6 `mulScalar` (e1 + e2 + e3 + e4 + e5 + e7))


mkVec :: Int -> Vec4 
mkVec n = vec4 (fromIntegral n) (fromIntegral n) (fromIntegral n) (fromIntegral n)

calc2 :: Int -> Float 
calc2 n = 
  let v = foldl (*) (vec4 1 1 1 1) (replicate n (mkVec n))
  in scalarp v v 


suite :: [Benchmark]
suite = 
  [bgroup "native"
    [bench "Vec calc1" $ whnf calc1 5
    , bench "Vec calc2" $ whnf calc2 100000]
  ]



main :: IO () 
main = defaultMain suite