{-# LANGUAGE 
  BangPatterns
#-}
{-# OPTIONS_GHC -mavx #-}
{-# OPTIONS_GHC -msse #-}
{-# OPTIONS_GHC -msse2 #-}
{-# OPTIONS_GHC -msse4 #-}
module Main (main) where


import Data.Vec4


main :: IO ()
main = do 

  let v1 = vec4 0.1 0.2 0.3 0.4 
      v2 = vec4 3 2 1 0 

      a = 0.314 

      !e1 = v1 + v2 
      !e2 = v1 - v2 
      !e3 = v1 * v2 

      !e4 = a `mulScalar` v1 
      !e5 = v2 `divScalar` a

      !e6 = scalarp v1 v2 
      !e7 = dotp v1 v2 

  putStrLn $ "V1: " <> show v1
  putStrLn $ "V2: " <> show v2
  putStrLn $ "\nE1: " <> show e1 
  putStrLn $ "E2: " <> show e2
  putStrLn $ "E3: " <> show e3 
  putStrLn $ "E4: " <> show e4 
  putStrLn $ "E5: " <> show e5 
  putStrLn $ "E6: " <> show e6 
  putStrLn $ "E7: " <> show e7 

  return ()