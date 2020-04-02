module Utils where


import           Data.Vec4

import           System.Random.SplitMix


randomInUnitSphere :: SMGen -> (Vec4, SMGen)
randomInUnitSphere = worker
  where
    worker r0 =
        let (v1, r1) = random r0 

            p        = 2.0 `mulScalar` v1 - vec4 1 1 1 0
        in  if magn2 p >= 1.0 then worker r1 else (p, r1)


saturate :: Double -> Double
saturate x = max (min x 0.0) 1.0

