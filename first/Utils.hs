module Utils where


import           Vector

import           Data.Int

import           System.Random.Mersenne.Pure64


randomInUnitSphere :: PureMT -> (Vec3, PureMT)
randomInUnitSphere rand = worker rand
  where
    worker r0 =
        let (v1, r1) = randomVal r0
            (v2, r2) = randomVal r1
            (v3, r3) = randomVal r2

            p        = 2.0 `mult` Vec3 v1 v2 v3 - Vec3 1 1 1
        in  if squared_length p >= 1.0 then worker r3 else (p, r3)



randomVal :: PureMT -> (Double, PureMT)
randomVal rand =
    let (val, r) = randomInt64 rand
    in  (fromIntegral val / fromIntegral (maxBound :: Int64), r)

