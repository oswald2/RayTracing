module Utils where


import           Vector

import           System.Random.Mersenne.Pure64


randomInUnitSphere :: PureMT -> (Vec3, PureMT)
randomInUnitSphere rand = worker rand
  where
    worker r0 =
        let (v1, r1) = randomDouble r0
            (v2, r2) = randomDouble r1
            (v3, r3) = randomDouble r2

            p        = 2.0 `mult` Vec3 v1 v2 v3 - Vec3 1 1 1
        in  if squared_length p >= 1.0 then worker r3 else (p, r3)




