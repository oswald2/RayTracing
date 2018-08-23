{-# LANGUAGE
    OverloadedStrings
#-}
module Ray where


import           Vector


data Ray = Ray {
    rayA :: !Vec3,
    rayB :: !Vec3
}


origin :: Ray -> Vec3
origin (Ray a _) = a

direction :: Ray -> Vec3
direction (Ray _ b) = b

pointAtParameter :: Ray -> Double -> Vec3
pointAtParameter (Ray a b) t = a + t `mult` b
