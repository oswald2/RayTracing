{-# LANGUAGE
    OverloadedStrings
#-}
module Ray where


import           Data.Vec4


data Ray = Ray {
    rayOrigin :: !Vec4,
    rayDirection :: !Vec4
}


origin :: Ray -> Vec4
origin (Ray a _) = a

direction :: Ray -> Vec4
direction (Ray _ b) = b

pointAtParameter :: Ray -> Float -> Vec4
pointAtParameter (Ray a b) t = a + t `mulScalar` b
