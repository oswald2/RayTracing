{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module Camera where



import Vector
import Ray


data Camera = Camera {
    camOrigin :: Vec3,
    camLowerLeftCorner :: Vec3,
    camHorizontal :: Vec3,
    camVertical :: Vec3
}   


defaultCamera :: Camera
defaultCamera = Camera {
        camOrigin = Vec3 0 0 0,
        camLowerLeftCorner = Vec3 -2 -1 -1,
        camHorizontal = Vec3 4 0 0,
        camVertical = Vec3 0 2 0
    }

camGetRay :: Camera -> Double -> Double -> Ray 
camGetRay cam u v =
    Ray (camOrigin cam) 
        (camLowerLeftCorner cam + u `mult` camHorizontal cam + 
         v `mult` camVertical cam - camOrigin cam)

