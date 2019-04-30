{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module Camera 
    (
        Camera
        , camGetRay
        , defaultCamera
        , newCamera
    )
where



import Vector
import Ray


data Camera = Camera {
    camOrigin :: !Vec3,
    camLowerLeftCorner :: !Vec3,
    camHorizontal :: !Vec3,
    camVertical :: !Vec3
} deriving Show


defaultCamera :: Camera
defaultCamera = Camera {
        camOrigin = Vec3 0 0 0,
        camLowerLeftCorner = Vec3 -2 -1 -1,
        camHorizontal = Vec3 4 0 0,
        camVertical = Vec3 0 2 0
    }

newCamera :: Vec3 -> Vec3 -> Vec3 -> Double -> Double -> Camera
newCamera lookfrom lookat vup vfov aspect = 
    let theta = vfov * pi / 180
        !halfHeight = tan (theta / 2.0)
        !halfWidth = aspect * halfHeight
        w = unitVector (lookfrom - lookat)
        u = unitVector (cross vup w)
        v = cross w u
    in
        Camera {
            camOrigin = lookfrom,
            camLowerLeftCorner = lookfrom - (halfWidth `mult` u) - (halfHeight `mult` v) - w,
            camHorizontal = 2 * halfWidth `mult` u,
            camVertical = 2 * halfHeight `mult` v
        }

camGetRay :: Camera -> Double -> Double -> Ray 
camGetRay cam s t =
    Ray (camOrigin cam) 
        (camLowerLeftCorner cam + s `mult` camHorizontal cam + 
         t `mult` camVertical cam)

