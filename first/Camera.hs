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



import Data.Vec4
import Ray


data Camera = Camera {
    camOrigin :: !Vec4,
    camLowerLeftCorner :: !Vec4,
    camHorizontal :: !Vec4,
    camVertical :: !Vec4
} deriving Show


defaultCamera :: Camera
defaultCamera = Camera {
        camOrigin = vec4 0 0 0 0,
        camLowerLeftCorner = vec4 -2 -1 -1 0,
        camHorizontal = vec4 4 0 0 0,
        camVertical = vec4 0 2 0 0
    }

newCamera :: Vec4 -> Vec4 -> Vec4 -> Float -> Float -> Camera
newCamera lookfrom lookat vup vfov aspect = 
    let theta = vfov * pi / 180
        !halfHeight = tan (theta / 2.0)
        !halfWidth = aspect * halfHeight
        w = norm (lookfrom - lookat)
        u = norm (crossp vup w)
        v = crossp w u
    in
        Camera {
            camOrigin = lookfrom,
            camLowerLeftCorner = lookfrom - (halfWidth `mulScalar` u) - (halfHeight `mulScalar` v) - w,
            camHorizontal = 2 * halfWidth `mulScalar` u,
            camVertical = 2 * halfHeight `mulScalar` v
        }

camGetRay :: Camera -> Float -> Float -> Ray 
camGetRay cam s t =
    Ray (camOrigin cam) 
        (camLowerLeftCorner cam + s `mulScalar` camHorizontal cam + 
         t `mulScalar` camVertical cam)

