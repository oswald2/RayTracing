{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module Main where


import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO             as T

import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int

import           System.Random.SplitMix
import           System.IO

import           Data.Vec4
import           Color
import           Ray
import           Hitable
import           HitableList                    ( )
import           Sphere
import           Camera
import           Material


nx = 1000
ny = 500
ns = 100
maxDepth = 50

values :: Camera -> SMGen -> [[Color]]
values cam rand = map (inner cam rand) [ny - 1, ny - 2 .. 0]

inner :: Camera -> SMGen -> Int -> [Color]
inner cam rand j = map (calc cam rand j) [0 .. nx - 1]



calc :: Camera -> SMGen -> Int -> Int -> Color
calc cam rand j i =
    let (v, _) = foldr calcVal (black, rand) [1 .. ns]
        Color r g b = v <</>> ns
    in  Color (sqrt r) (sqrt g) (sqrt b)
  where
    calcVal _ (c0, ra0) =
        let (v1, ra1)  = nextFloat ra0
            (v2, ra2)  = nextFloat ra1
            u          = (fromIntegral i + v1) / fromIntegral nx
            v          = (fromIntegral j + v2) / fromIntegral ny
            r          = camGetRay cam u v
            --p          = pointAtParameter r 2.0

            (col, ra3) = color ra2 r world 0
        in  (c0 <<+>> col, ra3)




-- example6
color :: Hitable a => SMGen -> Ray -> a -> Int -> (Color, SMGen)
color rand rIn x !depth = 
    case hit x rIn 0.001 (maxNonInfiniteFloat 0) of
        Just (ht, mat) -> if depth < maxDepth
            then case scatter rand rIn ht mat of
                Just ((ScatterRecord attenuation scattered), rand1) ->
                    let (Color r g b, rand2) = color rand1 scattered x (depth + 1)
                        (vecX, vecY, vecZ, _) = unpack attenuation
                        col = Color (r * vecX) (g * vecY) (b * vecZ)
                    in  (col, rand2)
                Nothing -> (Color 0 0 0, rand)
            else (Color 0 0 0, rand)
        Nothing ->
            let unitDirection = norm (direction rIn)
                (_, vecY, _, _) = unpack unitDirection
                t             = 0.5 * (vecY + 1.0)

                v1            = vec4 1.0 1.0 1.0 0
                v2            = vec4 0.5 0.7 1.0 0
                v3            = ((1.0 - t) `mulScalar` v1) + (t `mulScalar` v2)
            in  (vecToColor v3, rand)




maxNonInfiniteFloat :: Float -> Float
maxNonInfiniteFloat a = encodeFloat m n
  where
    b       = floatRadix a
    e       = floatDigits a
    (_, e') = floatRange a
    m       = b ^ e - 1
    n       = e' - e

lowerLeftCorner :: Vec4
lowerLeftCorner = vec4 (-2.0) (-1.0) (-1.0) 0

horizontalV :: Vec4
horizontalV = vec4 4.0 0.0 0.0 0.0

verticalV :: Vec4
verticalV = vec4 0.0 2.0 0.0 0.0

originV :: Vec4
originV = vec4 0.0 0.0 0.0 0.0


world :: [HitObject]
world =
    [ HitObject
        (Sphere (vec4 0 0 -1 0) 0.5 (Material (Lambertian (vec4 0.8 0.3 0.3 0))))
    , HitObject
        (Sphere (vec4 0 -100.5 -1 0) 100 (Material (Lambertian (vec4 0.8 0.8 0 0))))
     , HitObject
         (Sphere (vec4 1 0 -1 0) 0.5 (Material (Metal (vec4 0.8 0.6 0.2 0) 0.0)))
     , HitObject
         (Sphere (vec4 -1 0 -1 0) 0.5 (Material (Metal (vec4 0.8 0.8 0.8 0) 0.0)))
    ]




camera = defaultCamera
--camera = newCamera 90 (fromIntegral nx / fromIntegral ny)
--camera = newCamera (Vec3 -2 2 1) (Vec3 0 0 -1) (Vec3 0 1 0) 90 (fromIntegral nx / fromIntegral ny)


main :: IO ()
main = do
    rand <- newSMGen

    let content =
            fromLazyText "P3\n"
                <> decimal nx
                <> sp
                <> decimal ny
                <> fromText "\n255\n"
                <> valueContent (values camera rand)


        sp = singleton ' '

        valueContent :: [[Color]] -> Builder
        valueContent = mconcat . map lineToBuilder

        lineToBuilder :: [Color] -> Builder
        lineToBuilder = mconcat . map colorToBuilder

    T.hPutStrLn stderr (T.pack (show camera))
    T.putStrLn (toLazyText content)
