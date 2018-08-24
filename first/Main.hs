{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module Main where


import qualified Data.Text.Lazy.IO             as T

import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int

import           System.Random.Mersenne.Pure64

import           Vector
import           Color
import           Ray
import           Hitable
import           HitableList                    ( )
import           Sphere
import           Camera
import           Utils
import           Material


nx = 200
ny = 100
ns = 100
maxDepth = 50

values :: Camera -> PureMT -> [[Color]]
values cam rand = map (inner cam rand) [ny - 1, ny - 2 .. 0]

inner :: Camera -> PureMT -> Int -> [Color]
inner cam rand j = map (calc cam rand j) [0 .. nx - 1]

{-

-- example 1
calc :: Int -> Int -> Color
calc j i = mkColor (fromIntegral i / fromIntegral nx)
                   (fromIntegral j / fromIntegral ny)
                   0.2
-}
{-}
calc :: Camera -> PureMT -> Int -> Int -> Color
calc cam rand j i =
    let (v, _) = foldr calcVal (nullVector, rand) [1 .. ns]
        v1     = v `divide` ns
    in  mkColor (sqrt (vecX v1)) (sqrt (vecY v1)) (sqrt (vecZ v1))
  where
    calcVal _ (c0, ra0) =
        let (v1, ra1)  = randomVal ra0
            (v2, ra2)  = randomVal ra1
            u          = (fromIntegral i + v1) / fromIntegral nx
            v          = (fromIntegral j + v2) / fromIntegral ny
            r          = camGetRay cam


            let (randVec, ra1) = randomInUnitSphere rand
                target         = htP ht + htNormal ht + randVec
                (res, ra2)     = color ra1 (Ray (htP ht) (target - htP ht)) world
            in  (0.4 `mult` res, ra2) u v
            (col, ra3) = color ra2 r world
        in  (c0 + col, ra3)
-}

calc :: Camera -> PureMT -> Int -> Int -> Color
calc cam rand j i =
    let (v, _) = foldr calcVal (nullVector, rand) [1 .. ns]
        v1     = v `divide` ns
    in  mkColor (sqrt (vecX v1)) (sqrt (vecY v1)) (sqrt (vecZ v1))
  where
    calcVal _ (c0, ra0) =
        let (v1, ra1)  = randomVal ra0
            (v2, ra2)  = randomVal ra1
            u          = (fromIntegral i + v1) / fromIntegral nx
            v          = (fromIntegral j + v2) / fromIntegral ny
            r          = camGetRay cam u v
            --p          = pointAtParameter r 2.0

            (col, ra3) = color ra2 r world 0
        in  (c0 + col, ra3)


{-
-- example 2
color :: Ray -> Color
color r =
    let unitDirection = unitVector (direction r)
        t = 0.5 * (vecY unitDirection + 1.0)

        v1 = Vec3 1.0 1.0 1.0
        v2 = Vec3 0.5 0.7 1.0
        v3 = ((1.0 - t) `mult` v1) + (t `mult` v2)
    in
    mkColor (vecX v3) (vecY v3) (vecZ v3)
-}

{-}
--example 3
color :: Ray -> Color
color r =
    if hitSphere (Vec3 0.0 0.0 (-1.0)) 0.5 r 
        then Color 255 0 0 
        else
            let unitDirection = unitVector (direction r)
                t = 0.5 * (vecY unitDirection + 1.0)

                v1 = Vec3 1.0 1.0 1.0
                v2 = Vec3 0.5 0.7 1.0
                v3 = ((1.0 - t) `mult` v1) + (t `mult` v2)
            in
                mkColor (vecX v3) (vecY v3) (vecZ v3)


hitSphere :: Vec3 -> Double -> Ray -> Bool
hitSphere !center !radius !r =
    let oc = origin r - center
        dirr = direction r
        a = dot dirr dirr
        b = 2.0 * dot oc dirr
        c = dot oc oc - radius * radius
        discriminant = b * b - 4 * a * c
    in
    discriminant > 0
-}

{-}
--example 3
color :: Ray -> Color
color r
    = let t = hitSphere (Vec3 0.0 0.0 (-1.0)) 0.5 r
      in
          if t > 0.0
              then
                  let
                      n = unitVector
                          ((pointAtParameter r t) - (Vec3 0.0 0.0 -1.0))
                      n1 =
                          0.5 `mult` Vec3 (vecX n + 1) (vecY n + 1) (vecZ n + 1)
                  in
                      mkColor (vecX n1) (vecY n1) (vecZ n1)
              else
                  let unitDirection = unitVector (direction r)
                      t             = 0.5 * (vecY unitDirection + 1.0)

                      v1            = Vec3 1.0 1.0 1.0
                      v2            = Vec3 0.5 0.7 1.0
                      v3            = ((1.0 - t) `mult` v1) + (t `mult` v2)
                  in  mkColor (vecX v3) (vecY v3) (vecZ v3)


hitSphere :: Vec3 -> Double -> Ray -> Double
hitSphere !center !radius !r =
    let oc           = origin r - center
        dirr         = direction r
        a            = dot dirr dirr
        b            = 2.0 * dot oc dirr
        c            = dot oc oc - radius * radius
        discriminant = b * b - 4 * a * c
    in  if discriminant < 0 then -1.0 else (-b - sqrt discriminant) / (2.0 * a)
-}

{-}


        let (randVec, ra1) = randomInUnitSphere rand
            target         = htP ht + htNormal ht + randVec
            (res, ra2)     = color ra1 (Ray (htP ht) (target - htP ht)) world
        in  (0.4 `mult` res, ra2)
-- example4
color :: Hitable a => Ray -> a -> Vec3
color r x = case hit x r 0.0 (maxr more details):
 - first-0.1.0.0 (exe:first) (configuration changNonInfiniteFloat 0) of
    Just ht ->
        let v = 0.5 `mult` Vec3 ((vecX (htNormal ht)) + 1)
                                ((vecY (htNormal ht)) + 1)
                                ((vecZ (htNormal ht)) + 1)
        in  v
    Nothing ->
        let unitDirection = unitVector (direction r)
            t             = 0.5 * (vecY unitDirection + 1.0)

            v1            = Vec3 1.0 1.0 1.0
            v2            = Vec3 0.5 0.7 1.0
            v3            = ((1.0 - t) `mult` v1) + (t `mult` v2)
        in  v3
-}



{-r more details):
 - first-0.1.0.0 (exe:first) (configuration chang
-- example5
color :: Hitable a => PureMT -> Ray -> a -> (Vec3, PureMT)
color rand r x = case hit x r 0.001 (maxNonInfiniteFloat 0) of
    Just ht ->
        let (randVec, ra1) = randomInUnitSphere rand
            target         = htP ht + htNormal ht + randVec
            (res, ra2)     = color ra1 (Ray (htP ht) (target - htP ht)) world
        in  (0.4 `mult` res, ra2)
    Nothing ->
        let unitDirection = unitVector (direction r)
            t             = 0.5 * (vecY unitDirection + 1.0)

            v1            = Vec3 1.0 1.0 1.0
            v2            = Vec3 0.5 0.7 1.0
            v3            = ((1.0 - t) `mult` v1) + (t `mult` v2)
        in  (v3, rand)
-}

-- example6
color :: Hitable a => PureMT -> Ray -> a -> Int -> (Vec3, PureMT)
color rand r x !depth = case hit x r 0.001 (maxNonInfiniteFloat 0) of
    Just (ht, mat) -> case scatter rand r ht mat of
        Just (attenuation, scattered, rand1) ->
            let (col, rand2) = color rand1 scattered world (depth + 1)
            in  
                if depth < maxDepth
                    then (attenuation * col, rand2)
                    else (Vec3 0 0 0, rand1)
        Nothing -> (Vec3 0 0 0, rand)
    Nothing ->
        let unitDirection = unitVector (direction r)
            t             = 0.5 * (vecY unitDirection + 1.0)

            v1            = Vec3 1.0 1.0 1.0
            v2            = Vec3 0.5 0.7 1.0
            v3            = ((1.0 - t) `mult` v1) + (t `mult` v2)
        in  (v3, rand)




maxNonInfiniteFloat :: Double -> Double
maxNonInfiniteFloat a = encodeFloat m n
  where
    b       = floatRadix a
    e       = floatDigits a
    (_, e') = floatRange a
    m       = b ^ e - 1
    n       = e' - e

lowerLeftCorner :: Vec3
lowerLeftCorner = Vec3 (-2.0) (-1.0) (-1.0)

horizontalV :: Vec3
horizontalV = Vec3 4.0 0.0 0.0

verticalV :: Vec3
verticalV = Vec3 0.0 2.0 0.0

originV :: Vec3
originV = Vec3 0.0 0.0 0.0

world :: [Sphere]
world =
    [ Sphere (Vec3 0 0 -1)      0.5 (Lambertian (Vec3 0.8 0.3 0.3))
    , Sphere (Vec3 0 -100.5 -1) 100 (Lambertian (Vec3 0.8 0.8 0))
    , Sphere (Vec3 1 0 -1)      0.5 (Metal (Vec3 0.8 0.6 0.2) 0.3)
    , Sphere (Vec3 -1 0 -1)     0.5 (Metal (Vec3 0.8 0.8 0.8) 1.0)
    ]



main :: IO ()
main = do
    rand <- newPureMT

    let content =
            fromLazyText "P3\n"
                <> decimal nx
                <> sp
                <> decimal ny
                <> fromText "\n255\n"
                <> valueContent (values defaultCamera rand)


        sp = singleton ' '

        valueContent :: [[Color]] -> Builder
        valueContent = mconcat . map lineToBuilder

        lineToBuilder :: [Color] -> Builder
        lineToBuilder = mconcat . map colorToBuilder


    T.putStrLn (toLazyText content)
