{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module Main where


import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy.IO             as T

import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int

import           Data.Traversable
import           Data.Word


import           Vector
import           Color
import           Ray
import           Hitable
import           HitableList
import           Sphere



nx = 200
ny = 100


values :: [[Color]]
values = map inner [ny - 1, ny - 2 .. 0]

inner :: Int -> [Color]
inner j = map (calc j) [0 .. nx - 1]

{-

-- example 1
calc :: Int -> Int -> Color
calc j i = mkColor (fromIntegral i / fromIntegral nx)
                   (fromIntegral j / fromIntegral ny)
                   0.2
-}

calc :: Int -> Int -> Color
calc j i =
    let
        u = fromIntegral i / fromIntegral nx
        v = fromIntegral j / fromIntegral ny
        r = Ray
            originV
            (lowerLeftCorner + u `mult` horizontalV + v `mult` verticalV)
    in
        color r world

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

-- example4
color :: Hitable a => Ray -> a -> Color
color r x =
    case hit world r 0.0 (maxNonInfiniteFloat 0) of
        Just ht ->
            let v = 0.5 `mult` Vec3 ((vecX (htNormal ht)) + 1)
                                    ((vecY (htNormal ht)) + 1)
                                    ((vecZ (htNormal ht)) + 1)
            in
                mkColor (vecX v) (vecY v) (vecZ v)
        Nothing -> 
            let unitDirection = unitVector (direction r)
                t             = 0.5 * (vecY unitDirection + 1.0)

                v1            = Vec3 1.0 1.0 1.0
                v2            = Vec3 0.5 0.7 1.0
                v3            = ((1.0 - t) `mult` v1) + (t `mult` v2)
            in  mkColor (vecX v3) (vecY v3) (vecZ v3)


maxNonInfiniteFloat :: Double -> Double
maxNonInfiniteFloat a = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e

lowerLeftCorner :: Vec3
lowerLeftCorner = Vec3 (-2.0) (-1.0) (-1.0)

horizontalV :: Vec3
horizontalV = Vec3 4.0 0.0 0.0

verticalV :: Vec3
verticalV = Vec3 0.0 2.0 0.0

originV :: Vec3
originV = Vec3 0.0 0.0 0.0

world :: [Sphere]
world = [
        Sphere (Vec3 0 0 -1) 0.5,
        Sphere (Vec3 0 -100.5 -1) 100
    ]



main :: IO ()
main = do
    let content =
            fromLazyText "P3\n"
                <> decimal nx
                <> sp
                <> decimal ny
                <> fromText "\n255\n"
                <> valueContent values


        sp = singleton ' '

        valueContent :: [[Color]] -> Builder
        valueContent = mconcat . map lineToBuilder

        lineToBuilder :: [Color] -> Builder
        lineToBuilder = mconcat . map colorToBuilder


    T.putStrLn (toLazyText content)
