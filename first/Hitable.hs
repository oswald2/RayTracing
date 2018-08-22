{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module Hitable where



import Vector
import Ray


data HitRecord = HitRecord {
    htT :: !Double,
    htP :: !Vec3,
    htNormal :: !Vec3
}


class Hitable a where
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord
