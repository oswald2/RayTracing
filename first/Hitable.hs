{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module Hitable where



import Vector
import Ray
import HitRecord
import Material


class Hitable a where
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord
    material :: a -> Material

   