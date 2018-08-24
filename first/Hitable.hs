{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module Hitable where



import Ray
import HitRecord
import Material


class Hitable a where
    hit :: a -> Ray -> Double -> Double -> Maybe (HitRecord, Material)

class HasMaterial a where
    material :: a -> Material
