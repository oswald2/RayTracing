{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
    , ExistentialQuantification
#-}
module Hitable where



import Ray
import HitRecord
import Material


class Hitable a where
    hit :: a -> Ray -> Float -> Float -> Maybe (HitRecord, Material)

class HasMaterial a where
    material :: a -> Material


data HitObject = forall a. (Hitable a, HasMaterial a) => HitObject a 

instance Hitable HitObject where
    hit (HitObject a) = hit a

instance HasMaterial HitObject where
    material (HitObject a)  = material a
