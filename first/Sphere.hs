{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module Sphere where


import Data.Vec4
import Hitable
import Ray
import Material
import HitRecord


data Sphere = Sphere {
    sphCenter :: !Vec4,
    sphRadius :: !Float,
    sphMaterial :: Material
}


instance Hitable Sphere where
    hit sphere@(Sphere center radius material) r t_min t_max =
        let oc = origin r - sphCenter sphere
            dirr         = direction r
            a            = dotp dirr dirr
            b            = dotp oc dirr
            c            = dotp oc oc - radius * radius
            discriminant = b * b - a * c
        in  
        if discriminant > 0 
            then 
                let temp1 = (-b - sqrt discriminant) / a 
                    temp2 = (-b + sqrt discriminant) / a
                    ht t = HitRecord t (p t) (norm t)
                    p = pointAtParameter r
                    norm t = (p t - center) `divScalar` radius
                    b1 = t_min < temp1 && temp1 < t_max
                    b2 = t_min < temp2 && temp2 < t_max
                in
                if b1 || b2
                    then 
                        let t = if b1 then temp1 else temp2
                        in 
                            Just ((ht t), material) 
                    else
                        Nothing
            else
                Nothing


instance HasMaterial Sphere where
    material = sphMaterial