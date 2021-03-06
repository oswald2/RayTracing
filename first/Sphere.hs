{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module Sphere where


import Vector
import Hitable
import Ray
import Material
import HitRecord


data Sphere = Sphere {
    sphCenter :: !Vec3,
    sphRadius :: !Double,
    sphMaterial :: Material
}


instance Hitable Sphere where
    hit sphere@(Sphere center radius material) r t_min t_max =
        let oc = origin r - sphCenter sphere
            dirr         = direction r
            a            = dot dirr dirr
            b            = dot oc dirr
            c            = dot oc oc - radius * radius
            discriminant = b * b - a * c
        in  
        if discriminant > 0 
            then 
                let temp = (-b - sqrt discriminant) / a 
                    temp2 = (-b + sqrt discriminant) / a
                    ht t = HitRecord t (p t) (norm t)
                    p t = pointAtParameter r t
                    norm t = (p t - center) `divide` radius
                in
                if temp < t_max && temp > t_min 
                    then Just ((ht temp), material) 
                    else
                        if temp2 < t_max && temp2 > t_min 
                            then Just ((ht temp2), material)
                            else Nothing
            else
                Nothing


instance HasMaterial Sphere where
    material = sphMaterial