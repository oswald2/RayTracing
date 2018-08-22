{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module Sphere where


import Vector
import Hitable
import Ray



data Sphere = Sphere {
    sphCenter :: !Vec3,
    sphRadius :: !Double
}


instance Hitable Sphere where
    hit sphere@(Sphere center radius) r t_min t_max =
        let oc = origin r - sphCenter sphere
            dirr         = direction r
            a            = dot dirr dirr
            b            = 2.0 * dot oc dirr
            c            = dot oc oc - radius * radius
            discriminant = b * b - 4 * a * c
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
                    then Just (ht temp)
                    else
                        if temp2 < t_max && temp2 > t_min 
                            then Just (ht temp2)
                            else Nothing
            else
                Nothing

