{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
    , RecordWildCards
#-}
module Material where

import           Ray
import           HitRecord
import           Vector
import           Utils

import           System.Random.Mersenne.Pure64


data Material = 
    Lambertian { lambAlbedo :: !Vec3 }
    | Metal {metalAlbedo :: !Vec3 }


scatter :: PureMT -> Ray -> HitRecord -> Material -> Maybe (Vec3, Ray, PureMT)
scatter rand r ht Lambertian {..} =
    let (v1, ra1) = randomInUnitSphere rand
        target    = htP ht + htNormal ht + v1
    in  Just (lambAlbedo, Ray (htP ht) (target - htP ht), ra1)
scatter rand r ht Metal {..} = 
    let reflected = reflect (unitVector (direction r)) (htNormal ht)
        scattered = Ray (htP ht) reflected
        attenuation = metalAlbedo
    in
        if dot (direction scattered) (htNormal ht) > 0 
            then Just (attenuation, scattered, rand)
            else Nothing


reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - 2 * dot v n `mult` n

