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
    NoMaterial
    | Lambertian { lambAlbedo :: !Vec3 }
    | Metal {metalAlbedo :: !Vec3, metalFuzz :: !Double }


scatter :: PureMT -> Ray -> HitRecord -> Material -> Maybe (Vec3, Ray, PureMT)
scatter _ _ _ NoMaterial = Nothing
scatter rand _ ht Lambertian {..} =
    let (v1, ra1) = randomInUnitSphere rand
        target    = htP ht + htNormal ht + v1
    in  Just (lambAlbedo, Ray (htP ht) (target - htP ht), ra1)
scatter rand r ht Metal {..} = 
    let reflected = reflect (unitVector (direction r)) (htNormal ht)
        (v1, rand1) = randomInUnitSphere rand
        scattered = Ray (htP ht) (reflected + metalFuzz `mult` v1)
        attenuation = metalAlbedo
    in
        if dot (direction scattered) (htNormal ht) > 0 
            then Just (attenuation, scattered, rand1)
            else Nothing


reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - 2 * dot v n `mult` n


refract :: Vec3 -> Vec3 -> Double -> Maybe Vec3
refract v n ni_over_nt =
    let uv = unitVector v
        dt = dot uv n
        discriminant = 1.0 - ni_over_nt * ni_over_nt * (1 - dt * dt)
    in
    if discriminant > 0 
        then Just (ni_over_nt `mult` (uv - dt `mult` uv) - sqrt discriminant `mult` n)
        else Nothing

