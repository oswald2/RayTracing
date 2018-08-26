{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
    , RecordWildCards
    , ExistentialQuantification
#-}
module Material where

import           Ray
import           HitRecord
import           Vector
import           Utils

import           System.Random.Mersenne.Pure64


class MaterialC a where
    scatter :: PureMT -> Ray -> HitRecord -> a -> Maybe (Vec3, Ray, PureMT)


data Material = forall a. MaterialC a => Material a


instance MaterialC Material where
    scatter rand r rec (Material x) = scatter rand r rec x

-- data Material = 
--     NoMaterial
--     | Lambertian { lambAlbedo :: !Vec3 }
--     | Metal {metalAlbedo :: !Vec3, metalFuzz :: !Double }


data NoMaterial = NoMaterial

instance MaterialC NoMaterial where
    scatter _ _ _ NoMaterial = Nothing



data MaterialLambertian = Lambertian { lambAlbedo :: !Vec3 }

instance MaterialC MaterialLambertian where
    scatter rand _ ht Lambertian {..} =
        let (v1, ra1) = randomInUnitSphere rand
            target    = htP ht + htNormal ht + v1
        in  Just (lambAlbedo, Ray (htP ht) (target - htP ht), ra1)
    

data MaterialMetal = Metal {metalAlbedo :: !Vec3, metalFuzz :: !Double }        

instance MaterialC MaterialMetal where
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
reflect !v !n = v - 2 * dot v n `mult` n


refract :: Vec3 -> Vec3 -> Double -> Maybe Vec3
refract !v !n !ni_over_nt =
    let uv = unitVector v
        dt = dot uv n
        discriminant = 1.0 - ni_over_nt * ni_over_nt * (1 - dt * dt)
    in
    if discriminant > 0 
        then Just (ni_over_nt `mult` (uv - dt `mult` uv) - sqrt discriminant `mult` n)
        else Nothing


data MaterialDielectric = Dielectric { diecRefIdx :: Double }

instance MaterialC MaterialDielectric where
    scatter rand r rec Dielectric {..} = 
        let reflected = reflect (direction r) (htNormal rec)
            attenuation = Vec3 1 1 1
            dtp = dot (direction r) (htNormal rec) / Vector.length (direction r)
            (!outwardNormal, !niOverNt, !cosine) = if dot (direction r) (htNormal rec) > 0
                then (negate (htNormal rec), diecRefIdx, diecRefIdx * dtp)
                else (htNormal rec, 1.0 / diecRefIdx, negate dtp)
            refracted' = refract (direction r) outwardNormal niOverNt
            (reflectProb, refracted) = case refracted' of 
                Just r -> (schlick cosine diecRefIdx, r)
                Nothing -> (2.0, Vec3 0 0 0)
            (v1, rand1) = randomDouble rand
        in
            if v1 < reflectProb 
                then Just (attenuation, Ray (htP rec) reflected, rand1)
                else Just (attenuation, Ray (htP rec) refracted, rand1)



schlick :: Double -> Double -> Double
schlick !cosine !refIdx =
    let r0 = (1 - refIdx) / (1 + refIdx)
        r1 = r0 * r0
    in
    r1 + (1 - r1) * (1 - cosine) ^ 5