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
import           Data.Vec4
import           Utils

import           System.Random.SplitMix


data ScatterRecord = ScatterRecord {
    srAttenuation :: Vec4
    , srScattered :: Ray
}


class MaterialC a where
    scatter :: SMGen -> Ray -> HitRecord -> a -> Maybe (ScatterRecord, SMGen)


data Material = forall a. MaterialC a => Material a


instance MaterialC Material where
    scatter rand r rec (Material x) = scatter rand r rec x


-- data NoMaterial = NoMaterial

-- instance MaterialC NoMaterial where
--     scatter _ _ _ NoMaterial = Nothing



data MaterialLambertian = Lambertian { lambAlbedo :: !Vec4 {-, lambRoughness :: !Double-} }

instance MaterialC MaterialLambertian where
    -- scatter rand rIn hitRecord Lambertian {..} =
    --     let norm = htNormal hitRecord
    --         dir = direction rIn
    --         !cos = saturate $ dot (unitVector norm) (unitVector (negate dir))
    --         (v1, ra1) = randomInUnitSphere rand
    --         (v2, ra2) = randomDouble ra1
    --         (v3, ra3) = randomInUnitSphere ra2
    --         fresnel = schlick cos 0.04 
    --         bounce = if v2 > fresnel then norm + v1 else 
    --             reflect dir norm + lambRoughness `mult` v3
    --         scatterRec = ScatterRecord lambAlbedo bounce
    --     in  Just (scatterRec, ra3)
    scatter rand _rIn hitRecord Lambertian {..} =
        let target = htP hitRecord + htNormal hitRecord + v1
            (v1, ra1) = randomInUnitSphere rand
            r = Ray (htP hitRecord) (target - htP hitRecord)
            scatterRec = ScatterRecord lambAlbedo r
        in  Just (scatterRec, ra1)

    

data MaterialMetal = Metal {metalAlbedo :: !Vec4, metalFuzz :: !Float }        

instance MaterialC MaterialMetal where
    -- scatter rand r ht Metal {..} = 
    --     let reflected = reflect (unitVector (direction r)) (htNormal ht)
    --         (v1, rand1) = randomInUnitSphere rand
    --         scattered = Ray (htP ht) (reflected + metalFuzz `mult` v1)
    --         attenuation = metalAlbedo
    --     in
    --         if dot (direction scattered) (htNormal ht) > 0 
    --             then Just (attenuation, scattered, rand1)
    --             -- else Nothing
    --             else Just (attenuation, scattered, rand1)
    scatter rand rIn hitRecord Metal {..} = 
        let reflected = reflect (norm (direction rIn)) (htNormal hitRecord)
            --(v1, rand1) = randomInUnitSphere rand
            scattered = Ray (htP hitRecord) (reflected {-+ metalFuzz `mult` v1-})
        in
            if dotp (direction scattered) (htNormal hitRecord) > 0 
                then Just ((ScatterRecord metalAlbedo scattered), rand)
                else Nothing
                


reflect :: Vec4 -> Vec4 -> Vec4
reflect !v !n = v - 2 * dotp v n `mulScalar` n


refract :: Vec4 -> Vec4 -> Float -> Maybe Vec4
refract !v !n !ni_over_nt =
    let uv = norm v
        dt = dotp uv n
        discriminant = 1.0 - ni_over_nt * ni_over_nt * (1 - dt * dt)
    in
    if discriminant > 0 
        then Just (ni_over_nt `mulScalar` (uv - dt `mulScalar` uv) - sqrt discriminant `mulScalar` n)
        else Nothing


-- data MaterialDielectric = Dielectric { diecRefIdx :: Double }

-- instance MaterialC MaterialDielectric where
--     scatter rand r rec Dielectric {..} = 
--         let reflected = reflect (direction r) (htNormal rec)
--             attenuation = Vec3 1 1 1
--             dtp = dot (direction r) (htNormal rec) / Vector.length (direction r)
--             (!outwardNormal, !niOverNt, !cosine) = if dot (direction r) (htNormal rec) > 0
--                 then (negate (htNormal rec), diecRefIdx, diecRefIdx * dtp)
--                 else (htNormal rec, 1.0 / diecRefIdx, negate dtp)
--             refracted' = refract (direction r) outwardNormal niOverNt
--             (reflectProb, refracted) = case refracted' of 
--                 Just r -> (schlick cosine diecRefIdx, r)
--                 Nothing -> (2.0, Vec3 0 0 0)
--             (v1, rand1) = randomDouble rand
--         in
--             if v1 < reflectProb 
--                 then Just (attenuation, Ray (htP rec) reflected, rand1)
--                 else Just (attenuation, Ray (htP rec) refracted, rand1)



schlick :: Double -> Double -> Double
schlick !cosine !refIdx =
    let r0 = (1 - refIdx) / (1 + refIdx)
        r1 = r0 * r0
    in
    r1 + (1 - r1) * (1 - cosine) ^ 5