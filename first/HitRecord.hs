module HitRecord
where



import Vector



data HitRecord = HitRecord {
    htT :: !Double,
    htP :: !Vec3,
    htNormal :: !Vec3
}

