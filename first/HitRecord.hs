module HitRecord
where



import Data.Vec4



data HitRecord = HitRecord {
    htT :: !Float,
    htP :: !Vec4,
    htNormal :: !Vec4
}

