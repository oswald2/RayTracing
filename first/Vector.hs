{-# LANGUAGE
    OverloadedStrings
#-}
module Vector 

where



data Vec3 = Vec3 !Double !Double !Double


vecX :: Vec3 -> Double
vecX (Vec3 x _ _) = x

vecY :: Vec3 -> Double
vecY (Vec3 _ y _) = y

vecZ :: Vec3 -> Double
vecZ (Vec3 _ _ z) = z


squared_length :: Vec3 -> Double
squared_length (Vec3 x y z) = x*x + y*y + z*z

length :: Vec3 -> Double
length v = sqrt (squared_length v)


makeUnitVector :: Vec3 -> Vec3 
makeUnitVector v@(Vec3 x y z) =
    let k = 1.0 / Vector.length v
    in
    Vec3 (x * k) (y * k) (z * k)


dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = 
    x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = 
    Vec3 (y1 * z2 - z1 * y2)
         (- x1 * z2 - z1 * x2)
         (x1 * y2 - y1 * x2)
    

mult :: Double -> Vec3 -> Vec3
mult t (Vec3 x y z) = Vec3 (t * x) (t * y) (t * z)

divide :: Vec3 -> Double -> Vec3
divide (Vec3 x y z) t = Vec3 (x / t) (y / t) (z / t)

unitVector :: Vec3 -> Vec3
unitVector v@(Vec3 x y z) = v `divide` Vector.length v


instance Num Vec3 where
    negate (Vec3 x y z) = Vec3 (negate x) (negate y) (negate z)
    (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
    (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
    (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)

    abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)

    signum (Vec3 x y z) = 
        if (x == 0) && (y == 0) && (z == 0) then 0 else 1

    fromInteger x = Vec3 (fromIntegral x) 0 0