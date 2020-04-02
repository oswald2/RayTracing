{-# LANGUAGE
    OverloadedStrings
#-}
module Color 

where

import Data.Word

import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int

import Data.Vec4


data Color = Color !Float !Float !Float


black :: Color
black = Color 0 0 0 

white :: Color
white = Color 1 1 1



mkColor :: Float -> Float -> Float -> Color
mkColor = Color 

-- mkColor red green blue = Color (truncate (red * 255.99))
--                                (truncate (green * 255.99))
--                                (truncate (blue * 255.99))

colorToBuilder :: Color -> Builder 
colorToBuilder (Color r g b) =
    let sp = singleton ' '
        to8 :: Float -> Word8
        to8 x = truncate (x * 255.9)
        ri = to8 r
        gi = to8 g
        bi = to8 b 
    in
    decimal ri <> sp <> decimal gi <> sp <> decimal bi <> singleton '\n'

vecToColor :: Vec4 -> Color
vecToColor v = 
    let (x, y, z, _) = unpack v 
    in mkColor x y z

(<<+>>) :: Color -> Color -> Color
Color r1 g1 b1 <<+>> Color r2 g2 b2 = Color (r1 + r2) (g1 + g2) (b1 + b2)

(<<*>>) :: Float -> Color -> Color
t <<*>> Color r g b = Color (t * r) (t * g) (t * b)

(<</>>) :: Color -> Float -> Color
Color r g b <</>> t = Color (r / t) (g / t) (b / t)


