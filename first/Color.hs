{-# LANGUAGE
    OverloadedStrings
#-}
module Color 

where

import Data.Word

import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int


data Color = Color !Word8 !Word8 !Word8


mkColor :: Double -> Double -> Double -> Color
mkColor red green blue = Color (truncate (red * 255.99))
                               (truncate (green * 255.99))
                               (truncate (blue * 255.99))

colorToBuilder :: Color -> Builder 
colorToBuilder (Color r g b) =
    let sp = singleton ' '
    in
    decimal r <> sp <> decimal g <> sp <> decimal b <> singleton '\n'
