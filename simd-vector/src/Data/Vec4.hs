{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -mavx #-}
{-# OPTIONS_GHC -msse #-}
{-# OPTIONS_GHC -msse2 #-}
{-# OPTIONS_GHC -msse4 #-}
module Data.Vec4
  ( Vec4
  , vec4
  , mulScalar
  , divScalar
  , crossp
  , magn2
  , magn
  , dotp
  , norm
  , Random
  , randomR
  , random
  , unpack
  )
where


import           GHC.Exts
import           GHC.Prim

import           System.Random                  ( Random
                                                , randomR
                                                , random
                                                )
import           System.Random.SplitMix
import           Control.Monad.State.Strict     ( runState
                                                , state
                                                )

data Vec4 = Vec4# FloatX4#

instance Show Vec4 where
  show (Vec4# f) = case unpackFloatX4# f of
    (# a, b, c, d #) -> show (F# a, F# b, F# c, F# d)

instance Random Vec4 where
  randomR ((Vec4# vecLo), (Vec4# vecHi)) =
    let (# a1, a2, a3, _a4 #) = unpackFloatX4# vecLo
        (# b1, b2, b3, _b4 #) = unpackFloatX4# vecHi
        loX                   = F# a1
        loY                   = F# a2
        loZ                   = F# a3
        hiX                   = F# b1
        hiY                   = F# b2
        hiZ                   = F# b3
        x                     = state (randomR (loX, hiX))
        y                     = state (randomR (loY, hiY))
        z                     = state (randomR (loZ, hiZ))
    in  runState (vec4 <$> x <*> y <*> z <*> pure 0)

  random = runState
    (vec4 <$> state random <*> state random <*> state random <*> pure 0)

{-# INLINE vec4 #-}
vec4 :: Float -> Float -> Float -> Float -> Vec4
vec4 (F# v1) (F# v2) (F# v3) (F# v4) =
  Vec4# (packFloatX4# (# v1, v2, v3, v4 #))


{-# INLINE nullVec4 #-}
nullVec4 :: Vec4
nullVec4 = vec4 0 0 0 0


{-# INLINE unpack #-}
unpack :: Vec4 -> (Float, Float, Float, Float)
unpack (Vec4# f) =
  let (# a1, a2, a3, a4 #) = unpackFloatX4# f in (F# a1, F# a2, F# a3, F# a4)


{-# INLINE mulScalar #-}
mulScalar :: Float -> Vec4 -> Vec4
mulScalar (F# x) (Vec4# f) =
  let x1 = broadcastFloatX4# x in Vec4# (timesFloatX4# x1 f)

{-# INLINE divScalar #-}
divScalar :: Vec4 -> Float -> Vec4
divScalar (Vec4# f) (F# x) =
  let x1 = broadcastFloatX4# x in Vec4# (divideFloatX4# f x1)

{-# INLINE dotp #-}
dotp :: Vec4 -> Vec4 -> Float
dotp (Vec4# f1) (Vec4# f2) = case unpackFloatX4# (timesFloatX4# f1 f2) of
  (# a, b, c, d #) -> F# (a `plusFloat#` b `plusFloat#` c `plusFloat#` d)

{-# INLINE magn2 #-}
magn2 :: Vec4 -> Float
magn2 (Vec4# f) = case unpackFloatX4# (timesFloatX4# f f) of
  (# a, b, c, d #) -> F# (a `plusFloat#` b `plusFloat#` c `plusFloat#` d)

{-# INLINE magn #-}
magn :: Vec4 -> Float
magn x = sqrt (magn2 x)

{-# INLINE crossp #-}
crossp :: Vec4 -> Vec4 -> Vec4
crossp (Vec4# f1) (Vec4# f2) =
  let (# a1, a2, a3, _a4 #) = unpackFloatX4# f1
      (# b1, b2, b3, _b4 #) = unpackFloatX4# f2
  in  Vec4#
        (packFloatX4#
          (# (a2 `timesFloat#` b3) `minusFloat#` (a3 `timesFloat#` b2)
          ,  (a3 `timesFloat#` b1) `minusFloat#` (a1 `timesFloat#` b3)
          ,  (a1 `timesFloat#` b2) `minusFloat#` (a2 `timesFloat#` b1)
          ,  0.0#
          #)
        )

{-# INLINE norm #-}
norm :: Vec4 -> Vec4
norm v = v `divScalar` magn v

instance Num Vec4 where
  {-# INLINE (+) #-}
  (Vec4# f1) + (Vec4# f2) = Vec4# (plusFloatX4# f1 f2)
  {-# INLINE (-) #-}
  (Vec4# f1) - (Vec4# f2) = Vec4# (minusFloatX4# f1 f2)
  {-# INLINE (*) #-}
  (Vec4# f1) * (Vec4# f2) = Vec4# (timesFloatX4# f1 f2)
  abs (Vec4# f) = case unpackFloatX4# f of
    (# a, b, c, d #) -> Vec4#
      (packFloatX4# (# fabsFloat# a, fabsFloat# b, fabsFloat# c, fabsFloat# d #)
      )
  negate (Vec4# f) = Vec4# (negateFloatX4# f)
  signum (Vec4# f) = undefined
  fromInteger x =
    let (F# d) = fromIntegral x in Vec4# (packFloatX4# (# d, 0.0#, 0.0#, 0.0# #))

