{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
    , ExistentialQuantification
    , FlexibleInstances
#-}
module HitableList where



import           Hitable
import           HitRecord
import           Material


instance Hitable [HitObject] where
    hit ls r t_min t_max =
        let (res, _) = foldr calc (Nothing, t_max) ls
        in
            res
        where
            calc :: (Hitable a, HasMaterial a) => a -> (Maybe (HitRecord, Material), Double)
                -> (Maybe (HitRecord, Material), Double)
            calc x p@(_, closest_so_far) =
                case hit x r t_min closest_so_far of
                    Just rec@(ht, _) -> (Just rec, htT ht)
                    Nothing -> p
