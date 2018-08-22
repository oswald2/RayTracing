{-# LANGUAGE
    OverloadedStrings
    , BangPatterns
    , NegativeLiterals
#-}
module HitableList where



import Hitable


instance (Hitable a) => Hitable [a] where
    hit ls r t_min t_max = 
        let (res, _) = foldr calc (Nothing, t_max) ls
        in 
            res
        where
            calc :: Hitable a => a -> (Maybe HitRecord, Double) -> (Maybe HitRecord, Double)
            calc x p@(prevHt, closest_so_far) = 
                case hit x r t_min closest_so_far of
                    Just ht -> (Just ht, htT ht)
                    Nothing -> p