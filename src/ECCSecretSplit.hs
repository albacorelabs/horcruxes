{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ECCSecretSplit
    ( split_hocrux,reconstruct_hocrux
    ) where

import Crypto.PubKey.ECC.Prim
import Crypto.PubKey.ECC.Types
import Crypto.Random.Types
import Data.List.Split (chunksOf)
import Data.Ratio 



split_hocrux :: MonadRandom m => Integer -> Integer -> Point -> m [(Integer,Point)]
split_hocrux m n sec_point = do
    let crv = getCurveByName SEC_p256k1
    coefficients <- traverse (const (scalarGenerate crv)) [1..(m -1 )]
    let polynomial = zip [1..] coefficients
    let points = (\xcoord coeff -> (xcoord ^ (fst coeff) * (snd coeff) ) ) <$> [1..n] <*> polynomial
    let pointify = pointBaseMul crv <$> points
    if (m -1) > 0 then do -- has to be a cleaner way to do this
        let chunked = chunksOf (fromInteger (m -1)) pointify
        return $ zip [1..] $ foldr (pointAdd crv) sec_point <$> chunked    
    else 
        return $ zip [1..] $ foldr (pointAdd crv) sec_point <$> [pointify]

reconstruct_hocrux :: [(Integer,Point)] -> Point
reconstruct_hocrux hocruxes = foldr (pointAdd crv) l lagrangeParts
    where
        crv = getCurveByName SEC_p256k1
        fxs = snd <$> hocruxes
        prodArr = prodList $ (fromIntegral . fst <$>) hocruxes
        (l:lagrangeParts) = zipWith (pointMul crv) (numerator <$> prodArr) $ fxs



prodList :: [Integer] -> [Ratio Integer]
prodList dbls = prodList' dbls (length dbls)

prodList' :: [Integer] -> Int -> [Ratio Integer]
prodList' _      0 = []
prodList' []     _ = []
prodList' dbls@(i:is) n =
    product ((\x  -> x % (x - i)) <$> filter (/= i) dbls) : prodList' (is ++ [i]) (n-1)
