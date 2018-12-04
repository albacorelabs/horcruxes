{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ECCSecretSplit
    ( split_horcrux,reconstruct_horcrux,Horcrux,Horcruxes,crv,g
    ) where

import Crypto.PubKey.ECC.Prim
import Crypto.PubKey.ECC.Types
import Crypto.Random.Types
import Data.List.Split (chunksOf)
import Data.Ratio 

newtype Horcrux = Horcrux { openHorcrux :: (Integer,Point)} deriving (Show)
type Horcruxes = [Horcrux]

crv :: Curve
crv = getCurveByName SEC_p256k1

g :: Point
g = ecc_g $ common_curve crv

split_horcrux :: MonadRandom m => Integer -> Integer -> Point -> m Horcruxes
split_horcrux m n sec_point = do
    coefficients <- traverse (const (scalarGenerate crv)) [1..(m -1 )] -- randomise coefficients 
    let polynomial = zip [1..] coefficients
    let points = (\xcoord coeff -> (xcoord ^ (fst coeff) * (snd coeff) ) ) <$> [1..n] <*> polynomial
    let pointify = pointBaseMul crv <$> points
    let chunked = group_components m pointify
    return $ Horcrux <$> (zip [1..] $ foldr (pointAdd crv) sec_point <$> chunked)

reconstruct_horcrux :: Horcruxes -> Point
reconstruct_horcrux horcruxes = foldr (pointAdd crv) l lagrangeParts
    where
        crv = getCurveByName SEC_p256k1
        fxs = (snd . openHorcrux) <$> horcruxes
        prodArr = prodList $ (fromIntegral . fst . openHorcrux <$>) horcruxes
        (l:lagrangeParts) = zipWith (pointMul crv) (numerator <$> prodArr) $ fxs



prodList :: [Integer] -> [Ratio Integer]
prodList dbls = prodList' dbls (length dbls)

prodList' :: [Integer] -> Int -> [Ratio Integer]
prodList' _      0 = []
prodList' []     _ = []
prodList' dbls@(i:is) n =
    product ((\x  -> x % (x - i)) <$> filter (/= i) dbls) : prodList' (is ++ [i]) (n-1)


group_components :: Integer -> [(Point)] -> [[Point]]
group_components m pairs
    | (m-1) > 0 = chunksOf (fromInteger (m-1)) pairs
    | otherwise = [pairs]