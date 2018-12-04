{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module HorcruxHelpers (compress_horcrux,decompress_horcrux) where
import qualified Crypto.Number.Generate as GenNum
import Crypto.Number.ModArithmetic
import Crypto.PubKey.ECC.Types
import Crypto.Random.Types
import Data.ByteString (ByteString,cons,drop,take)
import qualified Data.ByteString.Base16 as B16
import Data.Word
import Prelude hiding (last,head,tail,drop,take)
import Crypto.Number.Serialize (i2osp,os2ip)


compress_horcrux :: Point -> ByteString
compress_horcrux PointO = error "You're at the 0 point mate"
compress_horcrux (Point x y) 
    | y `mod` 2 == 0 = B16.encode $ cons (2 :: Word8) $ i2osp x
    | otherwise      = B16.encode $ cons (3 :: Word8) $ i2osp x


decompress_horcrux :: MonadRandom m => Curve -> ByteString -> m Point
decompress_horcrux (CurveF2m _) _ = error "Curve must be prime of type Fp"
decompress_horcrux (CurveFP  (CurvePrime p _)) bs = do
    let parityBit = take 2 bs
    let xCoord = (os2ip . fst . B16.decode) $ drop 2 bs
    root <- cipolla_sqrt p $ (xCoord^3 + 7) `mod` p
    if (parityBit == "02") then
        if (root `mod` 2 == 0 ) then
            return $ Point xCoord root
        else
            return $ Point xCoord $ (root * (-1)) `mod` p
    else
        if (root `mod` 2 /= 0 ) then
            return $ Point xCoord root
        else
            return $ Point xCoord $ (root * (-1)) `mod` p


cipolla_sqrt :: MonadRandom m => Integer -> Integer -> m Integer
cipolla_sqrt p n = do
    if check_congruency (expFast n ((p-1) `div` 2) p) 1 p then do -- IT is a square
        a <- find_valid_a n p
        let omegaSquared = (a ^2 -n) `mod` p
        let power = ((p+1) `div` 2) `mod` p
        let (collapseMul,_) = fold_powers power omegaSquared p (1,0) (a,1)
        return collapseMul
    else
        error "Your solution to y^2 isn't a square bro"


fold_powers :: Integer -> Integer -> Integer -> (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
fold_powers 0 _ _ x _ = x
fold_powers n omegaSquared p r s
    | n `mod` 2 == 1 = fold_powers (n `div` 2) omegaSquared p (cipolla_mul omegaSquared p r s) (cipolla_mul omegaSquared p s s)
    | otherwise = fold_powers (n `div` 2) omegaSquared p r $ cipolla_mul omegaSquared p s s


find_valid_a :: MonadRandom m => Integer -> Integer -> m Integer
find_valid_a n p = do
    a <- GenNum.generateMax p
    let base = expFast (a^2 - n) ((p-1) `div` 2) p
    if check_congruency base (-1) p then
        return a
    else
        find_valid_a n p

cipolla_mul :: Integer -> Integer -> (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
cipolla_mul omegaSquared p (a,b) (c,d) = (omega_sum ,i_sum)
    where
        omega_sum = (a * c + b * d * omegaSquared) `mod` p
        i_sum = (a * d + c * b ) `mod` p

check_congruency :: Integer -> Integer -> Integer -> Bool
check_congruency y x p 
    | (y - x) `mod` p == 0 = True
    | otherwise = False



