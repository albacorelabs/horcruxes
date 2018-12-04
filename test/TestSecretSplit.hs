{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -fno-warn-orphans#-}

module TestSecretSplit where

import ECCSecretSplit
import HorcruxHelpers
import Crypto.PubKey.ECC.Generate
import Crypto.PubKey.ECC.Prim (scalarGenerate)
import Crypto.PubKey.ECC.Types
import Test.QuickCheck hiding (generate)
import Test.QuickCheck.Monadic


data Secret_Shards = Secret_Shards { 
    m :: Integer,
    n :: Integer
} deriving Show

instance Arbitrary Secret_Shards where
    arbitrary = do
        n <- arbitrary `suchThat` (> 1) 
        m <- choose (1, (n-1))
        return $ Secret_Shards m n

instance Arbitrary Point where
    arbitrary = do
        let crv = getCurveByName SEC_p256k1
        Positive i <- arbitrary
        return $ generateQ crv i
        

prop_compress_decompress :: Point -> Property
prop_compress_decompress p =  monadicIO $ do
    let crv = getCurveByName SEC_p256k1    
    recreated_p <- run $ ((decompress_horcrux crv) . compress_horcrux) p
    assert $ recreated_p == p
        
prop_split_reconstruct :: Curve -> Secret_Shards -> Property 
prop_split_reconstruct crv Secret_Shards{..} = monadicIO $ do
    secret <- run $ (return . generateQ) crv <*> (scalarGenerate crv)
    horcruxes <- run $ split_horcrux m n secret
    let recovered_secret = reconstruct_horcrux horcruxes
    assert $ recovered_secret == secret