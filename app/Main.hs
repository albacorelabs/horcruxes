module Main where

import ECCSecretSplit
import HocruxHelpers
import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.Generate
import Crypto.PubKey.ECC.Prim



main :: IO Bool
main = do
    let crv = getCurveByName SEC_p256k1
    secret <- (return . generateQ) crv <*> (scalarGenerate crv)
    hocruxes <- split_hocrux 2 3 secret
    let rebuild_sec = reconstruct_hocrux $ take 2 hocruxes
    return $ compress_hocrux secret == compress_hocrux rebuild_sec