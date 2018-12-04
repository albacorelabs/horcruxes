module Main where

import ECCSecretSplit
import HorcruxHelpers
import Crypto.PubKey.ECC.Generate
import Crypto.PubKey.ECC.Prim



main :: IO Bool
main = do
    secret <- (return . generateQ) crv <*> (scalarGenerate crv)
    horcruxes <- split_horcrux 2 3 secret
    let rebuild_sec = reconstruct_horcrux $ take 2 horcruxes
    return $ compress_horcrux secret == compress_horcrux rebuild_sec