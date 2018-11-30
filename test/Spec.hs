
import Test.QuickCheck
import TestSecretSplit
import Crypto.PubKey.ECC.Types

main :: IO Result
main = do
    _ <- quickCheckResult $ prop_split_reconstruct $ getCurveByName SEC_p256k1
    quickCheckResult prop_compress_decompress
