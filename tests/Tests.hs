module Main
where
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.Maybe

import Data.ByteString.Lazy( ByteString )
import qualified Data.ByteString.Lazy as B

import Codec.PerfectSecretSharing 

instance Arbitrary ByteString where
    arbitrary   = fmap B.pack arbitrary

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "encodingDecoding" propEncodingDecoding
       ] mempty


propEncodingDecoding bstr = ioProperty $ do
  enc <- encode 10 20 bstr
  return $ bstr == decode enc

