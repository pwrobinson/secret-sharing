module Main
where
import Data.Monoid
import Test.Framework
--import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
--import Test.HUnit
import Test.QuickCheck
import Data.Maybe

import Data.ByteString( ByteString )
import qualified Data.ByteString as B
-- import Data.ByteString.Arbitrary
import Codec.IDA

instance Arbitrary ByteString where
    arbitrary   = fmap B.pack arbitrary

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "encodingDecoding" propEncodingDecoding
       ] mempty


-- propEncodingDecoding :: ArbByteString10M -> Bool
propEncodingDecoding bstr = 
  bstr == (decode $ encode 10 20 bstr)

