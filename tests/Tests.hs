module Main
where
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.Maybe
import qualified Data.List as L
import Control.Monad

import Data.ByteString.Lazy( ByteString )
import qualified Data.ByteString.Lazy as B

import Crypto.SecretSharing.Internal

instance Arbitrary ByteString where
    arbitrary   = fmap B.pack arbitrary

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "encodingDecoding" propEncodingDecoding
       ] mempty

data ShareIdxs = ShareIdxs { shareIdxs :: [Int] }
  deriving (Show,Eq,Ord)
 
instance Arbitrary ShareIdxs where
  arbitrary = 
    liftM (ShareIdxs . L.nub) $ sequence [ choose (1,prime-1) 
                                         | i <- [1..(prime-1) `div` 2]]

propEncodingDecoding bstr idxs = ioProperty $ do
  shares <- encode 20 (prime-1) bstr 
  if null shares && B.null bstr
    then return True
    else do
      let chosen = [ shares !! (i-1) | i <- shareIdxs idxs ]  
      return (bstr == (decode chosen)) 

