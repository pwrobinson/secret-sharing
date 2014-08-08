{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, DeriveGeneric   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.PerfectSecretSharing.Internal
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
-- 
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  stable
-- Portability :  portable
-- 
-----------------------------------------------------------------------------

module Codec.PerfectSecretSharing.Internal
where
import Math.Polynomial
import Math.Polynomial.Interpolation

import Data.ByteString.Lazy( ByteString )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Word
import qualified Data.List as L
import Data.Maybe
import Data.Vector( Vector )
import qualified Data.Vector as V
import Data.Typeable
import Control.Exception
import Control.Monad
import Control.Monad.Random
import System.Entropy( getEntropy )
import Data.Binary( Binary )
import GHC.Generics


-- | A share of an encoded byte. 
data ByteShare = ByteShare 
  { shareId :: !Int                  -- ^ the index of this share 
  , reconstructionThreshold :: !Int  -- ^ number of shares required for reconstruction
  , shareValue :: !Word8        -- ^ the value of p(shareId) where p(x) is the 
                                --   generated (secret) polynomial
  }
  deriving(Typeable,Show,Eq,Generic)

-- | A share of the encoded (secret) 'ByteString'.
data Share = Share 
  { theShare :: ![ByteShare] }
  deriving(Typeable,Show,Eq,Generic)

instance Binary ByteShare
instance Binary Share

-- | Encodes a 'ByteString' as a list of n shares, m of which are required for
-- reconstruction.
-- Lives in the 'IO' monad to access a random source.
encode :: Int         -- ^ m 
       -> Int         -- ^ n
       -> ByteString  -- ^ the secret that we want to share
       -> IO [Share] -- a list of n-shares (per byte) 
encode m n bstr 
  | n > 254 || m > n = throw $ AssertionFailed "encode: require n <= 254 and m<=n."
  | BL.null bstr = return []
  | otherwise = do
  let bytes = BL.unpack bstr
  coeffs :: [[Word8]] <- (groupInto (m-1) . B.unpack)
                         `liftM` ((getEntropy $ (length bytes+1) * (m-1)) `catch` (\(e::SomeException) -> print "TODOOOOOOOO!!" >> throw e))
  let byteVecs = map (uncurry $ encodeByte m n) $ zip coeffs bytes
  return [ Share $ map (V.! i) byteVecs | i <- [1..n] ]

-- | Reconstructs a (secret) bytestring from a list of (at least m) shares. 
-- Throws 'AssertionFailed' if there are insufficient number of shares given.
decode :: [Share]
       -> ByteString
decode []     = BL.pack []
decode shares@((Share s):_) 
  | length shares < reconstructionThreshold (head s) = throw $ AssertionFailed 
      "decode: not enough shares for reconstruction."
  | otherwise =
    let origLength = length s in
    let byteVecs = map (V.fromList . theShare) shares in
    let byteShares = [ map ((V.! (i-1))) byteVecs | i <- [1..origLength] ] in
    BL.pack . catMaybes . map decodeByte $ byteShares


test :: IO ()
test = do
  bstr :: ByteString <- (BL.pack . take 100000) `liftM` getRandoms 
  ss <-  encode 10 30 bstr
  print (bstr == decode ss)
  return ()


encodeByte :: Int -> Int -> [Word8] -> Word8 -> Vector ByteShare
encodeByte m n coeffs secret = 
  let p = lagrangePolyFit $ zip [0..(fromIntegral m :: Double)] 
                                (map fromIntegral (secret : coeffs)) in
  V.fromList [ ByteShare i m (round $ evalPoly p (fromIntegral i)) | i <- [1..n] ]


decodeByte :: [ByteShare] -> Maybe Word8
decodeByte ss 
  | length ss < reconstructionThreshold (head ss) = Nothing
  | otherwise = 
    let shares = take (reconstructionThreshold $ head ss) ss in 
    let p = lagrangePolyFit [((fromIntegral $ shareId s),fromIntegral $
                                  shareValue s :: Double) 
                            | s <- shares ] in
    Just $ fromIntegral $ (round $ evalPoly p 0 :: Integer)



groupInto :: Int -> [a] -> [[a]]
groupInto num as
  | num <= 0  = throw $ AssertionFailed "groupInto: Need positive number as argument."
  | otherwise = 
    let (fs,ss) = L.splitAt num as in
    if L.null ss 
      then [fs]
      else fs : groupInto num ss

