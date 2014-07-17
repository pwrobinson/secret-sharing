{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
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
-- Implementation of an (m,n)-threshold scheme for secret sharing.
--
-----------------------------------------------------------------------------

module Codec.PerfectSecretSharing.Internal
where
import Math.Polynomial
import Math.Polynomial.Interpolation

import Data.ByteString( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Vector.Storable.ByteString -- converting byte strings to/from vectors
import Data.Vector.Storable( Vector, Storable, (!) )
import qualified Data.Vector as VGen
import qualified Data.Vector.Storable as V
import Data.Vector.Binary
import Data.Word
-- import Data.LargeWord
import Data.List
import Data.Char
import Data.Foldable( Foldable ) 
import qualified Data.Foldable as F
import Data.Bits
import Data.Maybe
import Data.Typeable
import Control.Monad
import Control.Arrow
import Control.Monad.Random

{-
 - TODO: move to secure-information-dispersal
import System.Entropy( getEntropy )
import qualified Codec.Crypto.AES as AES
-}

--fieldSize :: Int
--fieldSize = 2^128 

data Share = Share 
  { shareId     :: Int        -- ^ the index of this share 
  , reconstructionThreshold :: Int  -- ^ number of shares required for reconstruction
  , shareValue :: Word32        -- ^ the value of p(shareId) where p is the (secret) polynomial
  }
  deriving(Typeable,Show,Eq)


encode :: MonadRandom mon => Int -> Int -> ByteString -> mon [[Share]]
encode m n = groupIntoBytes 4 >>> mapM (encodeBS m n)
  where

--decode :: [[Share]] -> ByteString
decode =  catMaybes . map decodeBS 

testSS = do
  let bstr = B.pack $ take 1000000 $ [80..] 
  ss <- encode 10 20 bstr
  print $ decode ss
--  print (bstr == decode ss)


encodeBS :: MonadRandom mon => Int -> Int -> ByteString -> mon [Share]
encodeBS m n bstr = do
  coeffs :: [Word32] <- take (m-1) `liftM` getRandoms 
  let secret = byteStringToWord32 bstr
  let p = lagrangePolyFit $ zip [0..(fromIntegral m)] 
                                (map fromIntegral (secret : coeffs))
  return [ Share i m (round $ evalPoly p (fromIntegral i)) | i <- [1..n] ]

decodeBS :: [Share] -> Maybe ByteString
decodeBS ss 
  | null ss || length ss < reconstructionThreshold (head ss) = Nothing
  | otherwise = 
    let shares = take (reconstructionThreshold $ head ss) ss in 
    let p = lagrangePolyFit [ ((fromIntegral $ shareId s)::Double,fromIntegral $ shareValue s) 
                            | s <- shares ] in
    Just $ word32ToByteString $ round $ evalPoly p 0


groupIntoBytes :: Int -> ByteString -> [ByteString]
groupIntoBytes bnum as = 
  let (fs,ss) = B.splitAt bnum as in
  if B.null ss 
    then [fs]
    else fs : groupIntoBytes bnum ss


byteStringToWord32 :: ByteString -> Word32
byteStringToWord32 bstr 
  | B.length bstr > 4 = error "byteStringToWord32: ByteString must be of length <= 4"
  | otherwise = foldr (+) 0 . snd 
                           $ mapAccumL (\i b -> (i+1,shift (fromInteger $ toInteger b) (8*i))) 0 
                           $ B.unpack bstr

word32ToByteString :: Word32 -> ByteString
word32ToByteString = B.pack . intToBS 
  where intToBS 0 = []
        intToBS num = (fromIntegral (num .&. 127)) : (intToBS $ shift num (-8))
{-
byteStringToVector :: ByteString -> Int -> Vector Word32
byteStringToVector bstr n = 
  let times = floor $ logBase 8 (fromIntegral n) in
  groupInto times vecb
-}

{-
encode :: Int -> Int -> ByteString -> IO [Share]
encode m n msg = undefined

decode :: [Share] -> ByteString
decode = undefined
-}
