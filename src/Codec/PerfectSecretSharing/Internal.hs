{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables   #-}
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
import qualified Data.ByteString.Lazy as B
import Data.Word
import qualified Data.List as L
import Data.Maybe
import Data.Typeable
import Control.Exception
import Control.Monad
import Control.Monad.Random


{-
import System.Entropy( getEntropy )
import qualified Codec.Crypto.AES as AES
-}

-- | A share of the encoded secret. 
data Share = Share 
  { shareId :: !Int                  -- ^ the index of this share 
  , reconstructionThreshold :: !Int  -- ^ number of shares required for reconstruction
  , shareValue :: !Word8        -- ^ the value of p(shareId) where p(x) is the 
                                --   generated (secret) polynomial
  }
  deriving(Typeable,Show,Eq)


encode :: MonadRandom mon 
       => Int         -- ^ m 
       -> Int         -- ^ n
       -> ByteString  -- ^ the secret that we want to encrypt
       -> mon [[Share]] -- a (bytewise) list of n-shares
encode m n bstr 
  | n > 254 || m > n = error "encode: n > 254 or m>n."
  | otherwise = do
  let bytes = B.unpack bstr
  coeffs :: [[Word8]] <- (groupInto (m-1) . take (length bytes * (m-1))) 
                         `liftM` getRandomRs (1,maxBound :: Word8)
  return $ map (uncurry $ encodeByte m n) $ zip coeffs bytes
--  return $ runPar $ parMap (uncurry $ encodeByte m n) $ zip coeffs bytes
--encode m n = groupIntoBytes 1 >>> 
--             mapM (encodeByte m n)

decode :: [[Share]] 
       -> ByteString
decode =  B.pack . catMaybes . map decodeByte 


test :: IO ()
test = do
  bstr :: ByteString <- (B.pack . take 100000) `liftM` getRandoms 
  ss <- encode 10 30 bstr
  print (bstr == decode ss)
  return ()


encodeByte :: Int -> Int -> [Word8] -> Word8 -> [Share]
encodeByte m n coeffs secret = 
--  let secret = head $ B.unpack bstr -- byteStringToWord32 bstr
  let p = lagrangePolyFit $ zip [0..(fromIntegral m :: Double)] 
                                (map fromIntegral (secret : coeffs)) in
  [ Share i m (round $ evalPoly p (fromIntegral i)) | i <- [1..n] ]

decodeByte :: [Share] -> Maybe Word8
decodeByte ss 
  | length ss < reconstructionThreshold (head ss) = Nothing
  | otherwise = 
    let shares = take (reconstructionThreshold $ head ss) ss in 
    let p = lagrangePolyFit [ ((fromIntegral $ shareId s),fromIntegral $ shareValue s :: Double) 
                            | s <- shares ] in
    Just $ fromIntegral $ (round $ evalPoly p 0 :: Integer)


{-
groupIntoBytes :: Int64 -> ByteString -> [ByteString]
groupIntoBytes bnum as = 
  let (fs,ss) = B.splitAt bnum as in
  if B.null ss 
    then [fs]
    else fs : groupIntoBytes bnum ss
-}

groupInto :: Int -> [a] -> [[a]]
groupInto num as
  | num <= 0  = throw $ AssertionFailed "groupInto: Need positive number as argument."
  | otherwise = 
    let (fs,ss) = L.splitAt num as in
    if L.null ss 
      then [fs]
      else fs : groupInto num ss

{-
byteStringToWord32 :: ByteString -> Word32
byteStringToWord32 bstr 
  | B.length bstr > 4 = error "byteStringToWord32: ByteString must be of length <= 4"
  | otherwise = foldr (+) 0 . snd 
                           $ L.mapAccumL (\i b -> (i+1,shift (fromInteger $ toInteger b) (8*i))) 0 
                           $ B.unpack bstr

word32ToByteString :: Word32 -> ByteString
word32ToByteString = B.pack . intToBS 
  where intToBS 0 = []
        intToBS num = (fromIntegral (num .&. 127)) : (intToBS $ shift num (-8))
-}


