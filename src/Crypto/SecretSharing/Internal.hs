{-# LANGUAGE DataKinds, DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.SecretSharing.Internal
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
--
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Crypto.SecretSharing.Internal
where

import Data.ByteString.Lazy( ByteString )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.List as L
import Data.Char
import Data.Vector( Vector )
import qualified Data.Vector as V
import Data.Typeable
import Control.Exception
import Control.Monad
import Data.Binary( Binary )
import GHC.Generics
import Data.FiniteField.PrimeField as PF
import Data.FiniteField.Base(FiniteField,order)
import System.Random.Dice

-- | Evaluate a Lagrange interpolation polynomial
-- passing through the specified set of points.
polyInterp :: Fractional a => [(a, a)] -> a -> a
polyInterp xys x = sum $ map evalBasisPoly $ slidingFocus xys
  where
    evalBasisPoly (left, (xj, yj), right) =
      yj * product (map (\(xm, _) -> (x - xm) / (xj - xm)) (left ++ right))

-- [1,2,3] -> [([], 1, [2,3]), ([1], 2, [3]), ([2,1], 3, [])]
slidingFocus :: [a] -> [([a], a, [a])]
slidingFocus [] = []
slidingFocus (x : xs) = go [] x xs
  where
    go left focus right = (left, focus, right) : case right of
      [] -> []
      focus' : right' -> go (focus : left) focus' right'

-- | A share of an encoded byte.
data ByteShare = ByteShare
  { shareId :: !Int                  -- ^ the index of this share
  , reconstructionThreshold :: !Int  -- ^ number of shares required for
                                     -- reconstruction
  , shareValue :: !Int        -- ^ the value of p(shareId) where p(x) is the
                              --   generated (secret) polynomial
  }
  deriving(Typeable,Eq,Generic)

instance Show ByteShare where
  show = show . shareValue

-- | A share of the encoded secret.
data Share = Share
  { theShare :: ![ByteShare] }
  deriving(Typeable,Eq,Generic)

instance Show Share where
  show s = show (shareId $ head $ theShare s,BLC.pack $ map (chr . shareValue) $ theShare s)

instance Binary ByteShare
instance Binary Share

-- | Encodes a 'ByteString' as a list of n shares, m of which are required for
-- reconstruction.
-- Lives in the 'IO' to access a random source.
encode :: Int         -- ^ m
       -> Int         -- ^ n
       -> ByteString  -- ^ the secret that we want to share
       -> IO [Share] -- a list of n-shares (per byte)
encode m n bstr
  | n >= prime || m > n = throw $ AssertionFailed $
      "encode: require n < " ++ show prime ++ " and m<=n."
  | BL.null bstr = return []
  | otherwise = do
  let len = max 1 ((fromIntegral $ BL.length bstr) * (m-1))
  coeffs <- (groupInto (m-1) . map fromIntegral . take len )
                            `liftM` (getDiceRolls prime len)
  let byteVecs = zipWith (encodeByte m n) coeffs $
                    map fromIntegral $ BL.unpack bstr
  return [ Share $ map (V.! (i-1)) byteVecs | i <- [1..n] ]


-- | Reconstructs a (secret) bytestring from a list of (at least @m@) shares.
-- Throws 'AssertionFailed' if the number of shares is too small.
decode :: [Share]    -- ^ list of at least @m@ shares
       -> ByteString -- ^ reconstructed secret
decode []     = BL.pack []
decode shares@((Share s):_)
  | length shares < reconstructionThreshold (head s) = throw $ AssertionFailed
      "decode: not enough shares for reconstruction."
  | otherwise =
    let origLength = length s in
    let byteVecs = map (V.fromList . theShare) shares in
    let byteShares = [ map ((V.! (i-1))) byteVecs | i <- [1..origLength] ] in
    BL.pack . map (fromInteger . PF.toInteger . number)
            . map decodeByte $ byteShares


encodeByte :: Int -> Int -> Polyn -> FField -> Vector ByteShare
encodeByte m n coeffs secret =
  V.fromList[ ByteShare i m $ fromInteger . PF.toInteger . number $
                evalPolynomial (secret:coeffs) (fromIntegral i::FField)
            | i <- [1..n]
            ]


decodeByte :: [ByteShare] -> FField
decodeByte ss =
  let m = reconstructionThreshold $ head ss in
  if length ss < m
    then throw $ AssertionFailed "decodeByte: insufficient number of shares for reconstruction!"
    else
      let shares = take m ss
          pts = map (\s -> (fromIntegral $ shareId s,fromIntegral $ shareValue s))
                    shares
      in
      polyInterp pts 0


-- | Groups a list into blocks of certain size. Running time: /O(n)/
groupInto :: Int -> [a] -> [[a]]
groupInto num as
  | num < 0  = throw $ AssertionFailed "groupInto: Need positive number as argument."
  | otherwise =
    let (fs,ss) = L.splitAt num as in
    if L.null ss
      then [fs]
      else fs : groupInto num ss


-- | A finite prime field. All computations are performed in this field.
newtype FField = FField { number :: $(primeField $ fromIntegral 1021) }
  deriving(Show,Read,Ord,Eq,Num,Fractional,Generic,Typeable,FiniteField)


-- | The size of the finite field
prime :: Int
prime = fromInteger $ order (0 :: FField)


-- | A polynomial over the finite field given as a list of coefficients.
type Polyn = [FField]

-- | Evaluates the polynomial at a given point.
evalPolynomial :: Polyn -> FField -> FField
evalPolynomial coeffs x =
  foldr (\c res -> c + (x * res)) 0 coeffs
--  let clist = zipWith (\pow c -> (\x -> c * (x^pow))) [0..] coeffs
--  in L.foldl' (+) 0 [ c x | c <- clist ]

