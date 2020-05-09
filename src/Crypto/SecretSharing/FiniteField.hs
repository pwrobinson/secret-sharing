{-# LANGUAGE DataKinds, DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving, TemplateHaskell #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.SecretSharing.FiniteField
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
--
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Crypto.SecretSharing.FiniteField
where

import Data.Typeable
import GHC.Generics
import Data.FiniteField.PrimeField as PF
import Crypto.SecretSharing.Prime


-- | A finite prime field. All computations are performed in this field.
newtype FField = FField { number :: $(primeField $ fromIntegral prime) }
  deriving(Show,Read,Ord,Eq,Num,Fractional,Generic,Typeable)


-- | A polynomial over the finite field given as a list of coefficients.
type Polyn = [FField]

-- | Evaluates the polynomial at a given point.
evalPolynomial :: Polyn -> FField -> FField
evalPolynomial coeffs x
  = foldr (\c res -> c + (x * res)) 0 coeffs
