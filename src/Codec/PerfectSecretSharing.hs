{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.PerfectSecretSharing
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
-- 
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  stable
-- Portability :  portable
-- 
-- Implementation of an (m,n)-threshold secret sharing scheme.
-- A given ByteString 'b' (i.e. the secret) is split into n shares, 
-- and any m shares are sufficient to reconstruct 'b'.
-- The scheme was proven to be "perfect" in the sense that the knowledge of up
-- to m-1 shares does not reveal any information about the secret 'b'.
--
-- Note that each byte is encoded separately using a fresh set of random
-- coefficients.
--
-- The mathematics behind this is described here:
-- Shamir, Adi (1979), "How to share a secret".
-- Communications of the ACM 22 (11): 612–613.
-- 
--
-----------------------------------------------------------------------------

module Codec.PerfectSecretSharing( encode, decode, Share )
where
import Codec.PerfectSecretSharing.Internal
