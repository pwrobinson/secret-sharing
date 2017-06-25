-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.SecretSharing
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
-- 
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Implementation of an (@m@,@n@)-threshold secret sharing scheme.
-- A given ByteString @b@ (the secret) is split into @n@ shares, 
-- and any @m@ shares are sufficient to reconstruct @b@.
-- The scheme preserves perfect secrecy in the sense that the knowledge of up
-- to @m-1@ shares does not reveal any information about the secret @b@.
--
-- Typically, there are @n@ parties and we would like to give the @i@-th party
-- the @i@-share of each byte. 
-- For example, to encode a bytestring @secret@ as @10@ shares, any @5@ of which
-- are sufficient for reconstruction we could write:
--
-- > shares <- encode 5 10 secret
--
-- Note that each byte is encoded separately using a fresh set of random
-- coefficients.
--
-- The mathematics behind the secret sharing scheme is described in:
-- \"How to share a secret.\" by Shamir, Adi.
-- In Communications of the ACM 22 (11): 612–613, 1979.
-- 
--
-----------------------------------------------------------------------------

module Crypto.SecretSharing( encode, decode, Share )
where
import Crypto.SecretSharing.Internal
