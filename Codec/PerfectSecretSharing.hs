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
-- Implementation of an (m,n)-threshold scheme for secret sharing.
--
-----------------------------------------------------------------------------

module Codec.PerfectSecretSharing
where
import Codec.PerfectSecretSharing.Internal
