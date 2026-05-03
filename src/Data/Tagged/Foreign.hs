{-# LANGUAGE Safe #-}

{-# OPTIONS_GHC -Wno-deprecations #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Tagged
-- Copyright  : 2009-2015 Edward Kmett
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------

module Data.Tagged.Foreign where

import Data.Tagged(Tagged(..), untag)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import qualified Foreign.ForeignPtr as FP

withForeignPtr :: Tagged t (ForeignPtr a) -> (Tagged t (Ptr a) -> IO b) -> IO b
withForeignPtr fp f = FP.withForeignPtr (untag fp) (f . Tagged)
