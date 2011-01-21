{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Proxy
-- Copyright  : 2009-2011 Edward Kmett
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : generalized newtype deriving
--
-------------------------------------------------------------------------------

module Data.Proxy
    ( 
    -- * Tagged values
      Proxy(..)
    , reproxy
    , asProxyTypeOf
    -- * Conversion
    , proxy
    , unproxy
    ) where

import Control.Applicative (Applicative(..))
import Data.Traversable (Traversable(..))
import Data.Foldable (Foldable(..))
import Data.Monoid (Monoid(..))
import Data.Default (Default(..))
import Data.Data (Data,Typeable)
import Data.Ix (Ix(..))
import Data.Tagged
#ifdef __GLASGOW_HASKELL__
import GHC.Arr (unsafeIndex, unsafeRangeSize)
#endif

data Proxy p = Proxy deriving 
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data,Typeable
#endif
  )

instance Enum (Proxy s) where
    succ _ = error "Proxy.succ"
    pred _ = error "Proxy.pred"
    fromEnum _ = 0
    toEnum 0 = Proxy
    toEnum _ = error "Proxy.toEnum: 0 expected"
    enumFrom _ = [Proxy]
    enumFromThen _ _ = [Proxy]
    enumFromThenTo _ _ _ = [Proxy]
    enumFromTo _ _ = [Proxy]

{- 
Work around for the following GHC bug with deriving Ix instances with a phantom type:

Data/Tagged.hs:1:0:
    Expecting an ordinary type, but found a type of kind * -> *
    In an expression type signature: Proxy
    In the expression: ghc-prim:GHC.Prim.tagToEnum# a :: Proxy
    In the definition of `Data.Tagged.$tag2con_Proxy':
        Data.Tagged.$tag2con_Proxy (ghc-prim:GHC.Types.I# a)
                                     = ghc-prim:GHC.Prim.tagToEnum# a :: Proxy

-}

instance Ix (Proxy s) where
    range _ = [Proxy]
    index _ _ = 0
    inRange _ _ = True
    rangeSize _ = 1
#ifdef __GLASGOW_HASKELL__
    unsafeIndex _ _ = 0
    unsafeRangeSize _ = 1
#endif
    
instance Bounded (Proxy s) where
    minBound = Proxy
    maxBound = Proxy
    
instance Functor Proxy where
    fmap _ _ = Proxy
    {-# INLINE fmap #-}

instance Applicative Proxy where
    pure _ = Proxy
    {-# INLINE pure #-}
    _ <*> _ = Proxy
    {-# INLINE (<*>) #-}

instance Monoid (Proxy s) where
    mempty = Proxy
    {-# INLINE mempty #-}
    mappend _ _ = Proxy
    {-# INLINE mappend #-}
    mconcat _ = Proxy
    {-# INLINE mconcat #-}

instance Monad Proxy where
    return _ = Proxy
    {-# INLINE return #-}
    _ >>= _ = Proxy
    {-# INLINE (>>=) #-}

instance Foldable Proxy where
    foldMap _ _ = mempty
    {-# INLINE foldMap #-}
    fold _ = mempty
    {-# INLINE fold #-}
    foldr _ z _ = z
    {-# INLINE foldr #-}
    foldl _ z _ = z
    {-# INLINE foldl #-}
    foldl1 _ _ = error "foldl1: Proxy"
    {-# INLINE foldl1 #-}
    foldr1 _ _ = error "foldr1: Proxy"
    {-# INLINE foldr1 #-}

instance Traversable Proxy where
    traverse _ _ = pure Proxy
    {-# INLINE traverse #-}
    sequenceA _ = pure Proxy
    {-# INLINE sequenceA #-}
    mapM _ _ = return Proxy
    {-# INLINE mapM #-}
    sequence _ = return Proxy
    {-# INLINE sequence #-}

instance Default (Proxy s) where
    def = Proxy
    {-# INLINE def #-}

-- | Some times you need to change the tag you have lying around.
-- Idiomatic usage is to make a new combinator for the relationship 
-- between the tags that you want to enforce, and define that 
-- combinator using 'retag'.
--
-- > data Succ n
-- > reproxySucc :: Proxy n -> Proxy (Succ n)
-- > reproxySucc = reproxy
reproxy :: Proxy s -> Proxy t
reproxy _ = Proxy
{-# INLINE reproxy #-}

-- | Convert from a 'Tagged' representation to a representation 
-- based on a 'Proxy'.
proxy :: Tagged s a -> Proxy s -> a
proxy (Tagged x) _ = x
{-# INLINE proxy #-}

-- | Convert from a representation based on a 'Proxy' to a 'Tagged' 
-- representation.
unproxy :: (Proxy s -> a) -> Tagged s a
unproxy f = Tagged (f Proxy)
{-# INLINE unproxy #-}

-- | 'asProxyTypeOf' is a type-restricted version of 'const'. 
-- It is usually used as an infix operator, and its typing forces its first 
-- argument (which is usually overloaded) to have the same type as the tag 
-- of the second.
asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf = const
{-# INLINE asProxyTypeOf #-}
