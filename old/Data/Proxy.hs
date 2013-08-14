{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE StandaloneDeriving #-}
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Proxy
-- Copyright  : 2009-2013 Edward Kmett
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------
module Data.Proxy
    (
    -- * Proxy values
      Proxy(..)
    , reproxy
    , asProxyTypeOf
    ) where

import Control.Applicative (Applicative(..))
import Data.Traversable (Traversable(..))
import Data.Foldable (Foldable(..))
import Data.Ix (Ix(..))
import Data.Monoid
#ifdef __GLASGOW_HASKELL__
import GHC.Arr (unsafeIndex, unsafeRangeSize)
import Data.Data
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
deriving instance Typeable Proxy
#else
data Proxy s = Proxy
#endif

instance Eq (Proxy s) where
  _ == _ = True

instance Ord (Proxy s) where
  compare _ _ = EQ

instance Show (Proxy s) where
  showsPrec _ _ = showString "Proxy"

instance Read (Proxy s) where
  readsPrec d = readParen (d > 10) (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 707
instance Typeable1 Proxy where
  typeOf1 _ = mkTyConApp proxyTyCon []

proxyTyCon :: TyCon
#if __GLASGOW_HASKELL__ < 704
proxyTyCon = mkTyCon "Data.Proxy.Proxy"
#else
proxyTyCon = mkTyCon3 "tagged" "Data.Proxy" "Proxy"
#endif
{-# NOINLINE proxyTyCon #-}
#endif

instance Data s => Data (Proxy s) where
  gfoldl _ z _ = z Proxy
  toConstr _ = proxyConstr
  gunfold _ z c = case constrIndex c of
    1 -> z Proxy
    _ -> error "gunfold"
  dataTypeOf _ = proxyDataType
  dataCast1 f = gcast1 f

proxyConstr :: Constr
proxyConstr = mkConstr proxyDataType "Proxy" [] Prefix
{-# NOINLINE proxyConstr #-}

proxyDataType :: DataType
proxyDataType = mkDataType "Data.Proxy.Proxy" [proxyConstr]
{-# NOINLINE proxyDataType #-}
#endif

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

-- | Some times you need to change the proxy you have lying around.
-- Idiomatic usage is to make a new combinator for the relationship
-- between the proxies that you want to enforce, and define that
-- combinator using 'reproxy'.
--
-- > data Succ n
-- > reproxySucc :: proxy n -> Proxy (Succ n)
-- > reproxySucc = reproxy
reproxy :: proxy s -> Proxy t
reproxy _ = Proxy
{-# INLINE reproxy #-}

-- | 'asProxyTypeOf' is a type-restricted version of 'const'.
-- It is usually used as an infix operator, and its typing forces its first
-- argument (which is usually overloaded) to have the same type as the tag
-- of the second.
asProxyTypeOf :: a -> proxy a -> a
asProxyTypeOf = const
{-# INLINE asProxyTypeOf #-}
