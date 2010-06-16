{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Tagged
-- Copyright  : 2009 Edward Kmett
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------

module Data.Tagged
    ( 
    -- * Tagged values
      Tagged(..)
    , retag
    , untag
    , tagSelf
    , untagSelf
    , asTaggedTypeOf
    -- * Proxy values
    , Proxy(..)
    , reproxy
    , asProxyTypeOf
    -- * Conversion
    , proxy
    , unproxy
    ) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Monad (liftM)
import Data.Traversable (Traversable(..))
import Data.Foldable (Foldable(..))
import Data.Monoid (Monoid(..))
import Data.Default (Default(..))
import Data.Data (Data,Typeable)
import Data.Ix (Ix)
import Text.Read

-- | A @'Tagged' s b@ value is a value @b@ with an attached phantom type @s@.
-- This can be used in place of the more traditional but less safe idiom of
-- passing in an undefined value with the type, because unlike an @(s -> b)@, 
-- a @'Tagged' s b@ can't try to use the argument @s@ as a real value.
--
-- Moreover, you don't have to rely on the compiler to inline away the extra
-- argument, because the newtype is "free"

newtype Tagged s b = Tagged { unTagged :: b } 
    deriving (Eq,Ord,Ix,Enum,Bounded,Data,Typeable,Num,Real,Integral,Fractional,Floating,RealFrac,RealFloat)

instance Show b => Show (Tagged s b) where
    showsPrec n (Tagged b) = showParen (n > 10) $
        showString "Tagged " .
        showsPrec 10 b

instance Read b => Read (Tagged s b) where
    readPrec = parens $ prec 10 $ do
        Ident "Tagged" <- lexP
        Tagged <$> step readPrec

instance Functor (Tagged s) where 
    fmap f (Tagged x) = Tagged (f x)
    {-# INLINE fmap #-}

instance Applicative (Tagged s) where
    pure = Tagged
    {-# INLINE pure #-}
    Tagged f <*> Tagged x = Tagged (f x)
    {-# INLINE (<*>) #-}

instance Monad (Tagged s) where
    return = Tagged
    {-# INLINE return #-}
    Tagged m >>= k = k m 
    {-# INLINE (>>=) #-}
    _ >> n = n
    {-# INLINE (>>) #-}

instance Foldable (Tagged s) where
    foldMap f (Tagged x) = f x
    {-# INLINE foldMap #-}
    fold (Tagged x) = x
    {-# INLINE fold #-}
    foldr f z (Tagged x) = f x z
    {-# INLINE foldr #-}
    foldl f z (Tagged x) = f z x
    {-# INLINE foldl #-}
    foldl1 _ (Tagged x) = x 
    {-# INLINE foldl1 #-}
    foldr1 _ (Tagged x) = x
    {-# INLINE foldr1 #-}

instance Traversable (Tagged s) where
    traverse f (Tagged x) = Tagged <$> f x
    {-# INLINE traverse #-}
    sequenceA (Tagged x) = Tagged <$> x
    {-# INLINE sequenceA #-}
    mapM f (Tagged x) = liftM Tagged (f x)
    {-# INLINE mapM #-}
    sequence (Tagged x) = liftM Tagged x
    {-# INLINE sequence #-}

-- | Some times you need to change the tag you have lying around.
-- Idiomatic usage is to make a new combinator for the relationship between the
-- tags that you want to enforce, and define that combinator using 'retag'.
--
-- > data Succ n
-- > retagSucc :: Tagged n a -> Tagged (Succ n) a
-- > retagSucc = retag
retag :: Tagged s b -> Tagged t b
retag = Tagged . unTagged 
{-# INLINE retag #-}

-- | Alias for 'unTagged'
untag :: Tagged s b -> b
untag = unTagged

-- | Tag a value with its own type.
tagSelf :: a -> Tagged a a
tagSelf = Tagged
{-# INLINE tagSelf #-}

-- | 'asTaggedTypeOf' is a type-restricted version of 'const'. It is usually used as an infix operator, and its typing forces its first argument (which is usually overloaded) to have the same type as the tag of the second.
asTaggedTypeOf :: s -> Tagged s b -> s
asTaggedTypeOf = const
{-# INLINE asTaggedTypeOf #-}

-- | 'selfUntag' 
untagSelf :: Tagged a a -> a
untagSelf (Tagged x) = x
{-# INLINE untagSelf #-}

data Proxy p = Proxy
    -- deriving (Eq,Ord,Ix,Show,Read)

{- 
Work around for the following GHC bug with deriving:

Data/Tagged.hs:1:0:
    Expecting an ordinary type, but found a type of kind * -> *
    In an expression type signature: Proxy
    In the expression: ghc-prim:GHC.Prim.tagToEnum# a :: Proxy
    In the definition of `Data.Tagged.$tag2con_Proxy':
        Data.Tagged.$tag2con_Proxy (ghc-prim:GHC.Types.I# a)
                                     = ghc-prim:GHC.Prim.tagToEnum# a :: Proxy

-}

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

reproxy :: Proxy s -> Proxy t
reproxy _ = Proxy
{-# INLINE reproxy #-}

proxy :: Tagged s a -> Proxy s -> a
proxy (Tagged x) _ = x
{-# INLINE proxy #-}

unproxy :: (Proxy s -> a) -> Tagged s a
unproxy f = Tagged (f Proxy)
{-# INLINE unproxy #-}

asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf = const
{-# INLINE asProxyTypeOf #-}
