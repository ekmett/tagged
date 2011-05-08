{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Tagged
-- Copyright  : 2009-2011 Edward Kmett
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : generalized newtype deriving
--
-------------------------------------------------------------------------------

module Data.Tagged
    ( 
    -- * Tagged values
      Tagged
    , TaggedT(..)
    , retag
    , untag
    , tagSelf
    , untagSelf
    , asTaggedTypeOf
    ) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Monad (liftM)
import Data.Traversable (Traversable(..))
import Data.Foldable (Foldable(..))
import Data.Data (Data,Typeable)
import Data.Ix (Ix(..))
-- import Data.Functor.Identity
import Text.Read

-- | A @'Tagged' s b@ value is a value @b@ with an attached phantom type @s@.
-- This can be used in place of the more traditional but less safe idiom of
-- passing in an undefined value with the type, because unlike an @(s -> b)@, 
-- a @'Tagged' s b@ can't try to use the argument @s@ as a real value.
--
-- Moreover, you don't have to rely on the compiler to inline away the extra
-- argument, because the newtype is "free"

newtype TaggedT s m b = TaggedT { unTagged :: m b } deriving 
  ( Eq, Ord, Ix, Enum, Bounded 
-- , Num, Real, Integral, Fractional, Floating, RealFrac, RealFloat
-- #ifdef LANGUAGE_DeriveDataTypeable
-- , Data, Typeable
-- #endif
  )

instance Show b => Show (Tagged s b) where
  showsPrec n (Tagged b) = showParen (n > 10) $
    showString "Tagged " .
    showsPrec 11 b

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
retag :: TaggedT s m b -> TaggedT t m b
retag = TaggedT . unTaggedT
{-# INLINE retag #-}

-- | Alias for 'unTagged'
untag :: TaggedT s m b -> b
untag = unTagged

-- | Tag a value with its own type.
tagSelf :: a -> Tagged a a
tagSelf = tagged
{-# INLINE tagSelf #-}

-- | 'asTaggedTypeOf' is a type-restricted version of 'const'. It is usually used as an infix operator, and its typing forces its first argument (which is usually overloaded) to have the same type as the tag of the second.
asTaggedTypeOf :: s -> TaggedT s m b -> s
asTaggedTypeOf = const
{-# INLINE asTaggedTypeOf #-}

-- | 'untagSelf' is a type-restricted version of 'untag'.
untagSelf :: Tagged a a -> a
untagSelf (Tagged x) = x
{-# INLINE untagSelf #-}

instance Semigroup a => Semigroup (Tagged s a) where
  Tagged a <> Tagged b = Tagged (a <> b)

