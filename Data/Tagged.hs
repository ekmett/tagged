{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
----------------------------------------------------------------------------
-- |
-- Module     : Data.Tagged
-- Copyright  : 2009-2012 Edward Kmett
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
    , witness
    ) where

import Control.Applicative ((<$>), liftA2, Applicative(..))
import Control.Monad (liftM)
import Data.Traversable (Traversable(..))
import Data.Foldable (Foldable(..))
#ifdef __GLASGOW_HASKELL__
import Data.Data
#endif
import Data.Ix (Ix(..))
import Data.Monoid

-- | A @'Tagged' s b@ value is a value @b@ with an attached phantom type @s@.
-- This can be used in place of the more traditional but less safe idiom of
-- passing in an undefined value with the type, because unlike an @(s -> b)@,
-- a @'Tagged' s b@ can't try to use the argument @s@ as a real value.
--
-- Moreover, you don't have to rely on the compiler to inline away the extra
-- argument, because the newtype is \"free\"
newtype Tagged s b = Tagged { unTagged :: b } deriving
  ( Eq, Ord, Ix, Bounded
#if __GLASGOW_HASKELL__ >= 707
  , Typeable
#endif
  )

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 707
instance Typeable2 Tagged where
  typeOf2 _ = mkTyConApp taggedTyCon []

taggedTyCon :: TyCon
#if __GLASGOW_HASKELL__ < 704
taggedTyCon = mkTyCon "Data.Tagged.Tagged"
#else
taggedTyCon = mkTyCon3 "tagged" "Data.Tagged" "Tagged"
#endif

#endif

instance (Data s, Data b) => Data (Tagged s b) where
  gfoldl f z (Tagged b) = z Tagged `f` b
  toConstr _ = taggedConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z Tagged)
    _ -> error "gunfold"
  dataTypeOf _ = taggedDataType
  dataCast1 f = gcast1 f
  dataCast2 f = gcast2 f

taggedConstr :: Constr
taggedConstr = mkConstr taggedDataType "Tagged" [] Prefix
{-# INLINE taggedConstr #-}

taggedDataType :: DataType
taggedDataType = mkDataType "Data.Tagged.Tagged" [taggedConstr]
{-# INLINE taggedDataType #-}
#endif

instance Show b => Show (Tagged s b) where
    showsPrec n (Tagged b) = showParen (n > 10) $
        showString "Tagged " .
        showsPrec 11 b

instance Read b => Read (Tagged s b) where
    readsPrec d = readParen (d > 10) $ \r ->
        [(Tagged a, t) | ("Tagged", s) <- lex r, (a, t) <- readsPrec 11 s]

instance Monoid a => Monoid (Tagged s a) where
    mempty = Tagged mempty
    mappend (Tagged a) (Tagged b) = Tagged (mappend a b)

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

instance Enum a => Enum (Tagged s a) where
    succ = fmap succ
    pred = fmap pred
    toEnum = Tagged . toEnum
    fromEnum (Tagged x) = fromEnum x
    enumFrom (Tagged x) = map Tagged (enumFrom x)
    enumFromThen (Tagged x) (Tagged y) = map Tagged (enumFromThen x y)
    enumFromTo (Tagged x) (Tagged y) = map Tagged (enumFromTo x y)
    enumFromThenTo (Tagged x) (Tagged y) (Tagged z) =
        map Tagged (enumFromThenTo x y z)

instance Num a => Num (Tagged s a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = Tagged . fromInteger

instance Real a => Real (Tagged s a) where
    toRational (Tagged x) = toRational x

instance Integral a => Integral (Tagged s a) where
    quot = liftA2 quot
    rem = liftA2 rem
    div = liftA2 div
    mod = liftA2 mod
    quotRem (Tagged x) (Tagged y) = (Tagged a, Tagged b) where
        (a, b) = quotRem x y
    divMod (Tagged x) (Tagged y) = (Tagged a, Tagged b) where
        (a, b) = divMod x y
    toInteger (Tagged x) = toInteger x

instance Fractional a => Fractional (Tagged s a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = Tagged . fromRational

instance Floating a => Floating (Tagged s a) where
    pi = Tagged pi
    exp = fmap exp
    log = fmap log
    sqrt = fmap sqrt
    sin = fmap sin
    cos = fmap cos
    tan = fmap tan
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    tanh = fmap tanh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh
    (**) = liftA2 (**)
    logBase = liftA2 (**)

instance RealFrac a => RealFrac (Tagged s a) where
    properFraction (Tagged x) = (a, Tagged b) where
        (a, b) = properFraction x
    truncate (Tagged x) = truncate x
    round (Tagged x) = round x
    ceiling (Tagged x) = ceiling x
    floor (Tagged x) = floor x

instance RealFloat a => RealFloat (Tagged s a) where
    floatRadix (Tagged x) = floatRadix x
    floatDigits (Tagged x) = floatDigits x
    floatRange (Tagged x) = floatRange x
    decodeFloat (Tagged x) = decodeFloat x
    encodeFloat m n = Tagged (encodeFloat m n)
    exponent (Tagged x) = exponent x
    significand = fmap significand
    scaleFloat n = fmap (scaleFloat n)
    isNaN (Tagged x) = isNaN x
    isInfinite (Tagged x) = isInfinite x
    isDenormalized (Tagged x) = isDenormalized x
    isNegativeZero (Tagged x) = isNegativeZero x
    isIEEE (Tagged x) = isIEEE x
    atan2 = liftA2 atan2

-- | Some times you need to change the tag you have lying around.
-- Idiomatic usage is to make a new combinator for the relationship between the
-- tags that you want to enforce, and define that combinator using 'retag'.
--
-- @
-- data Succ n
-- retagSucc :: 'Tagged' n a -> 'Tagged' (Succ n) a
-- retagSucc = 'retag'
-- @
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

witness :: Tagged a b -> a -> b
witness (Tagged b) _ = b
{-# INLINE witness #-}

-- | 'untagSelf' is a type-restricted version of 'untag'.
untagSelf :: Tagged a a -> a
untagSelf (Tagged x) = x
{-# INLINE untagSelf #-}
