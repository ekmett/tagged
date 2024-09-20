{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) 1
#endif
-- template-haskell is only safe since GHC-8.2
#if __GLASGOW_HASKELL__ >= 802
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Proxy.TH
  ( pr
  , pr1
  ) where

import Data.Char
import Data.Proxy (Proxy(..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote

proxy_d, proxy_tc :: Name
proxy_d  = 'Proxy
proxy_tc = ''Proxy

proxyTypeQ :: TypeQ -> TypeQ
proxyTypeQ t = appT (conT proxy_tc) t

proxyExpQ :: TypeQ -> ExpQ
proxyExpQ t = sigE (conE proxy_d) (proxyTypeQ t)

proxyPatQ :: TypeQ -> PatQ
proxyPatQ t = sigP (conP proxy_d []) (proxyTypeQ t)

-- | A proxy value quasiquoter. @[pr|T|]@ will splice an expression
-- @Proxy::Proxy T@, while @[pr|A,B,C|]@ will splice in a value of
-- @Proxy :: Proxy [A,B,C]@.

-- TODO: parse a richer syntax for the types involved here so we can include spaces, applications, etc.
pr :: QuasiQuoter
pr = QuasiQuoter (mkProxy proxyExpQ) (mkProxy proxyPatQ) (mkProxy proxyTypeQ) undefined where
  mkProxy :: (TypeQ -> r) -> String -> r
  mkProxy p s = case ts of
    [h@(t:_)]
       | isUpper t -> p $ conT $ mkName h
       | otherwise -> p $ varT $ mkName h
    _ -> p $ mkList <$> cons
    where
      ts = map strip $ splitOn ',' s
      cons = mapM (conT . mkName) ts
      mkList = foldr (AppT . AppT PromotedConsT) PromotedNilT

-- | Like 'pr', but takes a single type, which is used to produce a
-- 'Proxy' for a single-element list containing only that type. This
-- is useful for passing a single type to a function that wants a list
-- of types.

-- TODO: parse a richer syntax for the types involved here so we can include spaces, applications, etc.
pr1 :: QuasiQuoter
pr1 = QuasiQuoter (mkProxy proxyExpQ) (mkProxy proxyPatQ) (mkProxy proxyTypeQ) undefined where
  sing x = AppT (AppT PromotedConsT x) PromotedNilT
  mkProxy p s = case s of
    t:_
      | isUpper t -> p (fmap sing (conT $ mkName s))
      | otherwise -> p (fmap sing (varT $ mkName s))
    _ -> error "Empty string passed to pr1"

-- | Split on a delimiter.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = go where
  go [] = []
  go xs = case t of
      [] -> [h]
      (_:t') -> h : go t'
    where (h,t) = break (== d) xs

-- | Remove white space from both ends of a 'String'.
strip :: String -> String
strip = takeWhile (not . isSpace) . dropWhile isSpace
