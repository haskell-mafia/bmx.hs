{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.BMX.Position where

import           Data.Data (Data, Typeable)
import           Data.Generics.Aliases (mkT, mkQ)
import           Data.Generics.Schemes (everywhere, everythingBut)
import           Data.List (sort)

import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.BMX.Arbitrary ()

import           BMX
import           BMX.Data

import           P

-- -----------------------------------------------------------------------------
-- SrcInfo

prop_srcinfo_monoid_assoc x y z = (x <> y) <> z === (x <> (y <> z) :: SrcInfo)
prop_srcinfo_monoid_left_id x = mempty <> x === (x :: SrcInfo)
prop_srcinfo_monoid_right_id x = x <> mempty === (x :: SrcInfo)

-- -----------------------------------------------------------------------------
-- Positioned

prop_positioned_monoid_assoc x y z = (x <> y) <> z === (x <> (y <> z) :: Positioned ())
prop_positioned_monoid_left_id x = mempty <> x === (x :: Positioned ())
prop_positioned_monoid_right_id x = x <> mempty === (x :: Positioned ())

prop_positioned_functor_id :: Positioned Int -> Property
prop_positioned_functor_id a = fmap id a === id a

prop_positioned_functor_compose :: Positioned Int -> Fun Int Int -> Fun Int Int -> Bool
prop_positioned_functor_compose = f_compose

prop_positioned_ap_id :: Positioned Int -> Property
prop_positioned_ap_id a = ap_id a

prop_positioned_ap_compose :: Positioned (Fun Int Int) -> Positioned (Fun Int Int)
                           -> Positioned Int -> Property
prop_positioned_ap_compose (fmap apply -> u) (fmap apply -> v) = ap_compose u v

prop_positioned_ap_hom :: Fun Int Int -> Int -> Property
prop_positioned_ap_hom (apply -> f) x = (pure f <*> pure x) === (pure (f x) :: Positioned Int)

prop_positioned_ap_interchange :: Positioned (Fun Int Int) -> Int -> Property
prop_positioned_ap_interchange (fmap apply -> u) = ap_interchange u

-- -----------------------------------------------------------------------------
-- common props

-- Borrowed from http://austinrochford.com/posts/2014-05-27-quickcheck-laws.html
f_compose :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
f_compose x (apply -> f) (apply -> g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

ap_id :: (Applicative f, Eq (f a), Show (f a)) => f a -> Property
ap_id a = (pure id <*> a) === a

ap_compose :: (Eq (f b), Show (f b), Applicative f) => f (a -> b) -> f (a1 -> a) -> f a1 -> Property
ap_compose u v w = (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

-- ap_hom :: (Eq (f b), Show (f b), Applicative f) => f () -> (a -> b) -> a -> Property
-- ap_hom _ f x = (pure f <*> pure x) === (pure (f x) :: f b)

ap_interchange :: (Eq (f b), Show (f b), Applicative f) => f (a -> b) -> a -> Property
ap_interchange u y = (u <*> pure y) === (pure ($ y) <*> u)

-- -----------------------------------------------------------------------------
-- Template location accuracy

-- | Sets all the positions in the template to zero, erasing all context
decontextualise :: Template -> Template
decontextualise = everywhere (mkT eraseLoc)
  where
    eraseLoc :: SrcInfo -> SrcInfo
    eraseLoc = const mempty

-- utility function: take an arbitrary template, pretty-print it,
-- parse it again. lazy way to get accurate locations when we need
-- them (i.e. here)
recontextualise :: Template -> Either BMXError Template
recontextualise = templateFromText . templateToText

-- For any list of [Positioned a], we should have an ordered list of
-- SrcInfo with no overlap.
prop_position_no_overlap t' = case recontextualise t' of
  Left _ -> False
  Right t -> noOverlap t
  where
    noOverlap t = and [
        checkStmts t
      , checkExprs t
      , checkHashes t
      ]
    checkStmts t = and . fmap overlapTest $ (extractPos t :: [[Positioned Stmt]])
    checkExprs t = and . fmap overlapTest $ (extractPos t :: [[Positioned Expr]])
    checkHashes t = and . fmap overlapTest $ (extractPos t :: [[Positioned HashPair]])
    --
    overlapTest :: [Positioned a] -> Bool
    overlapTest ((_ :@ x) : yy@(_ :@ y) : xs) = if overlap x y then False else overlapTest (yy:xs)
    overlapTest (_:_) = True
    overlapTest [] = True
    --
    overlap :: SrcInfo -> SrcInfo -> Bool
    overlap NoInfo _ = False
    overlap _ NoInfo = False
    overlap (SrcLoc a b) (SrcLoc c d) = let ordered = sort [a, b, c, d]
      in ordered /= [a, b, c, d] && ordered /= [c, d, a, b]

-- Pull out all nonempty lists of positioned elements
extractPos :: (Data a, Data b, Typeable a, Typeable b) => a -> [[Positioned b]]
extractPos = everythingBut (<>) (([], False) `mkQ` f)
  where
    f r@((_ :@ _):_) = ([r], True)
    f _ = ([], True)


-- For some template, extract all positions. any gaps in the coverage
-- should be whitespace in the original text
-- prop_position_gaps_are_whitespace
-- FIX TODO


return []
tests = $quickCheckAll
