{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.BMX.Page where

import Data.Text ()
import Test.QuickCheck

import BMX.Internal

import Test.BMX.Arbitrary ()

import P

prop_page_strip_left = once $
  Formattee "abcdefghij   \t\n\n \t   " <> Formatter Strip Verbatim "klmnop" "" ""
    === Formatter Verbatim Verbatim "klmnop" "abcdefghij" ""

prop_page_strip_right = once $
  Formatter Verbatim Strip "abcdefghij" "" "" <> Formattee "     \n \t \n \n \t     klmnop"
    === Formatter Verbatim Verbatim "abcdefghij" "" "klmnop"

prop_page_strip_left_twice = once $
  let p1 = Formatter Verbatim Strip "abcde" "" ""
      p2 = Formattee "                           \n\t\n\t\r\r         "
      p3 = Formattee "               \n\n\t\t\n  fghij "
  in p1 <> p2 <> p3 === Formatter Verbatim Verbatim "abcde" "" "fghij "

prop_page_strip_right_twice = once $
  let p1 = Formattee "abcde                \n\n \t\t\t \n   "
      p2 = Formattee "     \n\t\t\t\t\n \n \n               "
      p3 = Formatter Strip Verbatim "fghij " "" ""
  in p1 <> p2 <> p3 === Formatter Verbatim Verbatim "fghij " "abcde" ""

prop_page_monoid_assoc x y z = (x <> y) <> z === (x <> (y <> z) :: Page)

prop_page_monoid_left_id x = mempty <> x === (x :: Page)

prop_page_monoid_right_id x = x <> mempty === (x :: Page)

return []
tests = $forAllProperties $ quickCheckWithResult stdArgs { maxSuccess = 10000 }
