{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Test.BMX.TH where

import Data.Functor.Identity
import Data.List (lookup)
import Data.Text (Text)
import Test.QuickCheck

import BMX
import BMX.Data

import Test.BMX.Arbitrary ()

import P


-- Just simple functionality - if it's running the parser on the right
-- input and returning the right type, that's enough

prop_qq_content = once $
  liftM renderPage (renderTemplate defaultState t) === pure e
  where t = [bmx|abcdefghijklmnop|]
        e = "abcdefghijklmnop"

prop_qq_mustache = once $
  [bmx|{{this}}|] === Template [Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "this" Nothing)) [] (Hash []))]

prop_th_templateFile = once . and $ [
    $(templateFile "test/parser/each.hbs") == each
  , $(templateFile "test/parser/ifelse.hbs") == ifelse
  ]

-- must type check
prop_th_partialFile = once $
  True === True
  where
    p :: (Applicative m, Monad m) => Partial m
    p = $(partialFile "test/parser/each.hbs")

prop_th_partialDir = once . and $ [
    isJust (lookup "test/parser/each" partials)
  , isJust (lookup "test/parser/ifelse" partials)
  ]
  where

partials :: [(Text, Partial Identity)]
partials = $(partialDir "test/parser")

each =  Template [ContentStmt "<ul class=\"people_list\">\n  ",Block (Fmt Verbatim Verbatim) (Fmt Verbatim Verbatim) (SExp (PathL (PathID "each" Nothing)) [Lit (PathL (PathID "people" Nothing))] (Hash [])) (BlockParams []) (Template [ContentStmt "\n    <li>",Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "this" Nothing)) [] (Hash [])),ContentStmt "</li>\n  "]) (Template []),ContentStmt "\n</ul>\n"]

ifelse = Template [ContentStmt "<div class=\"entry\">\n  ",Block (Fmt Verbatim Verbatim) (Fmt Verbatim Verbatim) (SExp (PathL (PathID "if" Nothing)) [Lit (PathL (PathID "author" Nothing))] (Hash [])) (BlockParams []) (Template [ContentStmt "\n    <h1>",Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "firstName" Nothing)) [] (Hash [])),ContentStmt " ",Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "lastName" Nothing)) [] (Hash [])),ContentStmt "</h1>\n  "]) (Template [Inverse (Fmt Verbatim Verbatim) (Template [ContentStmt "\n    <h1>Unknown Author</h1>\n  "])]),ContentStmt "\n</div>\n"]

return []
tests = $quickCheckAll
