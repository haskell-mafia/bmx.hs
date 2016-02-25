{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Test.BMX.TH where

import Data.Functor.Identity
import Data.List (lookup)
import Data.Text (Text)
import Test.QuickCheck

import BMX
import BMX.Data

import Test.BMX.Arbitrary ()
import Test.BMX.Position (decontextualise)

import P


-- Just simple functionality - if it's running the parser on the right
-- input and returning the right type, that's enough

prop_qq_content = once $
  liftM renderPage (renderTemplate defaultState t) === pure e
  where t = [bmx|abcdefghijklmnop|]
        e = "abcdefghijklmnop"

prop_qq_mustache = once $
  [bmx|{{this}}|] ===
    Template [Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "this" Nothing) :@ SrcInfo {leadingPosition = Position {posLine = 1, posColumn = 3}, trailingPosition = Position {posLine = 1, posColumn = 7}}) [] (Hash [] :@ SrcInfo {leadingPosition = Position {posLine = 0, posColumn = 0}, trailingPosition = Position {posLine = 0, posColumn = 0}}) :@ SrcInfo {leadingPosition = Position {posLine = 1, posColumn = 3}, trailingPosition = Position {posLine = 0, posColumn = 0}}) :@ SrcInfo {leadingPosition = Position {posLine = 1, posColumn = 1}, trailingPosition = Position {posLine = 1, posColumn = 9}}]

prop_th_templateFile = once . and $ [
    decontextualise $(templateFile "test/parser/each.hbs") == decontextualise each
  , decontextualise $(templateFile "test/parser/ifelse.hbs") == decontextualise ifelse
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

each = Template [ContentStmt ("<ul class=\"people_list\">\n  " :@ SrcInfo {leadingPosition = Position {posLine = 1, posColumn = 1}, trailingPosition = Position {posLine = 2, posColumn = 3}}) :@ SrcInfo {leadingPosition = Position {posLine = 1, posColumn = 1}, trailingPosition = Position {posLine = 2, posColumn = 3}},Block (Fmt Verbatim Verbatim) (Fmt Verbatim Verbatim) (SExp (PathL (PathID "each" Nothing) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 6}, trailingPosition = Position {posLine = 2, posColumn = 10}}) [Lit (PathL (PathID "people" Nothing) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 11}, trailingPosition = Position {posLine = 2, posColumn = 17}}) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 11}, trailingPosition = Position {posLine = 2, posColumn = 17}}] (Hash [] :@ SrcInfo {leadingPosition = Position {posLine = 0, posColumn = 0}, trailingPosition = Position {posLine = 0, posColumn = 0}}) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 6}, trailingPosition = Position {posLine = 0, posColumn = 0}}) Nothing (Template [ContentStmt ("\n    <li>" :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 19}, trailingPosition = Position {posLine = 3, posColumn = 9}}) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 19}, trailingPosition = Position {posLine = 3, posColumn = 9}},Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "this" Nothing) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 11}, trailingPosition = Position {posLine = 3, posColumn = 15}}) [] (Hash [] :@ SrcInfo {leadingPosition = Position {posLine = 0, posColumn = 0}, trailingPosition = Position {posLine = 0, posColumn = 0}}) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 11}, trailingPosition = Position {posLine = 0, posColumn = 0}}) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 9}, trailingPosition = Position {posLine = 3, posColumn = 17}},ContentStmt ("</li>\n  " :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 17}, trailingPosition = Position {posLine = 4, posColumn = 3}}) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 17}, trailingPosition = Position {posLine = 4, posColumn = 3}}] :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 17}, trailingPosition = Position {posLine = 3, posColumn = 9}}) (Template [] :@ SrcInfo {leadingPosition = Position {posLine = 0, posColumn = 0}, trailingPosition = Position {posLine = 0, posColumn = 0}}) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 3}, trailingPosition = Position {posLine = 4, posColumn = 12}},ContentStmt ("\n</ul>\n" :@ SrcInfo {leadingPosition = Position {posLine = 4, posColumn = 12}, trailingPosition = Position {posLine = 6, posColumn = 1}}) :@ SrcInfo {leadingPosition = Position {posLine = 4, posColumn = 12}, trailingPosition = Position {posLine = 6, posColumn = 1}}]

ifelse = Template [ContentStmt ("<div class=\"entry\">\n  " :@ SrcInfo {leadingPosition = Position {posLine = 1, posColumn = 1}, trailingPosition = Position {posLine = 2, posColumn = 3}}) :@ SrcInfo {leadingPosition = Position {posLine = 1, posColumn = 1}, trailingPosition = Position {posLine = 2, posColumn = 3}},Block (Fmt Verbatim Verbatim) (Fmt Verbatim Verbatim) (SExp (PathL (PathID "if" Nothing) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 6}, trailingPosition = Position {posLine = 2, posColumn = 8}}) [Lit (PathL (PathID "author" Nothing) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 9}, trailingPosition = Position {posLine = 2, posColumn = 15}}) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 9}, trailingPosition = Position {posLine = 2, posColumn = 15}}] (Hash [] :@ SrcInfo {leadingPosition = Position {posLine = 0, posColumn = 0}, trailingPosition = Position {posLine = 0, posColumn = 0}}) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 6}, trailingPosition = Position {posLine = 0, posColumn = 0}}) Nothing (Template [ContentStmt ("\n    <h1>" :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 17}, trailingPosition = Position {posLine = 3, posColumn = 9}}) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 17}, trailingPosition = Position {posLine = 3, posColumn = 9}},Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "firstName" Nothing) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 11}, trailingPosition = Position {posLine = 3, posColumn = 20}}) [] (Hash [] :@ SrcInfo {leadingPosition = Position {posLine = 0, posColumn = 0}, trailingPosition = Position {posLine = 0, posColumn = 0}}) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 11}, trailingPosition = Position {posLine = 0, posColumn = 0}}) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 9}, trailingPosition = Position {posLine = 3, posColumn = 22}},ContentStmt (" " :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 22}, trailingPosition = Position {posLine = 3, posColumn = 23}}) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 22}, trailingPosition = Position {posLine = 3, posColumn = 23}},Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "lastName" Nothing) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 25}, trailingPosition = Position {posLine = 3, posColumn = 33}}) [] (Hash [] :@ SrcInfo {leadingPosition = Position {posLine = 0, posColumn = 0}, trailingPosition = Position {posLine = 0, posColumn = 0}}) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 25}, trailingPosition = Position {posLine = 0, posColumn = 0}}) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 23}, trailingPosition = Position {posLine = 3, posColumn = 35}},ContentStmt ("</h1>\n  " :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 35}, trailingPosition = Position {posLine = 4, posColumn = 3}}) :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 35}, trailingPosition = Position {posLine = 4, posColumn = 3}}] :@ SrcInfo {leadingPosition = Position {posLine = 3, posColumn = 35}, trailingPosition = Position {posLine = 3, posColumn = 9}}) (Template [Inverse (Fmt Verbatim Verbatim) (Template [ContentStmt ("\n    <h1>Unknown Author</h1>\n  " :@ SrcInfo {leadingPosition = Position {posLine = 4, posColumn = 11}, trailingPosition = Position {posLine = 6, posColumn = 3}}) :@ SrcInfo {leadingPosition = Position {posLine = 4, posColumn = 11}, trailingPosition = Position {posLine = 6, posColumn = 3}}] :@ SrcInfo {leadingPosition = Position {posLine = 4, posColumn = 11}, trailingPosition = Position {posLine = 6, posColumn = 3}}) :@ SrcInfo {leadingPosition = Position {posLine = 4, posColumn = 3}, trailingPosition = Position {posLine = 6, posColumn = 3}}] :@ SrcInfo {leadingPosition = Position {posLine = 4, posColumn = 3}, trailingPosition = Position {posLine = 6, posColumn = 3}}) :@ SrcInfo {leadingPosition = Position {posLine = 2, posColumn = 3}, trailingPosition = Position {posLine = 6, posColumn = 10}},ContentStmt ("\n</div>\n" :@ SrcInfo {leadingPosition = Position {posLine = 6, posColumn = 10}, trailingPosition = Position {posLine = 8, posColumn = 1}}) :@ SrcInfo {leadingPosition = Position {posLine = 6, posColumn = 10}, trailingPosition = Position {posLine = 8, posColumn = 1}}]

return []
tests = $quickCheckAll
