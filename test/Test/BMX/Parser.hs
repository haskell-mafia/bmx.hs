{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn #-}
module Test.BMX.Parser where

import           Disorder.Core
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           BMX (templateFromText, templateToText)

import           Test.BMX.Arbitrary

import           P

--------------------------------------------------------------------------------

prop_parse_roundtrip p = tripping templateToText templateFromText p

--------------------------------------------------------------------------------
-- dopey regression tests - the stuff that's easy to break

-- FIX freeze these to their current AST values
doesParse text = isRight (templateFromText text)

prop_parse_basic_mustache = once . doesParse $
  "{{mustache expression}}"

prop_parse_basic_mustache_unescaped = once . doesParse $
  "{{{mustache expression}}}"

prop_parse_basic_partial = once . doesParse $
  "{{> partial }}"

prop_parse_basic_partial_ctx = once . doesParse $
  "{{>partial expression}}"

prop_parse_basic_dynamic_partial = once . doesParse $
  "{{> (lookup . \"component\") }}"

prop_parse_basic_dynamic_partial_ctx = once . doesParse $
  "{{> (lookup . 'component') somecontext }}"

prop_parse_basic_partial_block = once . doesParse $
  "{{#> partial block }} aowiefj {{/partial}}"

prop_parse_basic_block = once . doesParse $
  "{{# block expression}} abcdefghi {{/block}}"

prop_parse_basic_block_inverse_1 = once . doesParse $
  "{{# block expression }} abcdefghi {{^}} jklmnop {{/block}}"

prop_parse_basic_block_inverse_2 = once . doesParse $
  "{{# block expression }} aoiwejfoai {{else}} aowiefj {{/block}}"

prop_parse_basic_inverse_block = once . doesParse $
  "{{^inverse block}} here we go {{^}} with inverse {{/inverse}}"

prop_parse_basic_hash_pair = once . and $ fmap doesParse [
    "{{ mustache with hash = pair }}"
  , "{{ mustache with hash = pair fun = times }}"
  ]

prop_parse_basic_raw_block = forAll rawContent $ \t ->
  doesParse ("{{{{noop}}}}" <> t <> "{{{{/noop}}}}")

-- null should throw a parse error
prop_parse_null_fails = once $
  isLeft (templateFromText "{{ null }}")

prop_parse_undef_fails = once $
  isLeft (templateFromText "{{ undefined }}")

--------------------------------------------------------------------------------

return []
tests = $forAllProperties $ quickCheckWithResult stdArgs { maxSuccess = 500 }
