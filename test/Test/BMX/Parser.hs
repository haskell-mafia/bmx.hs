{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn #-}
module Test.BMX.Parser where

import           Disorder.Core
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           BMX (templateFromText)
import           BMX.Data (renderTemplate)

import           Test.BMX.Arbitrary ()

import           P

--------------------------------------------------------------------------------

prop_parse_roundtrip p = tripping renderTemplate templateFromText p

--------------------------------------------------------------------------------
-- dopey regression tests

doesParse text = isRight (templateFromText text)

prop_basic_mustache = once . doesParse $
  "{{mustache expression}}"

prop_basic_partial = once . doesParse $
  "{{>partial expression}}"

prop_basic_partial_block = once . doesParse $
  "{{#> partial block expression}} aowiefj {{/partial}}"

prop_basic_block = once . doesParse $
  "{{# block expression}} abcdefghi {{/block}}"

prop_basic_block_inverse_1 = once . doesParse $
  "{{# block expression }} abcdefghi {{^}} jklmnop {{/block}}"

prop_basic_block_inverse_2 = once . doesParse $
  "{{# block expression }} aoiwejfoai {{else}} aowiefj {{/block}}"

prop_basic_inverse_block = once . doesParse $
  "{{^inverse block}} here we go {{^}} with inverse {{/inverse}}"

prop_basic_hash_pair = once . and $ fmap doesParse [
    "{{ mustache with hash = pair }}"
  , "{{ mustache with hash = pair fun = times }}"
  ]

--------------------------------------------------------------------------------

return []
tests = $forAllProperties $ quickCheckWithResult stdArgs { maxSuccess = 500 }
