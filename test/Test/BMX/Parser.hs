{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn #-}
module Test.BMX.Parser where

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           BMX.Data (renderProgram)
import           BMX.Lexer (tokenise, LexError (..))
import           BMX.Parser (parse, ParseError (..))

import           Test.BMX.Arbitrary ()

import           P

--------------------------------------------------------------------------------

prop_parse_roundtrip p =
  let parsed = either (Left . ParseError . (<>) "Lexer error: " . renderLexError) parse . tokenise
  in  parsed (renderProgram p) === pure p

--------------------------------------------------------------------------------
-- dopey regression tests

doesParse text = isRight parsed
  where
    tokens = tokenise text
    parsed = either (Left . ParseError . (<>) "Lexer error: " . renderLexError) parse tokens

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
tests = $forAllProperties $ quickCheckWithResult stdArgs { maxSuccess = 200 }
