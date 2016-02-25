{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wwarn #-}
module Test.BMX.Lexer where

import           Data.List (iterate, (!!))
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           BMX.Data.Position
import           BMX.Data.Token
import qualified BMX.Lexer as Lexer (tokenise)

import           Test.BMX.Arbitrary ()

import           P

-- all lexer tests are equality-modulo-position
tokenise = fmap (fmap depo) . Lexer.tokenise

--------------------------------------------------------------------------------
-- The parser roundtrip test provides most of our lexer coverage.
-- These regression tests are here for quick diagnosis of any breakage,
-- and whitespace stuff that the pretty printer doesn't generate

prop_lex_none = once $ tokenise T.empty == Right []

prop_lex_content = once $
  let str = T.pack ['A'..'z']
  in tokenise str === Right [Content str]

prop_lex_comment = once $
  tokenise "{{! inline comment }}"
    === Right [OpenComment Verbatim, Comment " inline comment ", Close Verbatim]

prop_lex_block_comment = once $
  tokenise "{{!-- block comment --}}"
    === Right [OpenCommentBlock Verbatim, Comment " block comment ", CloseCommentBlock Verbatim]

prop_lex_block_inverse_caret = once $
  tokenise "{{^}}" === Right [OpenInverse Verbatim, Close Verbatim]

prop_lex_block_inverse_else = once $
       tokenise "{{ else }}" === res
  .&&. tokenise "{{else}}" === res
  .&&. tokenise "{{ else}}" === res
  .&&. tokenise "{{else }}" === res
  where res = Right [OpenInverseChain Verbatim, Close Verbatim]

prop_lex_inverse_chain = once $
  tokenise "{{else if abcde}}"
    === Right [OpenInverseChain Verbatim, ID "if", ID "abcde", Close Verbatim]

prop_lex_inverse_block = once $
  tokenise "{{^if cool}}"
    === Right [OpenInverse Verbatim, ID "if", ID "cool", Close Verbatim]

prop_lex_block_empty = once $
  tokenise "{{#}}" === Right [OpenBlock Verbatim, Close Verbatim]

prop_lex_block_empty_star = once $
  tokenise "{{#*}}" === Right [OpenDecoratorBlock Verbatim, Close Verbatim]

prop_lex_end_block_empty = once $
  tokenise "{{/}}" === Right [OpenEndBlock Verbatim, Close Verbatim]

prop_lex_partial_empty = once $
  tokenise "{{>}}" === Right [OpenPartial Verbatim, Close Verbatim]

prop_lex_partial_block_empty = once $
  tokenise "{{#>}}" === Right [OpenPartialBlock Verbatim, Close Verbatim]

prop_lex_unescaped_empty = once $
  tokenise "{{&}}" === Right [OpenUnescaped Verbatim, Close Verbatim]

prop_lex_unescaped_empty_2 = once $
  tokenise "{{{}}}" === Right [OpenUnescaped Verbatim, CloseUnescaped Verbatim]

prop_lex_ordinary_empty = once $
  tokenise "{{}}" === Right [Open Verbatim, Close Verbatim]

prop_lex_decorator_empty = once $
  tokenise "{{*}}" === Right [OpenDecorator Verbatim, Close Verbatim]

prop_lex_decorator_block_empty = once $
  tokenise "{{#*}}" === Right [OpenDecoratorBlock Verbatim, Close Verbatim]

prop_lex_raw_simple = once $
  let str = T.pack ['A'..'z']
  in tokenise ("{{{{rawhelper}}}}" <> str <> "{{{{/rawhelper}}}}")
       === Right [OpenRawBlock, ID "rawhelper", CloseRawBlock, RawContent str, CloseRaw "rawhelper"]

prop_lex_raw_nested_n i = isRight $ tokenise (rawNest (abs i))
  where raw n s = "{{{{" <> str n <> "}}}}" <> s <> "{{{{/" <> str n <> "}}}}"
        str n = "raw" <> T.pack (show n)
        rawNest n = iterate (raw i) T.empty !! n

prop_lex_raw_nested_fail = once $
  isLeft $ tokenise "{{{{raw}}}} {{{{nested}}}} {{{{/raw}}}}"

-- . is an ID in some contexts and a separator in others - fiddly code
prop_lex_id_dot = once $
  tokenise "{{ . }}" === Right [Open Verbatim, ID ".", Close Verbatim]

prop_lex_id_dotdot = once $
  tokenise "{{ .. }}" === Right [Open Verbatim, ID "..", Close Verbatim]

prop_lex_id_dot_dotdot = once $
  tokenise "{{ ./.. }}" === Right [Open Verbatim, ID ".", Sep '/', ID "..", Close Verbatim]

prop_lex_id_dotdot_dot = once $
  tokenise "{{ ../. }}" === Right [Open Verbatim, ID "..", Sep '/', ID ".", Close Verbatim]

prop_lex_id_multipath = once $
  tokenise "{{ abc.def.ghi/jkl/.. }}"
    === Right [Open Verbatim, ID "abc", Sep '.', ID "def", Sep '.',
               ID "ghi", Sep '/', ID "jkl", Sep '/', ID "..", Close Verbatim]

prop_lex_segid_simple = once $
  tokenise "{{ [title] }}" === Right [Open Verbatim, SegmentID "title", Close Verbatim]

prop_lex_segid_number = once $
  tokenise "{{ [10] }}" === Right [Open Verbatim, SegmentID "10", Close Verbatim]

prop_lex_nullable_id = once $
  tokenise "{{ nullable }}" === Right [ Open Verbatim, ID "nullable", Close Verbatim ]

return []
tests = $quickCheckAll
