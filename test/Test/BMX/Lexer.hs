{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.BMX.Lexer where

import qualified Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           BMX.Lexer
import           BMX.Data (Token (..), Format (..), renderToken)

import           Test.BMX.Arbitrary (genTokens, validContent)

import           P

--------------------------------------------------------------------------------
prop_none = once $ tokenise T.empty == Right []

prop_content t = validContent t ==>
  tokenise t == Right [Content t]

prop_lex_pretty_lex = forAll genTokens $ \ts ->
  let pretty = foldMap renderToken ts
  in  tokenise pretty == Right ts

--------------------------------------------------------------------------------
prop_block_comment = once $
  tokenise "{{!-- block comment --}}"
    == Right [OpenCommentBlock Verbatim, Comment " block comment ", CloseCommentBlock Verbatim]

prop_block_comment_lstrip = once $
  tokenise "{{~!-- block comment --}}"
    == Right [OpenCommentBlock Strip, Comment " block comment ", CloseCommentBlock Verbatim]

prop_block_comment_rstrip = once $
  tokenise "{{!-- block comment --~}}"
    == Right [OpenCommentBlock Verbatim, Comment " block comment ", CloseCommentBlock Strip]

prop_block_comment_lrstrip = once $
  tokenise "{{~!-- block comment --~}}"
    == Right [OpenCommentBlock Strip, Comment " block comment ", CloseCommentBlock Strip]

--------------------------------------------------------------------------------
prop_block_inverse_caret = once $
  tokenise "{{^}}" == Right [OpenInverse Verbatim, Close Verbatim]

prop_block_inverse_else = once $
  tokenise "{{else}}" == Right [OpenInverseChain Verbatim, Close Verbatim]

--------------------------------------------------------------------------------
prop_block_empty = once $
  tokenise "{{#}}" == Right [OpenBlock Verbatim, Close Verbatim]

prop_block_empty_star = once $
  tokenise "{{#*}}" == Right [OpenBlock Verbatim, Close Verbatim]

prop_end_block_empty = once $
  tokenise "{{/}}" == Right [OpenEndBlock Verbatim, Close Verbatim]

prop_partial_empty = once $
  tokenise "{{>}}" == Right [OpenPartial Verbatim, Close Verbatim]

prop_partial_block_empty = once $
  tokenise "{{#>}}" == Right [OpenPartialBlock Verbatim, Close Verbatim]

prop_ordinary_empty = once $
  tokenise "{{&}}" == Right [Open Verbatim, Close Verbatim]


return []
tests = $quickCheckAll
