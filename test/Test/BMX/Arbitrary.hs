{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.BMX.Arbitrary where

import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           BMX.Data

import           P


instance Arbitrary Format where
  arbitrary = elements [Strip, Verbatim]

-- | Generate a sensible stream of arbitrary tokens, i.e. such that
-- genTokens >>= \ts -> tokenise (concatMap renderToken ts) == ts
genTokens :: Gen [Token]
genTokens = merge . mconcat <$> listOf (oneof [
    genTokenContent
  , genTokenMustache
  ])
  where
    merge (Content t1 : Content t2 : ts) = merge (Content (t1 <> t2) : ts)
    merge (t:ts) = t : merge ts
    merge [] = []

-- | Generate a sensible piece of Content
genTokenContent :: Gen [Token]
genTokenContent = (:[]) . Content <$> arbtext
  where arbtext = arbitrary `suchThat` validContent

-- | Generate a sensible mustache expression
genTokenMustache :: Gen [Token]
genTokenMustache = oneof [
    genTokenMuExpr
  , genTokenComment
  ]

-- | Generate a sensible block expression
genTokenMuExpr :: Gen [Token]
genTokenMuExpr = do
  f1 <- arbitrary
  o  <- elements [Open, OpenPartial, OpenPartialBlock, OpenBlock, OpenEndBlock,
                  OpenUnescaped, OpenInverse, OpenInverseChain]
  body <- genTokenExpr
  c  <- Close <$> arbitrary
  pure (o f1 : body <> [c])

-- | Generate a sensible Handlebars expression (inside a partial block 4etc)
genTokenExpr :: Gen [Token]
genTokenExpr = listOf (oneof [
    pure OpenSExp
  , pure CloseSExp
  ])

-- | Block comment - no other content can appear in the block
genTokenComment :: Gen [Token]
genTokenComment = do
  o <- OpenCommentBlock <$> arbitrary
  t <- Comment <$> (arbitrary `suchThat` validComment)
  c <- CloseCommentBlock <$> arbitrary
  pure [o, t, c]

-- | Allow only escaped Mustache expressions
noMustaches :: Text -> Bool
noMustaches t = and (fmap escaped splits)
  where
    splits = T.breakOnAll "{{" t
    escaped (m, _) = and [T.takeEnd 1 m == "\\", T.takeEnd 2 m /= "\\\\"]

-- | Get out of here, NUL!
noNull :: Text -> Bool
noNull t = isNothing (T.find (== '\0') t)

-- | Anything is valid as Content except for unescaped Mustaches, NUL, and empty.
-- Can contain a '{', but not at the end (e.g. [Content "{", Open] -> "{{{", parse fail)
-- Also can't end in '\', accidental escape
validContent :: Text -> Bool
validContent t = and [
    t /= T.empty
  , noNull t
  , noMustaches t
  , T.last t /= '{'
  , T.last t /= '\\'
  ]

-- | Anything without NUL is a valid comment
validComment :: Text -> Bool
validComment = noNull
