{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.BMX.Arbitrary where

import           Data.Char (isNumber, isAlpha)
import           Data.List (intersperse)
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           BMX.Data
import           BMX.Lexer

import           P


instance Arbitrary Format where
  arbitrary = elements [Strip, Verbatim]


-- | Generate a sensible stream of arbitrary tokens, i.e. such that
-- genTokens >>= \ts -> tokenise (concatMap renderToken ts) == ts
genTokens :: Gen [Token]
genTokens = sized sizedGen
  where
    sizedGen 0 = pure []
    sizedGen n = merge . mconcat <$> vectorOf n (oneof [
        genTokenContent
      , genTokenMustache
      ])
    --
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
  , genTokenUnescaped
  , genTokenRawBlock
  ]

-- | Generate a sensible block expression
genTokenMuExpr :: Gen [Token]
genTokenMuExpr = do
  f1 <- arbitrary
  o  <- elements [
            Open
          , OpenPartial
          , OpenPartialBlock
          , OpenBlock
          , OpenEndBlock
          , OpenUnescaped
          , OpenInverse
          , OpenInverseChain
          , OpenDecorator
          , OpenDecoratorBlock
          ]
  body <- genTokenExpr
  c  <- Close <$> arbitrary
  pure (o f1 : body <> [c])

-- | Generate a sensible Handlebars expression (inside a partial block etc)
genTokenExpr :: Gen [Token]
genTokenExpr = oneof [genTokenId, atoms]
  where atoms = listOf (oneof [
            pure OpenSExp
          , pure CloseSExp
          , Number <$> arbitrary
          , pure Equals
          , pure Data
          , pure Undefined
          , pure Null
          , pure OpenBlockParams
          , pure CloseBlockParams
          ])

-- | The '.' '..' '/' characters are parsed differently when bare / mid-ID
genTokenId :: Gen [Token]
genTokenId = do
  ids <- listOf $ oneof [bareId, segId]
  sp  <- Sep <$> elements ['.', '/']
  pure (intersperse sp ids)

bareId :: Gen Token
bareId = ID <$> (arbitrary `suchThat` validId)

segId :: Gen Token
segId = SegmentID <$> (arbitrary `suchThat` validSegId)
  where noEscape t = and (fmap escaped (splits t))
        splits = T.breakOnAll "]"
        escaped (m, _) = and [T.takeEnd 1 m == "\\", T.takeEnd 2 m /= "\\\\"]
        validSegId t = and [noEscape t, noNull t, T.takeEnd 1 t /= "\\"]

-- | Unescaped blocks have a custom closing tag
genTokenUnescaped :: Gen [Token]
genTokenUnescaped = do
  o <- OpenUnescaped <$> arbitrary
  body <- genTokenExpr
  c <- CloseUnescaped <$> arbitrary
  pure (o : body <> [c])

-- | Block comment - no other content can appear in the block
genTokenComment :: Gen [Token]
genTokenComment = oneof [genCommentBlock, genComment]
  where
    genCommentBlock = do
      o <- OpenCommentBlock <$> arbitrary
      t <- Comment <$> (arbitrary `suchThat` validComment)
      c <- CloseCommentBlock <$> arbitrary
      pure [o, t, c]
    genComment = do
      o <- OpenComment <$> arbitrary
      t <- Comment <$> (arbitrary `suchThat` validWeakComment)
      pure [o, t, Close Verbatim]

-- | Handlebars raw block: a block in {{{{someid params}}}}, followed by arbitrary text,
-- followed by {{{{/someid}}}}.
-- Any enclosed raw blocks must be closed and balanced
-- (nice recursive generator - any valid Handlebars expression should parse)
genTokenRawBlock :: Gen [Token]
genTokenRawBlock = do
  body <- genTokenExpr
  spam <- RawContent <$> prettyMustache
  c    <- CloseRaw <$> weakId
  pure (OpenRawBlock : body <> [CloseRawBlock, spam, c])
  where
    weakId = arbitrary `suchThat` alpha
    alpha = T.all isAlpha
    prettyMustache = do
      ts <- sized $ \s -> resize (min s 5) genTokens
      pure (T.concat $ fmap renderToken ts)

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
  , T.head t /= '}'
  ]

-- | Anything without NUL is a valid comment
validComment :: Text -> Bool
validComment = noNull

-- | Weaker comments (inside {{! }} blocks) can't have mustaches
validWeakComment :: Text -> Bool
validWeakComment t = and [noNull t, noMustaches t, noMustacheClose t, T.takeEnd 1 t /= "}"]
  where noMustacheClose = P.null . T.breakOnAll "}}"

-- | Generated ID can't contain Sep characters, conflicts w sep
-- also can't contain anything in the other keywords
-- can't start with a number, else number parser kicks in
validId t = and [
    T.all (\c -> and [validIdChar c, c /= '.', c /= '/']) t
  , t /= "as"
  , not (T.null t)
  , not (isNumber (T.head t))
  ]
