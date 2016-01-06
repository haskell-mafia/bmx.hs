{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.BMX.Arbitrary where

import           Data.Char (isAlpha)
import           Data.Data
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.List (intersperse, zipWith)
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           BMX.Data
import           BMX.Lexer

import           P

--------------------------------------------------------------------------------
-- AST / parser generators

instance Arbitrary Program where
  arbitrary = everywhere (mkT merge) . Program <$> smallList arbitrary
  shrink (Program sts) = everywhere (mkT merge) . Program <$> shrinkList shrink sts

merge :: Program -> Program
merge (Program ps) = Program (go ps)
  where
    go (ContentStmt t1 : ContentStmt t2 : xs) = go (ContentStmt (t1 <> t2) : xs)
    go (x : xs) = x : go xs
    go [] = []

instance Arbitrary Stmt where
  arbitrary = oneof [
      Mustache <$> arbitrary <*> bareExpr
    , MustacheUnescaped <$> arbitrary <*> bareExpr
    , Partial <$> arbitrary <*> bareExpr
    , PartialBlock <$> arbitrary <*> arbitrary <*> bareExpr <*> body
    , Block <$> arbitrary <*> arbitrary <*> bareExpr <*> bparams <*> body <*> inverseChain
    , InverseBlock <$> arbitrary <*> arbitrary <*> bareExpr <*> bparams <*> body <*> inverse
    , RawBlock <$> bareExpr <*> rawContent
    , ContentStmt <$> arbitrary `suchThat` validContent
    , CommentStmt <$> arbitrary <*> arbitrary `suchThat` validComment
    , Decorator <$> arbitrary <*> bareExpr
    , DecoratorBlock <$> arbitrary <*> arbitrary <*> bareExpr <*> body
    ]
    where
      bparams = oneof [pure Nothing, arbitrary]
      body = Program <$> smaller (smallList arbitrary)
      inverseChain = smaller $ sized goInverse
      goInverse 0 = pure Nothing
      goInverse n = oneof [pure Nothing, inverse, inverseChain' n]
      inverseChain' n = fmap Just $
        InverseChain <$> arbitrary <*> bareExpr <*> bparams <*> body <*> goInverse (n `div` 2)
      inverse = fmap Just $ Inverse <$> arbitrary <*> body
  shrink = \case
    ContentStmt t -> ContentStmt <$> filter validContent (shrink t)
    CommentStmt f t -> CommentStmt f <$> filter validComment (shrink t)
    RawBlock e _ -> RawBlock <$> shrink e <*> [T.empty]
    other -> genericShrink other

instance Arbitrary Expr where
  arbitrary = oneof [
      Lit <$> arbitrary
    , smaller bareExpr
    ]
  shrink = genericShrink

bareExpr :: Gen Expr
bareExpr = SExp <$> arbitrary <*> smaller (smallList arbitrary) <*> smaller arbitrary

smallList :: Gen a -> Gen [a]
smallList g = sized go
  where
    go 0 = pure []
    go n = (:) <$> g <*> go (n `div` 2)

smaller :: Gen a -> Gen a
smaller g = sized $ \s -> resize (s `div` 2) g

instance Arbitrary Literal where
  arbitrary = oneof [
      PathL <$> arbitrary
    , StringL <$> arbitrary `suchThat` validString
    , NumberL <$> arbitrary
    , BooleanL <$> arbitrary
    , pure UndefinedL
    , pure NullL
    ]
  shrink = \case
    StringL t -> StringL <$> filter validString (shrink t)
    other -> recursivelyShrink other

instance Arbitrary BlockParams where
  arbitrary = BlockParams <$> listOf1 name
    where
      -- A 'simple' name, i.e. an ID without path components
      name = PathL . Path . (:[]) . PathID <$> arbitrary `suchThat` validId
  shrink (BlockParams ps) = BlockParams <$> filter (not . null) (shrinkList shrink ps)

instance Arbitrary Path where
  arbitrary = do
    p <- elements [Path, DataPath]
    i <- PathID <$> arbitrary `suchThat` validId
    cs <- listOf pair
    pure $ p (i : mconcat cs)
    where
      ident = PathID <$> arbitrary `suchThat` validId
      sep = PathSep <$> elements ['.', '/']
      pair = do
        s <- sep
        i <- ident
        pure [s, i]
  shrink = \case
    Path ps -> Path <$> filter (not . null) (idsep ps)
    DataPath ps -> DataPath <$> filter (not . null) (idsep ps)
    where idsep = subsequenceCon [PathSep '_', PathID T.empty]

instance Arbitrary PathComponent where
  arbitrary = oneof [
      PathID <$> arbitrary `suchThat` validId
    , PathSep <$> elements ['.', '/']
    ]
  shrink = \case
    PathID t -> PathID <$> filter validId (shrink t)
    _ -> []

instance Arbitrary Hash where
  arbitrary = Hash <$> smallList arbitrary
  shrink (Hash hps) = Hash <$> shrinkList shrink hps

instance Arbitrary HashPair where
  arbitrary = HashPair <$> arbitrary `suchThat` validId <*> smaller arbitrary
  shrink (HashPair t e) = HashPair <$> filter validId (shrink t) <*> shrink e

instance Arbitrary Fmt where
  arbitrary = Fmt <$> arbitrary <*> arbitrary

--------------------------------------------------------------------------------
-- Token / lexer generators

instance Arbitrary Tokens where
  arbitrary = Tokens <$> genTokens
  shrink (Tokens ts) = mconcat [
      nodrop
    , dropexprs
    , dropids
    , dropseqs
    ]
    where
      numtok = length ts
      shrunk = recursivelyShrink ts
      nodrop = Tokens <$> filter (\sts -> length sts == numtok) shrunk
      noIds =
       let list = filter (\t -> let con = toConstr t
                                in con /= toConstr (ID T.empty)
                                && con /= toConstr (SegmentID T.empty)
                                && con /= toConstr (Sep '_')) ts
       in  if list /= ts then list else []
      -- * Expr tokens can be pruned freely
      dropexprs = Tokens <$> mconcat [
          shrinkCon (Content T.empty) ts
        , shrinkCon (String T.empty) ts
        , shrinkCon (Number 0) ts
        , shrinkCon (Boolean True) ts
        , shrinkCon OpenSExp ts
        , shrinkCon CloseSExp ts
        , shrinkCon Equals ts
        , shrinkCon Data ts
        , shrinkCon Undefined ts
        , shrinkCon Null ts
        , shrinkCon OpenBlockParams ts
        , shrinkCon CloseBlockParams ts
        ]
      dropids = [Tokens noIds]
      dropseqs = Tokens <$> mconcat [
          subsequenceCon [ID T.empty, Sep '_'] ts
        , subsequenceCon [SegmentID T.empty, Sep '_'] ts
        , subsequenceCon [OpenCommentBlock Strip, Comment T.empty, CloseCommentBlock Strip] ts
        , subsequenceCon [OpenComment Strip, Comment T.empty, Close Strip] ts
        , subsequenceCon [Open Strip, Close Strip] ts
        , subsequenceCon [OpenBlock Strip, Close Strip] ts
        , subsequenceCon [OpenPartial Strip, Close Strip] ts
        , subsequenceCon [OpenPartialBlock Strip, Close Strip] ts
        , subsequenceCon [OpenUnescaped Strip, CloseUnescaped Strip] ts
        , subsequenceCon [OpenInverse Strip, Close Strip] ts
        , subsequenceCon [OpenInverseChain Strip, Close Strip] ts
        , subsequenceCon [OpenDecorator Strip, Close Strip] ts
        , subsequenceCon [OpenRawBlock, CloseRawBlock, RawContent T.empty, CloseRaw T.empty] ts
        ]

-- shrink options: try removing each element with the same constructor as m
shrinkCon :: Token -> [Token] -> [[Token]]
shrinkCon m ts = shrunk
  where
    toktype = toConstr m
    matches = listify (\t -> toConstr t == toktype) ts
    shrunk  = fmap (\mm -> filter (/= mm) ts) matches

-- Remove each subsequence with the same constructors as ms from ts
subsequenceCon :: (Data a, Typeable a) => [a] -> [a] -> [[a]]
subsequenceCon = dropSubsequenceBy (\t1 t2 -> toConstr t1 == toConstr t2)

-- given a pred and a subsequence, remove it from the list in every possible way
dropSubsequenceBy :: (a -> a -> Bool) -> [a] -> [a] -> [[a]]
dropSubsequenceBy _ [] _ = []
dropSubsequenceBy pred ms ts = go [] ts
  where
    lms = length ms
    go _ [] = []
    go tsa tsb =
      let rest = case tsb of
            [] -> []
            (x:xs) -> go (tsa <> [x]) xs
      in if and (zipWith pred ms tsb) then (tsa <> drop lms tsb) : rest else rest


instance Arbitrary Token where
  arbitrary = oneof [
      Content <$> arbitrary `suchThat` validContent
    , RawContent <$> rawContent
    , Comment <$> arbitrary `suchThat` validComment
    , Open <$> arbitrary
    , OpenPartial <$> arbitrary
    , OpenPartialBlock <$> arbitrary
    , OpenBlock <$> arbitrary
    , OpenEndBlock <$> arbitrary
    , OpenUnescaped <$> arbitrary
    , OpenInverse <$> arbitrary
    , OpenInverseChain <$> arbitrary
    , OpenCommentBlock <$> arbitrary
    , OpenComment <$> arbitrary
    , OpenDecorator <$> arbitrary
    , OpenDecoratorBlock <$> arbitrary
    , pure OpenRawBlock
    , Close <$> arbitrary
    , CloseCommentBlock <$> arbitrary
    , CloseUnescaped <$> arbitrary
    , pure CloseRawBlock
    , CloseRaw <$> arbitrary `suchThat` validId
    , ID <$> arbitrary `suchThat` validId
    , segId
    , String <$> arbitrary `suchThat` validString
    , Number <$> arbitrary
    , Boolean <$> arbitrary
    , Sep <$> elements ['.', '/']
    , pure OpenSExp
    , pure CloseSExp
    , pure Equals
    , pure Data
    , pure Undefined
    , pure Null
    , pure OpenBlockParams
    , pure CloseBlockParams
    ]
  shrink = \case
    Content t -> Content <$> filter validContent (shrink t)
    Comment t -> Comment <$> filter validComment (shrink t)
    RawContent t -> RawContent <$> filter (isRight . tokenise) (shrink t)
    ID t -> ID <$> filter validId (shrink t)
    SegmentID t -> SegmentID <$> filter validSegId (shrink t)
    String t -> String <$> filter validString (shrink t)
    CloseRaw t -> CloseRaw <$> filter validId (shrink t)
    _ -> []

instance Arbitrary Format where
  arbitrary = elements [Strip, Verbatim]


-- | Generate a sensible stream of arbitrary tokens, i.e. such that
-- genTokens >>= \ts -> tokenise (concatMap renderToken ts) == ts
genTokens :: Gen [Token]
genTokens = sized sizedGen
  where
    sizedGen 0 = pure []
    sizedGen n = merg . mconcat <$> vectorOf n (oneof [
        genTokenContent
      , genTokenMustache
      ])
    --
    merg (Content t1 : Content t2 : ts) = merg (Content (t1 <> t2) : ts)
    merg (t:ts) = t : merg ts
    merg [] = []

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
  body <- genTokenExpr1
  c  <- Close <$> arbitrary
  pure (o f1 : body <> [c])

-- | Generate a sensible Handlebars expression (inside a partial block etc)
genTokenExpr :: Gen [Token]
genTokenExpr = do
  names <- listOf genTokenId
  atomz <- fmap (:[]) (listOf atoms)
  pure (mconcat (interweave names atomz))

interweave :: [a] -> [a] -> [a]
interweave as bs = mconcat (zipWith (\a b -> [a, b]) as bs)

genTokenExpr1 :: Gen [Token]
genTokenExpr1 = do
  names <- listOf1 genTokenId
  atomz <- fmap (:[]) (listOf1 atoms)
  pure (mconcat (interweave names atomz))

atoms :: Gen Token
atoms = oneof [
    pure OpenSExp
  , pure CloseSExp
  , Number <$> arbitrary
  , String <$> arbitrary `suchThat` validString
  , pure Equals
  , pure Data
  , pure Undefined
  , pure Null
  , pure OpenBlockParams
  , pure CloseBlockParams
  ]

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

validSegId :: Text -> Bool
validSegId t = and [noEscape t, noNull t, T.takeEnd 1 t /= "\\"]
  where noEscape tt = and (fmap escaped (splits tt))
        splits = T.breakOnAll "]"
        escaped (m, _) = and [T.takeEnd 1 m == "\\", T.takeEnd 2 m /= "\\\\"]

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
  spam <- RawContent <$> rawContent
  c    <- CloseRaw <$> weakId
  pure (OpenRawBlock : body <> [CloseRawBlock, spam, c])
  where
    weakId = arbitrary `suchThat` alpha
    alpha = T.all isAlpha

rawContent :: Gen Text
rawContent = do
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
  , isAlpha (T.head t)
  ]

validString :: Text -> Bool
validString t = and $ noNull t : unescapedEnd : (fmap noescape splits)
  where
    splits = T.breakOnAll "\"" t
    noescape (s, _) = T.takeEnd 1 s /= "\\"
    unescapedEnd = T.takeEnd 1 t /= "\\"
