{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.BMX.Arbitrary where

import           Data.Char (isAlpha)
import           Data.Data
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.List (nubBy, zipWith)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           BMX.Data
import           BMX.Lexer (validIdChar)

import           Test.BMX.Orphans ()

import           P

--------------------------------------------------------------------------------
-- Contexts and values

instance Arbitrary Context where
  arbitrary = sized genContext
  shrink c = genericShrink c

instance Arbitrary Value where
  arbitrary = sized genVal
  shrink v = genericShrink v

instance Arbitrary Param where
  arbitrary = Param <$> simpleId
  shrink (Param t) = Param <$> filter validId (shrink t)

genContext 0 = pure (Context mempty)
genContext n = Context . M.fromList <$> vectorOf (n `div` 2) ctxPairs

ctxPairs = (,) <$> simpleId <*> genVal 4

genVal 0 = oneof [
    pure NullV
  , pure UndefinedV
  , NumberV <$> arbitrary
  , StringV <$> arbitrary
  , BoolV <$> arbitrary
  ]
genVal n = frequency [
    (10, genVal 0)
  , (1, ContextV <$> genContext (n `div` 2))
  , (1, ListV <$> smallList (genVal (n `div` 2)))
  ]

instance Arbitrary BMXValue where
  shrink = genericShrink
  arbitrary = unbox <$> genVal 8
    where
      unbox :: Value -> BMXValue
      unbox = \case
        NumberV i -> BMXNum i
        StringV s -> BMXString s
        BoolV b -> BMXBool b
        NullV -> BMXNull
        ListV ls -> BMXList (fmap unbox ls)
        ContextV (Context ctx) -> BMXContext . M.toList $ fmap unbox ctx
        UndefinedV -> BMXNull -- why not

genCtxList :: Gen [(Text, BMXValue)]
genCtxList = liftM (sortOn fst . nubBy (on (==) fst)) $
  zipWith (,) <$> smallList simpleId <*> smallList arbitrary

--------------------------------------------------------------------------------
-- Page

instance Arbitrary Page where
  arbitrary = elements [Formatter, \_ _ t _ _ -> Formattee t]
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Formattee t) = Formattee <$> shrink t
  shrink (Formatter lf rf t1 t2 t3) = bodies <> leftsh <> rightsh
    where bodies = (\t -> Formatter lf rf t t2 t3) <$> shrink t1
          leftsh = (\t -> Formatter lf rf t1 t t3) <$> shrink t2
          rightsh = (\t -> Formatter lf rf t1 t2 t) <$> shrink t3

--------------------------------------------------------------------------------
-- AST / parser generators

instance Arbitrary Template where
  -- Adjacent ContentStmt clauses need to get concatenated.
  -- Do this recursively across all template fragments in the tree via syb
  arbitrary = everywhere (mkT merge) . Template <$> smallList arbitrary
  shrink (Template sts) = everywhere (mkT merge) . Template <$> shrinkList shrink sts

instance Arbitrary Stmt where
  arbitrary = oneof [
      Mustache <$> arbitrary <*> bareExpr
    , MustacheUnescaped <$> arbitrary <*> bareExpr
    , PartialStmt <$> arbitrary <*> arbitrary <*> expmay <*> arbitrary
    , PartialBlock <$> arbitrary <*> arbitrary <*> arbitrary <*> expmay <*> arbitrary <*> body
    , Block <$> arbitrary <*> arbitrary <*> bareExpr <*> arbitrary <*> body <*> inverseChain
    , InverseBlock <$> arbitrary <*> arbitrary <*> bareExpr <*> arbitrary <*> body <*> inverse
    , RawBlock <$> bareExpr <*> rawContent
    , ContentStmt <$> arbitrary `suchThat` validContent
    , CommentStmt <$> arbitrary <*> arbitrary `suchThat` validComment
    , DecoratorStmt <$> arbitrary <*> bareExpr
    , DecoratorBlock <$> arbitrary <*> arbitrary <*> bareExpr <*> body
    ]
    where
      body = Template <$> smaller (smallList arbitrary)
      inverseChain = smaller $ sized goInverse
      goInverse 0 = inverse
      goInverse n = oneof [inverse, inverseChain' n]
      inverseChain' n = fmap (Template . (:[])) $
        InverseChain <$> arbitrary <*> bareExpr <*> arbitrary <*> body <*> goInverse (n `div` 2)
      inverse = fmap (Template . (:[])) $ Inverse <$> arbitrary <*> body
      expmay = elements [Just, const Nothing] <*> bareExpr
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

instance Arbitrary Literal where
  arbitrary = oneof [
      PathL <$> arbitrary
    , DataL <$> arbitrary
    , StringL <$> arbitrary `suchThat` validString
    , NumberL <$> arbitrary
    , BooleanL <$> arbitrary
    , pure NullL
    ]
  shrink = \case
    StringL t -> StringL <$> filter validString (shrink t)
    other -> recursivelyShrink other

instance Arbitrary BlockParams where
  arbitrary = BlockParams <$> listOf1 name
    where
      -- A 'simple' name, i.e. an ID without path components
      name = fmap PathL . PathID <$> arbitrary `suchThat` validId <*> pure Nothing
  shrink (BlockParams ps) = BlockParams <$> filter (not . null) (shrinkList shrink ps)

instance Arbitrary DataPath where
  arbitrary = DataPath <$> arbitrary
  shrink (DataPath p) = DataPath <$> shrink p

instance Arbitrary Path where
  arbitrary = oneof [
        oneof [dot, dotdot] <*> sized slashrest
      , oneof [reg, seg] <*> sized rest
      ]
    where
      paths 0 = oneof [reg, seg] <*> pure Nothing
      paths n = oneof [reg, seg] <*> rest (n - 1)
      rest 0 = pure Nothing
      rest n = fmap Just . (,) <$> elements ['.', '/'] <*> paths n
      slashrest n = fmap Just . (,) <$> pure '/' <*> paths n
      reg = PathID <$> arbitrary `suchThat` validId
      seg = PathSeg <$> arbitrary `suchThat` validSegId
      dot = pure (PathID ".")
      dotdot = pure (PathID "..")
  shrink = \case
    PathID _ Nothing -> []
    PathSeg _ Nothing -> []
    PathID t ts -> (PathID <$> filter validId (shrink t) <*> pure ts) <> (PathID t <$> trim ts)
    PathSeg t ts -> (PathSeg <$> filter validSegId (shrink t) <*> pure ts) <> (PathSeg t <$> trim ts)
    where
      trim Nothing = [Nothing]
      trim (Just (s, p)) = Just . (,) s <$> shrink p

instance Arbitrary Hash where
  arbitrary = Hash <$> smallList arbitrary
  shrink (Hash hps) = Hash <$> shrinkList shrink hps

instance Arbitrary HashPair where
  arbitrary = HashPair <$> arbitrary `suchThat` validId <*> smaller arbitrary
  shrink (HashPair t e) = HashPair <$> filter validId (shrink t) <*> shrink e

instance Arbitrary Fmt where
  arbitrary = Fmt <$> arbitrary <*> arbitrary

instance Arbitrary Format where
  arbitrary = elements [Strip, Verbatim]

simpleId :: Gen Text
simpleId = arbitrary `suchThat` validId

bareExpr :: Gen Expr
bareExpr = SExp <$> arbitrary <*> smaller (smallList arbitrary) <*> smaller arbitrary

smallList :: Gen a -> Gen [a]
smallList g = sized go
  where
    go 0 = pure []
    go n = (:) <$> g <*> go (n `div` 2)

smaller :: Gen a -> Gen a
smaller g = sized $ \s -> resize (s `div` 2) g

merge :: Template -> Template
merge (Template ps) = Template (go ps)
  where
    go (ContentStmt t1 : ContentStmt t2 : xs) = go (ContentStmt (t1 <> t2) : xs)
    go (x : xs) = x : go xs
    go [] = []

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

validSegId :: Text -> Bool
validSegId t = and [noEscape t, noNull t, T.takeEnd 1 t /= "\\"]
  where noEscape tt = and (fmap escaped (splits tt))
        splits = T.breakOnAll "]"
        escaped (m, _) = and [T.takeEnd 1 m == "\\", T.takeEnd 2 m /= "\\\\"]

rawContent :: Gen Text
rawContent = do
  ts <- sized $ \s -> resize (min s 5) arbitrary
  pure (templateToText ts)

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
validWeakComment t = and [noNull t, noMustaches t, noMustacheClose t, T.takeEnd 1 t /= "}", T.take 2 t /= "--"]
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
