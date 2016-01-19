{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.AST where

import           Data.Text (Text)
import qualified Data.Text as T

import           BMX.Data.Token

import           P

newtype Template = Template [Stmt]
  deriving (Show, Eq)

data Stmt
  = Mustache Fmt Expr
  | MustacheUnescaped Fmt Expr
  | PartialStmt Fmt Expr (Maybe Expr)
  | PartialBlock Fmt Fmt Expr (Maybe Expr) Template
  | Block Fmt Fmt Expr BlockParams Template Template
  | Inverse Fmt Template
  | InverseChain Fmt Expr BlockParams Template Template
  | InverseBlock Fmt Fmt Expr BlockParams Template Template
  | RawBlock Expr Text
  | ContentStmt Text
  | CommentStmt Fmt Text
  | DecoratorStmt Fmt Expr
  | DecoratorBlock Fmt Fmt Expr Template
  deriving (Show, Eq)

data Expr
  = Lit Literal
  | SExp Literal [Expr] Hash
  deriving (Show, Eq)

data Literal
  = PathL Path
  | DataL DataPath
  | StringL Text
  | NumberL Integer
  | BooleanL Bool
  | UndefinedL
  | NullL
  deriving (Show, Eq)

data BlockParams = BlockParams [Literal]
  deriving (Show, Eq)

instance Monoid BlockParams where
  mempty = BlockParams []
  mappend (BlockParams a) (BlockParams b) = BlockParams (mappend a b)

data Path
  = PathID Text (Maybe (Char, Path))
  | PathSeg Text (Maybe (Char, Path))
  deriving (Show, Eq)

data DataPath = DataPath Path
  deriving (Show, Eq)

data Hash = Hash [HashPair]
  deriving (Show, Eq)

instance Monoid Hash where
  mempty = Hash []
  mappend (Hash a) (Hash b) = Hash (a <> b)

data HashPair = HashPair Text Expr
  deriving (Show, Eq)

data Fmt = Fmt Format Format
  deriving (Show, Eq)

emptyHash :: Hash
emptyHash = Hash []

renderPath :: Path -> Text
renderPath = \case
  PathID t ts -> t <> maybe T.empty (\(s, p) -> T.cons s (renderPath p)) ts
  PathSeg t ts -> "[" <> t <> "]" <> maybe T.empty (\(s, p) -> T.cons s (renderPath p)) ts

renderDataPath :: DataPath -> Text
renderDataPath (DataPath p) = "@" <> renderPath p

renderLiteral :: Literal -> Text
renderLiteral = \case
  PathL p    -> renderPath p
  DataL p    -> renderDataPath p
  StringL t  -> "\"" <> T.replace "\"" "\\\"" t <> "\""
  NumberL i  -> T.pack (show i)
  BooleanL b -> if b then "true" else "false"
  UndefinedL -> "undefined"
  NullL      -> "null"

renderBlockParams :: BlockParams -> Text
renderBlockParams (BlockParams ps) = " as |" <> T.intercalate " " (fmap renderLiteral ps) <> "|"

renderHash :: Hash -> Text
renderHash (Hash hps) = T.intercalate " " (fmap renderHashPair hps)

renderHashPair :: HashPair -> Text
renderHashPair (HashPair t e) = t <> " = " <> renderExpr e

renderExpr :: Expr -> Text
renderExpr (Lit l) = renderLiteral l
renderExpr e@(SExp _ _ _) = "(" <> renderBareExpr e <> ")"

renderBareExpr :: Expr -> Text
renderBareExpr (Lit l) = renderLiteral l
renderBareExpr (SExp l es hash) =
  T.intercalate " " (renderLiteral l : fmap renderExpr es <> [renderHash hash])

renderStmt :: Stmt -> Text
renderStmt = \case
  Mustache (Fmt l r) e ->
    openFormat l <> renderBareExpr e <> closeFormat r
  MustacheUnescaped (Fmt l r) e ->
    openFormat l <> "{" <> renderBareExpr e <> "}" <> closeFormat r
  PartialStmt (Fmt l r) e ctx ->
    openFormat l <> ">" <> renderExpr e
       <> " " <> maybe T.empty renderBareExpr ctx
       <> closeFormat r
  PartialBlock (Fmt l1 r1) (Fmt l2 r2) e ctx body ->
    openFormat l1 <> "#>" <> renderExpr e
      <> " " <> maybe T.empty renderBareExpr ctx
      <> closeFormat r1
      <> renderTemplate body
      <> closeBlock l2 r2 e
  Block (Fmt l1 r1) (Fmt l2 r2) e bparams body inverse ->
    openFormat l1 <> "#" <> renderBareExpr e <> blockParams bparams <> closeFormat r1
      <> renderTemplate body
      <> inverseMay inverse
      <> closeBlock l2 r2 e
  Inverse (Fmt l r) body ->
    openFormat l <> "^" <> closeFormat r <> renderTemplate body
  InverseChain (Fmt l r) e bparams body inverse ->
    openFormat l <> "else " <> renderBareExpr e <> blockParams bparams <> closeFormat r
      <> renderTemplate body
      <> inverseMay inverse
  InverseBlock (Fmt l1 r1) (Fmt l2 r2) e bparams body inverse ->
    openFormat l1 <> "^" <> renderBareExpr e <> blockParams bparams <> closeFormat r1
      <> renderTemplate body
      <> inverseMay inverse
      <> closeBlock l2 r2 e
  RawBlock e content ->
    "{{{{" <> renderBareExpr e <> "}}}}" <> content <> "{{{{/" <> exprHelper e <> "}}}}"
  ContentStmt content -> content
  CommentStmt (Fmt l r) comment ->
    openFormat l <> "!--" <> comment <> "--" <> closeFormat r
  DecoratorStmt (Fmt l r) e ->
    openFormat l <> "*" <> renderBareExpr e <> closeFormat r
  DecoratorBlock (Fmt l1 r1) (Fmt l2 r2) e body ->
    openFormat l1 <> "#*" <> renderBareExpr e <> closeFormat r1
      <> renderTemplate body
      <> closeBlock l2 r2 e
  where
    openFormat f = "{{" <> renderFormat f
    closeFormat f = renderFormat f <> "}}"
    exprHelper (Lit lit) = renderLiteral lit
    exprHelper (SExp lit _ _) = renderLiteral lit
    blockParams = renderBlockParams
    inverseMay = renderTemplate
    closeBlock l r e = openFormat l <> "/" <> exprHelper e <> closeFormat r

renderTemplate :: Template -> Text
renderTemplate (Template ss) = foldMap renderStmt ss
