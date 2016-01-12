{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.AST where

import           Data.Data
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics

import           BMX.Data.Token

import           P

newtype Program = Program [Stmt]
  deriving (Show, Eq, Generic, Data, Typeable)

data Stmt
  = Mustache Fmt Expr
  | MustacheUnescaped Fmt Expr
  | Partial Fmt Expr (Maybe Expr)
  | PartialBlock Fmt Fmt Expr (Maybe Expr) Program
  | Block Fmt Fmt Expr (Maybe BlockParams) Program (Maybe Stmt)
  | Inverse Fmt Program
  | InverseChain Fmt Expr (Maybe BlockParams) Program (Maybe Stmt)
  | InverseBlock Fmt Fmt Expr (Maybe BlockParams) Program (Maybe Stmt)
  | RawBlock Expr Text
  | ContentStmt Text
  | CommentStmt Fmt Text
  | Decorator Fmt Expr
  | DecoratorBlock Fmt Fmt Expr Program
  deriving (Show, Eq, Generic, Data, Typeable)

data Expr
  = Lit Literal
  | SExp Literal [Expr] Hash
  deriving (Show, Eq, Generic, Data, Typeable)

data Literal
  = PathL Path
  | StringL Text
  | NumberL Integer
  | BooleanL Bool
  | UndefinedL
  | NullL
  deriving (Show, Eq, Generic, Data, Typeable)

data BlockParams = BlockParams [Literal]
  deriving (Show, Eq, Generic, Data, Typeable)

data Path
  = Path [PathComponent]
  | DataPath [PathComponent]
  deriving (Show, Eq, Generic, Data, Typeable)

data PathComponent
  = PathID Text
  | PathSegment Text
  | PathSep Char
  deriving (Show, Eq, Generic, Data, Typeable)

data Hash = Hash [HashPair]
  deriving (Show, Eq, Generic, Data, Typeable)

data HashPair = HashPair Text Expr
  deriving (Show, Eq, Generic, Data, Typeable)

data Fmt = Fmt Format Format
  deriving (Show, Eq, Generic, Data, Typeable)

emptyHash :: Hash
emptyHash = Hash []

renderPath :: Path -> Text
renderPath (Path pcs) = foldMap renderPathComponent pcs
renderPath (DataPath pcs) = "@" <> foldMap renderPathComponent pcs

renderPathComponent :: PathComponent -> Text
renderPathComponent = \case
  PathID t -> t
  PathSep c -> T.singleton c
  PathSegment t -> "[" <> t <> "]"

renderLiteral :: Literal -> Text
renderLiteral = \case
  PathL p    -> renderPath p
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
  Partial (Fmt l r) e ctx ->
    openFormat l <> ">" <> renderExpr e
       <> " " <> maybe T.empty renderBareExpr ctx
       <> closeFormat r
  PartialBlock (Fmt l1 r1) (Fmt l2 r2) e ctx body ->
    openFormat l1 <> "#>" <> renderExpr e
      <> " " <> maybe T.empty renderBareExpr ctx
      <> closeFormat r1
      <> renderProgram body
      <> closeBlock l2 r2 e
  Block (Fmt l1 r1) (Fmt l2 r2) e bparams body inverse ->
    openFormat l1 <> "#" <> renderBareExpr e <> blockParams bparams <> closeFormat r1
      <> renderProgram body
      <> inverseMay inverse
      <> closeBlock l2 r2 e
  Inverse (Fmt l r) body ->
    openFormat l <> "^" <> closeFormat r <> renderProgram body
  InverseChain (Fmt l r) e bparams body inverse ->
    openFormat l <> "else " <> renderBareExpr e <> blockParams bparams <> closeFormat r
      <> renderProgram body
      <> inverseMay inverse
  InverseBlock (Fmt l1 r1) (Fmt l2 r2) e bparams body inverse ->
    openFormat l1 <> "^" <> renderBareExpr e <> blockParams bparams <> closeFormat r1
      <> renderProgram body
      <> inverseMay inverse
      <> closeBlock l2 r2 e
  RawBlock e content ->
    "{{{{" <> renderBareExpr e <> "}}}}" <> content <> "{{{{/" <> exprHelper e <> "}}}}"
  ContentStmt content -> content
  CommentStmt (Fmt l r) comment ->
    openFormat l <> "!--" <> comment <> "--" <> closeFormat r
  Decorator (Fmt l r) e ->
    openFormat l <> "*" <> renderBareExpr e <> closeFormat r
  DecoratorBlock (Fmt l1 r1) (Fmt l2 r2) e body ->
    openFormat l1 <> "#*" <> renderBareExpr e <> closeFormat r1
      <> renderProgram body
      <> closeBlock l2 r2 e
  where
    openFormat f = "{{" <> renderFormat f
    closeFormat f = renderFormat f <> "}}"
    exprHelper (Lit lit) = renderLiteral lit
    exprHelper (SExp lit _ _) = renderLiteral lit
    blockParams = maybe T.empty renderBlockParams
    inverseMay = maybe T.empty renderStmt
    closeBlock l r e = openFormat l <> "/" <> exprHelper e <> closeFormat r

renderProgram :: Program -> Text
renderProgram (Program ss) = foldMap renderStmt ss
