{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.AST where

import           Data.Text (Text)
import qualified Data.Text as T

import           BMX.Data.Token

import           P

newtype Program = Program { unTemplate :: [Stmt] }
  deriving (Show, Eq)

data Stmt
  = Mustache Fmt Expr
  | MustacheUnescaped Fmt Expr
  | Partial Fmt Expr
  | PartialBlock Fmt Fmt Expr [Stmt]
  | Block Fmt Fmt Expr (Maybe BlockParams) [Stmt] (Maybe Stmt)
  | Inverse Fmt [Stmt]
  | InverseChain Fmt Expr (Maybe BlockParams) [Stmt] (Maybe Stmt)
  | InverseBlock Fmt Fmt Expr (Maybe BlockParams) [Stmt] (Maybe Stmt)
  | RawBlock Expr Text
  | ContentStmt Text
  | CommentStmt Fmt Text
  | Decorator Fmt Expr
  | DecoratorBlock Fmt Fmt Expr [Stmt]
  deriving (Show, Eq)

data Expr
  = Lit Literal
  | SExp Literal [Expr] Hash
  deriving (Show, Eq)

data Literal
  = PathL Path
  | StringL Text
  | NumberL Integer
  | BooleanL Bool
  | UndefinedL
  | NullL
  deriving (Show, Eq)

data BlockParams = BlockParams [Literal]
  deriving (Show, Eq)

data Path
  = Path [PathComponent]
  | DataPath [PathComponent]
  deriving (Show, Eq)

data PathComponent
  = PathID Text
  | PathSep Char
  deriving (Show, Eq)

data Hash = Hash [HashPair]
  deriving (Show, Eq)

data HashPair = HashPair Text Expr
  deriving (Show, Eq)

data Fmt = Fmt Format Format
  deriving (Show, Eq)

emptyHash :: Hash
emptyHash = Hash []

renderPath :: Path -> Text
renderPath (Path pcs) = foldMap renderPathComponent pcs
renderPath (DataPath pcs) = "@" <> foldMap renderPathComponent pcs

renderPathComponent :: PathComponent -> Text
renderPathComponent (PathID t)  = t
renderPathComponent (PathSep c) = T.singleton c

renderLiteral :: Literal -> Text
renderLiteral = \case
  PathL p    -> renderPath p
  StringL t  -> "\"" <> t <> "\""
  NumberL i  -> T.pack (show i)
  BooleanL b -> if b then "true" else "false"
  UndefinedL -> "undefined"
  NullL      -> "null"

renderBlockParams :: BlockParams -> Text
renderBlockParams (BlockParams ps) = " as |" <> T.intercalate " " (fmap renderLiteral ps) <> "|"

renderHash :: Hash -> Text
renderHash (Hash hps) = foldMap renderHashPair hps

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
  Partial (Fmt l r) e ->
    openFormat l <> ">" <> renderBareExpr e <> closeFormat r
  PartialBlock (Fmt l1 r1) (Fmt l2 r2) e body ->
    openFormat l1 <> "#>" <> renderBareExpr e <> closeFormat r1
      <> foldMap renderStmt body
      <> closeBlock l2 r2 e
  Block (Fmt l1 r1) (Fmt l2 r2) e bparams body inverse ->
    openFormat l1 <> "#" <> renderBareExpr e <> blockParams bparams <> closeFormat r1
      <> foldMap renderStmt body
      <> inverseMay inverse
      <> closeBlock l2 r2 e
  Inverse (Fmt l r) body ->
    openFormat l <> "^" <> closeFormat r <> foldMap renderStmt body
  InverseChain (Fmt l r) e bparams body inverse ->
    openFormat l <> "else " <> renderBareExpr e <> blockParams bparams <> closeFormat r
      <> foldMap renderStmt body
      <> inverseMay inverse
  InverseBlock (Fmt l1 r1) (Fmt l2 r2) e bparams body inverse ->
    openFormat l1 <> "^" <> renderBareExpr e <> blockParams bparams <> closeFormat r1
      <> foldMap renderStmt body
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
      <> foldMap renderStmt body
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
