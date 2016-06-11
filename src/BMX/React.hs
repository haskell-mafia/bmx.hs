{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.React (
    renderReact
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           BMX.Data

import           P

renderReact :: Template -> Text
renderReact (Template ss) =
  foldMap renderReactStmt ss

renderReactTemplate :: Template -> Text
renderReactTemplate (Template ss) =
  "[" <> (T.intercalate ", " . fmap renderReactStmt) ss <> "]"

renderReactStmt :: Positioned Stmt -> Text
renderReactStmt (stmt :@ _) = case stmt of
  ContentStmt (t :@ _) ->
    "'" <> t <> "'"
  CommentStmt _ (comment :@ _) ->
    "/*" <> comment <> "*/"
  Mustache _ (v :@ _) ->
    renderReactExpr v
  MustacheUnescaped (Fmt l r) (e :@ _) ->
    -- FIX Be nice not to have to create a span tag
    "React.createElement('span', {dangerouslySetInnerHTML: {__html: " <> renderReactExpr e <> "}})";
  Block _ _ e  bp b i ->
    renderBlock e b i
  Inverse _ (p :@ _) ->
    -- FIX What happens to inverse?
    renderReactTemplate p
  InverseBlock _ _ e bp b i ->
    renderBlock e i b

  -- Treat this as a block too, although it lacks the lower formatting
  InverseChain _ e bp b i ->
    renderBlock e b i

  -- Special handler that resolves and inlines the partial
  PartialStmt (Fmt l r) e@(_ :@ el) ee h ->
    renderPartial e ee h (Template [] :@ el)

  -- Special handler that registers @partial-block, and fails over if partial not found
  PartialBlock (Fmt l1 r1) (Fmt l2 r2) e ee h b ->
    renderPartial e ee h b

  -- Special handler that treats it as a regular block with a single ContentStmt
  RawBlock (e :@ _) (b :@ _) ->
    "helpers['" <> renderReactExpr e <> "']("
      -- FIX Is this right, what happens if this is raw html?
      <> "function() { return " <> b <> " },"
      <> "function() { return [] }"
      <> ")"

  -- Decorators are handled in a first pass, so here they are mere formatting
  DecoratorStmt (Fmt l r) a ->
    error "Decorators not implemented "

  DecoratorBlock (Fmt l _) (Fmt _ r) _ _ ->
    error "Decorators not implemented "

  Tag (n :@ _) attr (b :@ _) ->
    let
      as = T.intercalate ", " . fmap (\(Attribute k v) -> k <> ": " <> "'" <> v <> "'") . fmap depo $ attr
    in
      "React.createElement('" <> n <> "', { " <> as <> "}, " <> renderReactTemplate b <> ");"

renderBlock :: Positioned Expr -> Positioned Template -> Positioned Template -> Text
renderBlock (e :@ _) (b :@ _) (i :@ _) =
  "helpers['" <> renderReactExpr e <> "']("
    <> "function(data) { return " <> renderReactTemplate b <> " },"
    <> "function(data) { return " <> renderReactTemplate i <> " }"
    <> ")"

renderPartial :: Positioned Expr -> Maybe (Positioned Expr) -> Positioned Hash -> Positioned Template -> Text
renderPartial (e :@ _) ee (Hash hash :@ _) (b :@ _) =
  "helpers['" <> renderReactExpr e <> "']("
    <> maybe "null" (\(ee' :@ _) -> renderReactExpr ee') ee <> ", "
    <> "{" <> (T.intercalate ", " . fmap (\((HashPair (k :@ _) (v :@ _)) :@ _) -> k <> ": " <> renderReactExpr v)) hash <> "}, "
    <> renderReactTemplate b
    <> ")"

renderReactExpr :: Expr -> Text
renderReactExpr e = case e of
  Lit (l :@ _) ->
    renderReactLiteral l
  SExp (l :@ _) es h ->
    renderReactLiteral l

renderReactLiteral :: Literal -> Text
renderReactLiteral l = case l of
  PathL p ->
    -- TODO Is used by helpers, which is wrong!
    "args." <> renderReactPath p
  DataL (DataPath dp) ->
    -- Data is _only_ passed in from block callbacks
    "data." <> renderReactPath dp
  StringL t ->
    t
  NumberL i ->
    T.pack . show $ i
  BooleanL b ->
    case b of
      False ->
        "false"
      True ->
        "true"
  NullL ->
    "null"

renderReactPath :: Path -> Text
renderReactPath p = case p of
  -- TODO "../"
  PathID t m ->
    t <> maybe "" (\(c, p') -> T.singleton c <> renderReactPath p') m
  PathSeg t m ->
    t <> maybe "" (\(c, p') -> T.singleton c <> renderReactPath p') m
