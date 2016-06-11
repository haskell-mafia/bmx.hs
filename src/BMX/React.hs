{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.React (
    renderReactFile
  , renderReact
  ) where

import qualified Data.Char as C
import           Data.Text (Text)
import qualified Data.Text as T

import           BMX.Data

import           P

renderReactFile :: Text -> Template -> Text
renderReactFile n t =
     "\"use strict\";\n"
  <> "\n"
  <> "exports." <> n <> " = function(context, args, data) {"
       <> "  return " <> renderReact t <> ";"
       <> "};\n"

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
  Mustache _ ((SExp (l :@ _) _ _) :@ _) ->
    renderReactLiteral l
  MustacheUnescaped (Fmt l r) (e :@ _) ->
    -- FIX Be nice not to have to create a span tag
    "React.createElement('span', {dangerouslySetInnerHTML: {__html: " <> renderReactExpr e <> "}})";
  Block _ _ e bp b i ->
    renderBlock e bp b i
  Inverse _ (p :@ _) ->
    -- FIX What happens to inverse?
    renderReactTemplate p
  InverseBlock _ _ e bp b i ->
    renderBlock e bp i b

  -- Treat this as a block too, although it lacks the lower formatting
  InverseChain _ e bp b i ->
    renderBlock e bp b i

  -- Special handler that resolves and inlines the partial
  PartialStmt (Fmt l r) e@(_ :@ el) ee h ->
    renderPartial e ee h (Template [] :@ el)

  -- Special handler that registers @partial-block, and fails over if partial not found
  PartialBlock (Fmt l1 r1) (Fmt l2 r2) e ee h b ->
    renderPartial e ee h b

  -- Special handler that treats it as a regular block with a single ContentStmt
  RawBlock (e :@ _) (b :@ _) ->
    "context.helpers['" <> renderReactExpr e <> "']("
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

-- FIX This can't be anything but SExp
renderBlock :: Positioned Expr -> Maybe (Positioned BlockParams) -> Positioned Template -> Positioned Template -> Text
renderBlock ((SExp l es _) :@ _) bp (b :@ _) (i :@ _) =
  let
    args =
      maybe "args" (\(BlockParams bp' :@ _) -> T.intercalate ", " . fmap (\(l :@ _) -> renderReactLiteral l) $ bp') bp
  in
    -- TODO: Worth making these helpers return a function that takes _more_ arguments?
    -- This would mean not passing in everything to renderSExp
    renderSExp l es . Just $
         "function(" <> args <> ", data) { return " <> renderReactTemplate b <> " }, "
      <> "function(args, data) { return " <> renderReactTemplate i <> " }"

renderPartial :: Positioned Expr -> Maybe (Positioned Expr) -> Positioned Hash -> Positioned Template -> Text
renderPartial (e :@ _) ee (Hash hash :@ _) (b :@ _) =
  "context.partials['" <> renderReactExpr e <> "']("
    <> "context, "
    -- FIX We'll need to make sure _.extend is available in some fashion.
    -- What's the best way to do that? Define our own version somewhere?
    <> "_.extend({}, "
      <> maybe "{}" (\(ee' :@ _) -> renderReactExpr ee') ee <> ", "
      <> "{" <> (T.intercalate ", " . fmap (\((HashPair (k :@ _) (v :@ _)) :@ _) -> k <> ": " <> renderReactExpr v)) hash <> "}"
    <> "), "
    <> "{ 'partial-body': " <> renderReactTemplate b <> " }"
    <> ")"

renderReactExpr :: Expr -> Text
renderReactExpr e = case e of
  Lit (l :@ _) ->
    renderReactLiteral l
  SExp l es h ->
    renderSExp l es Nothing

renderSExp :: Positioned Literal -> [Positioned Expr] -> Maybe Text -> Text
renderSExp (l :@ _) es b =
  "context.helpers" <> renderJsArg (renderReactLiteral l) <> "("
    <> "[" <> (T.intercalate ", " . fmap (\(e :@ _) -> renderReactExpr e)) es <> "]"
    <> maybe "" (", " <>) b
    <> ")"

renderReactLiteral :: Literal -> Text
renderReactLiteral l = case l of
  PathL p ->
    -- TODO Is used by helpers, which is wrong!
    renderReactPath p
  DataL (DataPath dp) ->
    -- Data is _only_ passed in from block callbacks
    "data" <> renderJsArg (renderReactPath dp)
  StringL t ->
    "'" <> t <> "'"
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

-- Pure vanity, render arguments nicely if possible
renderJsArg :: Text -> Text
renderJsArg a =
  case T.all C.isAlpha a of
    True ->
      "." <> a
    False ->
      "['" <> a <> "']"

renderReactPath :: Path -> Text
renderReactPath p = case p of
  -- TODO "../"
  PathID t m ->
    t <> maybe "" (\(c, p') -> T.singleton c <> renderReactPath p') m
  PathSeg t m ->
    t <> maybe "" (\(c, p') -> T.singleton c <> renderReactPath p') m
