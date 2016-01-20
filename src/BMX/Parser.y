{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Parser where

import           Data.Either
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Safe (headMay)

import           BMX.Data

import           P

}

%name parse
%tokentype { Token }
%monad { Either ParseError }
%error { parseError }

%token
  content              { Content $$ }
  comment              { Comment $$ }
  --
  open                 { Open $$ }
  open_block           { OpenBlock $$ }
  open_end_block       { OpenEndBlock $$ }
  open_unescaped       { OpenUnescaped $$ }
  open_comment         { OpenComment $$ }
  open_comment_block   { OpenCommentBlock $$ }
  open_partial         { OpenPartial $$ }
  open_partial_block   { OpenPartialBlock $$ }
  open_decorator       { OpenDecorator $$ }
  open_decorator_block { OpenDecoratorBlock $$ }
  open_inverse         { OpenInverse $$ }
  open_inverse_chain   { OpenInverseChain $$ }
  --
  close                { Close $$ }
  close_unescaped      { CloseUnescaped $$ }
  close_comment_block  { CloseCommentBlock $$ }
  --
  string               { String $$ }
  number               { Number $$ }
  bool                 { Boolean $$ }
  undef                { Undefined }
  nul                  { Null }
  ident                { ID $$ }
  segment_id           { SegmentID $$ }
  open_sexp            { OpenSExp }
  close_sexp           { CloseSExp }
  equals               { Equals }
  data_sigil           { Data }
  sep                  { Sep $$ }
  open_block_params    { OpenBlockParams }
  close_block_params   { CloseBlockParams }
  -- * Raw blocks
  raw_content          { RawContent $$ }
  open_raw_block       { OpenRawBlock }
  close_raw_block      { CloseRawBlock }
  close_raw            { CloseRaw $$ }

%%

template :: { Template }:
    statements                     { Template (reverse $1) }

statements :: { [Stmt] }:
    statements statement           { $2 : $1 }
  | {- empty -}                    { [] }

statement :: { Stmt }:
    content                        { ContentStmt $1 }
  | do_comment                     { let (fmt, com) = $1 in CommentStmt fmt com }
  | mustache                       { $1 }
  | raw                            { $1 }
  | block                          { $1 }
  | inverse_block                  { $1 }
  | partial                        { $1 }
  | decorator                      { $1 }

do_comment :: { (Fmt, Text) }:
  -- You'd think we could discard comments here, but they have formatting implications
    open_comment_block comment close_comment_block { (Fmt $1 $3, $2) }
  | open_comment comment close                     { (Fmt $1 $3, $2) }

mustache :: { Stmt }:
    open bare_expr close                     { Mustache (Fmt $1 $3) $2 }
  | open_unescaped bare_expr close_unescaped { MustacheUnescaped (Fmt $1 $3) $2 }

-- -----------------------------------------------------------------------------
-- Blocks

{-
  {{# blockHelper arguments abc=def as |block params|}}
  arbitrary handlebars content here, incl. nested blocks
  the helper name in closing tag must precisely match opening tag
  {{else}}
  if blockHelper expression above is falsey, we'll get this output instead.
  'falsey' condition is part of Helper definition, i.e. a custom thing per helper
  {{/blockHelper}}
-}
block :: { Stmt }:
    do_open_block statements inverse_chain do_close_block
      {%
          -- Match the helpername with the closing block
          let (fmt1, exp1, bparams) = $1
              (fmt2, exp2) = $4
              expected = exprHelper exp1
              actual = litHelper exp2
          in  if expected == actual && isJust expected
              then return (Block fmt1 fmt2 exp1 bparams (prg $2) $3)
              else Left (blockError "Block" expected actual)
      }

do_open_block :: { (Fmt, Expr, BlockParams) }:
    open_block bare_expr block_params close { (Fmt $1 $4, $2, $3) }

do_close_block :: { (Fmt, Literal) }:
    open_end_block literal close     { (Fmt $1 $3, $2) }

{-
  {{^if tuesday}}
  arbitrary handlebars content for days other than tuesday
  {{^}}
  a double-negative for when it is tuesday, just to confuse you
  {{/if}}

  note that you can't start an inverse block with {{else if tuesday}},
  though you could use {{else}} in place of {{^}}. ^ seems to ~mean 'not'

-}
inverse_block :: { Stmt }:
    open_inverse_block statements inverse do_close_block
      {%
          let (fmt1, exp1, bparams) = $1
              (fmt2, exp2) = $4
              expected = exprHelper exp1
              actual = litHelper exp2
          in  if expected == actual && isJust expected
              then return (InverseBlock fmt1 fmt2 exp1 bparams (prg $2) $3)
              else Left (blockError "Inverse block" expected actual)
      }

open_inverse_block :: { (Fmt, Expr, BlockParams) }:
    open_inverse bare_expr block_params close { (Fmt $1 $4, $2, $3) }

{-
  {{^}}
  {{else}}

  Mustache statement inside a block. If a block's main expression evaluates to falsey,
  everything after its Inverse is evaluated instead.
  An Inverse terminates the block, i.e. it can't be followed by a chained inverse.
-}
inverse :: { Template }:
    open_inverse close statements          { Template [Inverse (Fmt $1 $2) (prg $3)] }
  | open_inverse_chain close statements    { Template [Inverse (Fmt $1 $2) (prg $3)] }
  | {- empty -}                            { Template [] }

{-
  {{# some block here }}
  content
  {{ else some other helper }}
  this is an inverse chain
  {{ else another expr to evaluate }}
  the chain continues
  {{^}}
  the final clause
  {{/some}}
-}
inverse_chain :: { Template }:
    do_open_inverse_chain statements inverse_chain
      {
        let (fmt, expr, bparams) = $1
        in  Template [InverseChain fmt expr bparams (prg $2) $3]
      }
  | inverse                           { $1 }

do_open_inverse_chain :: { (Fmt, Expr, BlockParams) }:
    open_inverse_chain bare_expr block_params close { (Fmt $1 $4, $2, $3) }

-- -----------------------------------------------------------------------------
-- Partials

{-
  {{> partial }} regular mustache style inline partial
  {{#> (partial block) abc=def}}
  arbitrary handlebars content inside a partial block
  {{/partial}}
  {{> partial withContext abc=def}}
-}
partial :: { Stmt }:
    open_partial expr expr hash close     { PartialStmt (Fmt $1 $5) $2 (Just $3) $4 }
  | open_partial expr hash close          { PartialStmt (Fmt $1 $4) $2 Nothing $3 }
  | partial_block                         { $1 }

partial_block :: { Stmt }:
    do_open_partial_block statements do_close_block
      {%
          let (fmt1, exp1, ctx, hash) = $1
              (fmt2, exp2) = $3
              expected = exprHelper exp1
              actual = litHelper exp2
          in  if expected == actual && isJust expected
              then return (PartialBlock fmt1 fmt2 exp1 ctx hash (prg $2))
              else Left (blockError "Partial block" expected actual)
      }

do_open_partial_block :: { (Fmt, Expr, Maybe Expr, Hash) }:
    open_partial_block expr expr hash close { (Fmt $1 $5, $2, (Just $3), $4) }
  | open_partial_block expr hash close      { (Fmt $1 $4, $2, Nothing, $3) }

-- -----------------------------------------------------------------------------
-- Decorators

decorator :: { Stmt }:
    open_decorator bare_expr close { DecoratorStmt (Fmt $1 $3) $2 }
  | decorator_block                 { $1 }

decorator_block :: { Stmt }:
    do_open_decorator_block statements do_close_block
      {%
          let (fmt1, exp1) = $1
              (fmt2, exp2) = $3
              expected = exprHelper exp1
              actual = litHelper exp2
          in  if expected == actual && isJust expected
              then return (DecoratorBlock fmt1 fmt2 exp1 (prg $2))
              else Left (blockError "Decorator block" expected actual)
      }

do_open_decorator_block :: { (Fmt, Expr) }:
    open_decorator_block bare_expr close { (Fmt $1 $3, $2) }

-- -----------------------------------------------------------------------------
-- Raw blocks

raw :: { Stmt }:
    open_raw_block bare_expr close_raw_block raw_content close_raw
      {%
         let helperName = exprHelper $2 in
         if helperName == Just $5
         then return $ RawBlock $2 $4
         else Left (rawBlockError helperName $5)
      }

-- -----------------------------------------------------------------------------
-- Exprs, literals, atoms

bare_expr :: { Expr }:
    -- A SExp without parentheses
    literal exprs hash               { SExp $1 (reverse $2) $3 }

expr :: { Expr }:
    open_sexp bare_expr close_sexp   { $2 }
  | literal                          { Lit $1 }

exprs :: { [Expr] }:
    exprs expr                       { $2 : $1 }
  | {- empty -}                      { [] }

literal :: { Literal }:
    string                           { StringL $1 }
  | number                           { NumberL $1 }
  | bool                             { BooleanL $1 }
  | undef                            { UndefinedL }
  | nul                              { NullL }
  | path                             { PathL $1 }
  | data_path                         { DataL $1 }

path :: { Path }:
    ident sep path                   { PathID $1 (Just ($2, $3)) }
  | ident                            { PathID $1 Nothing }
  | segment_id sep path              { PathSeg $1 (Just ($2, $3)) }
  | segment_id                       { PathSeg $1 Nothing }

data_path :: { DataPath }:
    data_sigil path                  { DataPath $2 }

hash :: { Hash }:
    hash_pairs                       { Hash $1 }

hash_pairs :: { [HashPair] }:
    ident equals expr hash_pairs   { HashPair $1 $3 : $4 }
  | {- empty -}                    { [] }

block_params :: { BlockParams }:
    open_block_params names close_block_params { BlockParams (reverse $2) }
  | {- empty -}                    { BlockParams [] }

names :: { [Literal] }:
  -- Used only in BlockParams, where the LHS of each pair must be a 'simple' name
    simple_id                       { [$1] }
  | names simple_id                 { $2 : $1 }

simple_id :: { Literal }:
    ident                          { PathL (PathID $1 Nothing) }

{

newtype ParseError = ParseError { renderParseError :: Text }
  deriving (Eq, Show)

parseError ts = Left . ParseError $ "Parse error at token " <> T.pack (show (headMay ts))

prg :: [Stmt] -> Template
prg = Template . reverse

-- Extract the helper name from an Expr
exprHelper :: Expr -> Maybe Text
exprHelper (SExp lit _ _) = litHelper lit
exprHelper (Lit lit) = litHelper lit

litHelper :: Literal -> Maybe Text
litHelper = Just . renderLiteral

rawBlockError :: Maybe Text -> Text -> ParseError
rawBlockError Nothing t = ParseError "Raw block: Invalid helper"
rawBlockError (Just t1) t2 = ParseError $
  "Raw block: Helper name mismatch (Expected " <> t1 <> ", got " <> t2 <> ")"

blockError :: Text -> Maybe Text -> Maybe Text -> ParseError
blockError t Nothing _ = ParseError $ t <> ": Invalid helper"
blockError t (Just t1) Nothing = ParseError $
  t <> ": Helper name mismatch (Expected " <> t1 <> ", got nothing)"
blockError t (Just t1) (Just t2) = ParseError $
  t <> ": Helper name mismatch (Expected " <> t1 <> ", got " <> t2 <> ")"

}
