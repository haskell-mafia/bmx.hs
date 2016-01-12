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
  content                          { Content $$ }
  comment                          { Comment $$ }
  --
  open                             { Open $$ }
  open_block                        { OpenBlock $$ }
  open_end_block                     { OpenEndBlock $$ }
  open_unescaped                    { OpenUnescaped $$ }
  open_comment                      { OpenComment $$ }
  open_comment_block                 { OpenCommentBlock $$ }
  open_partial                      { OpenPartial $$ }
  open_partial_block                 { OpenPartialBlock $$ }
  open_decorator                    { OpenDecorator $$ }
  open_decorator_block               { OpenDecoratorBlock $$ }
  open_inverse                      { OpenInverse $$ }
  open_inverse_chain                 { OpenInverseChain $$ }
  --
  close                            { Close $$ }
  close_unescaped                   { CloseUnescaped $$ }
  close_comment_block                { CloseCommentBlock $$ }
  --
  string                           { String $$ }
  number                           { Number $$ }
  bool                             { Boolean $$ }
  undef                            { Undefined }
  nul                              { Null }
  ident                            { ID $$ }
  segment_id                        { SegmentID $$ }
  open_sexp                         { OpenSExp }
  close_sexp                        { CloseSExp }
  equals                           { Equals }
  data_sigil                        { Data }
  sep                              { Sep $$ }
  open_block_params                  { OpenBlockParams }
  close_block_params                 { CloseBlockParams }
  -- * Raw blocks
  raw_content                       { RawContent $$ }
  open_raw_block                     { OpenRawBlock }
  close_raw_block                    { CloseRawBlock }
  close_raw                         { CloseRaw $$ }

%%

Program :: { Program }:
    Statements                     { Program (reverse $1) }

Statements :: { [Stmt] }:
    Statements Statement           { $2 : $1 }
  | {- empty -}                    { [] }

Statement :: { Stmt }:
    content                        { ContentStmt $1 }
  | Comment                        { let (fmt, com) = $1 in CommentStmt fmt com }
  | Mustache                       { $1 }
  | Raw                            { $1 }
  | Block                          { $1 }
  | InverseBlock                   { $1 }
  | Partial                        { $1 }
  | Decorator                      { $1 }

Comment :: { (Fmt, Text) }:
  -- You'd think we could discard comments here, but they have formatting implications
    open_comment_block comment close_comment_block { (Fmt $1 $3, $2) }
  | open_comment comment close                  { (Fmt $1 $3, $2) }

Mustache :: { Stmt }:
    open BareExpr close                   { Mustache (Fmt $1 $3) $2 }
  | open_unescaped BareExpr close_unescaped { MustacheUnescaped (Fmt $1 $3) $2 }

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
Block :: { Stmt }:
    OpenBlock Statements InverseChain CloseBlock
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

OpenBlock :: { (Fmt, Expr, Maybe BlockParams) }:
    open_block BareExpr BlockParams close { (Fmt $1 $4, $2, $3) }

CloseBlock :: { (Fmt, Literal) }:
    open_end_block Literal close     { (Fmt $1 $3, $2) }

{-
  {{^if tuesday}}
  arbitrary handlebars content for days other than tuesday
  {{^}}
  a double-negative for when it is tuesday, just to confuse you
  {{/if}}

  note that you can't start an inverse block with {{else if tuesday}},
  though you could use {{else}} in place of {{^}}. ^ seems to ~mean 'not'

-}
InverseBlock :: { Stmt }:
    OpenInverseBlock Statements Inverse CloseBlock
      {%
          let (fmt1, exp1, bparams) = $1
              (fmt2, exp2) = $4
              expected = exprHelper exp1
              actual = litHelper exp2
          in  if expected == actual && isJust expected
              then return (InverseBlock fmt1 fmt2 exp1 bparams (prg $2) $3)
              else Left (blockError "Inverse block" expected actual)
      }

OpenInverseBlock :: { (Fmt, Expr, Maybe BlockParams) }:
    open_inverse BareExpr BlockParams close { (Fmt $1 $4, $2, $3) }

{-
  {{^}}
  {{else}}

  Mustache statement inside a block. If a block's main expression evaluates to falsey,
  everything after its Inverse is evaluated instead.
  An Inverse terminates the block, i.e. it can't be followed by a chained inverse.
-}
Inverse :: { Maybe Stmt }:
    open_inverse close Statements      { Just $ Inverse (Fmt $1 $2) (prg $3) }
  | open_inverse_chain close Statements { Just $ Inverse (Fmt $1 $2) (prg $3) }
  | {- empty -}                       { Nothing }

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
InverseChain :: { Maybe Stmt }:
    OpenInverseChain Statements InverseChain
      {
        let (fmt, expr, bparams) = $1
        in  Just $ InverseChain fmt expr bparams (prg $2) $3
      }
  | Inverse                           { $1 }

OpenInverseChain :: { (Fmt, Expr, Maybe BlockParams) }:
    open_inverse_chain BareExpr BlockParams close { (Fmt $1 $4, $2, $3) }

-- -----------------------------------------------------------------------------
-- Partials

{-
  {{> partial }} regular mustache style inline partial
  {{#> (partial block) abc=def}}
  arbitrary handlebars content inside a partial block
  {{/partial}}
  {{> partial withContext abc=def}}
-}
Partial :: { Stmt }:
    open_partial Expr BareExpr close     { Partial (Fmt $1 $4) $2 (Just $3) }
  | open_partial Expr close              { Partial (Fmt $1 $3) $2 Nothing }
  | PartialBlock                   { $1 }

PartialBlock :: { Stmt }:
    OpenPartialBlock Statements CloseBlock
      {%
          let (fmt1, exp1, ctx) = $1
              (fmt2, exp2) = $3
              expected = exprHelper exp1
              actual = litHelper exp2
          in  if expected == actual && isJust expected
              then return (PartialBlock fmt1 fmt2 exp1 ctx (prg $2))
              else Left (blockError "Partial block" expected actual)
      }

OpenPartialBlock :: { (Fmt, Expr, Maybe Expr) }:
    open_partial_block Expr BareExpr close { (Fmt $1 $4, $2, Just $3) }
  | open_partial_block Expr close          { (Fmt $1 $3, $2, Nothing) }

-- -----------------------------------------------------------------------------
-- Decorators

Decorator :: { Stmt }:
    open_decorator BareExpr close   { Decorator (Fmt $1 $3) $2 }
  | DecoratorBlock                 { $1 }

DecoratorBlock :: { Stmt }:
    OpenDecoratorBlock Statements CloseBlock
      {%
          let (fmt1, exp1) = $1
              (fmt2, exp2) = $3
              expected = exprHelper exp1
              actual = litHelper exp2
          in  if expected == actual && isJust expected
              then return (DecoratorBlock fmt1 fmt2 exp1 (prg $2))
              else Left (blockError "Decorator block" expected actual)
      }

OpenDecoratorBlock :: { (Fmt, Expr) }:
    open_decorator_block BareExpr close { (Fmt $1 $3, $2) }

-- -----------------------------------------------------------------------------
-- Raw blocks

Raw :: { Stmt }:
    open_raw_block BareExpr close_raw_block raw_content close_raw
      {%
         let helperName = exprHelper $2 in
         if helperName == Just $5
         then return $ RawBlock $2 $4
         else Left (rawBlockError helperName $5)
      }

-- -----------------------------------------------------------------------------
-- Exprs, literals, atoms

BareExpr :: { Expr }:
    -- A SExp without parentheses.
    Literal Exprs Hash             { SExp $1 (reverse $2) $3 }

Expr :: { Expr }:
    open_sexp BareExpr close_sexp    { $2 }
  | Literal                        { Lit $1 }

Exprs :: { [Expr] }:
    Exprs Expr                     { $2 : $1 }
  | {- empty -}                    { [] }

Literal :: { Literal }:
    string                         { StringL $1 }
  | number                         { NumberL $1 }
  | bool                           { BooleanL $1 }
  | undef                          { UndefinedL }
  | nul                            { NullL }
  | Path                           { PathL $1 }

Path :: { Path }:
    data_sigil PathComponents       { DataPath $2 }
  | PathComponents                 { Path $1 }

PathComponents :: { [PathComponent] }:
    ident sep PathComponents       { PathID $1 : PathSep $2 : $3 }
  | ident                          { PathID $1 : [] }
  | segment_id sep PathComponents  { PathSegment $1 : PathSep $2 : $3 }
  | segment_id                     { PathSegment $1 : [] }

Hash :: { Hash }:
    HashPairs                      { Hash $1 }

HashPairs :: { [HashPair] }:
    ident equals Expr HashPairs    { HashPair $1 $3 : $4 }
  | {- empty -}                    { [] }

BlockParams :: { Maybe BlockParams }:
    open_block_params Names close_block_params { Just $ BlockParams (reverse $2) }
  | {- empty -}                    { Nothing }

Names :: { [Literal] }:
  -- Used only in BlockParams, where the LHS of each pair must be a 'simple' name
    SimpleID                       { [$1] }
  | Names SimpleID                 { $2 : $1 }

SimpleID :: { Literal }:
    ident                          { PathL (Path [PathID $1]) }

{

newtype ParseError = ParseError { renderParseError :: Text }
  deriving (Eq, Show)

parseError ts = Left . ParseError $ "Parse error at token " <> T.pack (show (headMay ts))

prg :: [Stmt] -> Program
prg = Program . reverse

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
