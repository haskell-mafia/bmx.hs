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
  openBlock                        { OpenBlock $$ }
  openEndBlock                     { OpenEndBlock $$ }
  openUnescaped                    { OpenUnescaped $$ }
  openComment                      { OpenComment $$ }
  openCommentBlock                 { OpenCommentBlock $$ }
  openPartial                      { OpenPartial $$ }
  openPartialBlock                 { OpenPartialBlock $$ }
  openDecorator                    { OpenDecorator $$ }
  openDecoratorBlock               { OpenDecoratorBlock $$ }
  openInverse                      { OpenInverse $$ }
  openInverseChain                 { OpenInverseChain $$ }
  --
  close                            { Close $$ }
  closeUnescaped                   { CloseUnescaped $$ }
  closeCommentBlock                { CloseCommentBlock $$ }
  --
  string                           { String $$ }
  number                           { Number $$ }
  bool                             { Boolean $$ }
  undef                            { Undefined }
  nul                              { Null }
  ident                            { ID $$ }
  segmentId                        { SegmentID $$ }
  openSexp                         { OpenSExp }
  closeSexp                        { CloseSExp }
  equals                           { Equals }
  dataSigil                        { Data }
  sep                              { Sep $$ }
  openBlockParams                  { OpenBlockParams }
  closeBlockParams                 { CloseBlockParams }
  -- * Raw blocks
  rawContent                       { RawContent $$ }
  openRawBlock                     { OpenRawBlock }
  closeRawBlock                    { CloseRawBlock }
  closeRaw                         { CloseRaw $$ }

%%

Program :
    Statements                     { Program (reverse $1) }

Statements :
    Statements Statement           { $2 : $1 }
  | {- empty -}                    { [] }

Statement :
    content                        { ContentStmt $1 }
  | Comment                        { let (fmt, com) = $1 in CommentStmt fmt com }
  | Mustache                       { $1 }
  | Raw                            { $1 }
  | Block                          { $1 }
  | InverseBlock                   { $1 }
  | Partial                        { $1 }
  | Decorator                      { $1 }

Comment :
  -- You'd think we could discard comments here, but they have formatting implications
    openCommentBlock comment closeCommentBlock { (Fmt $1 $3, $2) }
  | openComment comment close                  { (Fmt $1 $3, $2) }

Mustache :
    open BareExpr close                   { Mustache (Fmt $1 $3) $2 }
  | openUnescaped BareExpr closeUnescaped { MustacheUnescaped (Fmt $1 $3) $2 }

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
Block :
    OpenBlock Statements InverseChain CloseBlock
      {%
          -- Match the helpername with the closing block
          let (fmt1, exp1, bparams) = $1
              (fmt2, exp2) = $4
              expected = exprHelper exp1
              actual = litHelper exp2
          in  if expected == actual && isJust expected
              then return (Block fmt1 fmt2 exp1 bparams (reverse $2) $3)
              else Left (blockError "Block" expected actual)
      }

OpenBlock :
    openBlock BareExpr BlockParams close { (Fmt $1 $4, $2, $3) }

CloseBlock :
    openEndBlock Literal close     { (Fmt $1 $3, $2) }

{-
  {{^if tuesday}}
  arbitrary handlebars content for days other than tuesday
  {{^}}
  a double-negative for when it is tuesday, just to confuse you
  {{/if}}

  note that you can't start an inverse block with {{else if tuesday}},
  though you could use {{else}} in place of {{^}}. ^ seems to ~mean 'not'

-}
InverseBlock :
    OpenInverseBlock Statements Inverse CloseBlock
      {%
          let (fmt1, exp1, bparams) = $1
              (fmt2, exp2) = $4
              expected = exprHelper exp1
              actual = litHelper exp2
          in  if expected == actual && isJust expected
              then return (InverseBlock fmt1 fmt2 exp1 bparams (reverse $2) $3)
              else Left (blockError "Inverse block" expected actual)
      }

OpenInverseBlock :
    openInverse BareExpr BlockParams close { (Fmt $1 $4, $2, $3) }

{-
  {{^}}
  {{else}}

  Mustache statement inside a block. If a block's main expression evaluates to falsey,
  everything after its Inverse is evaluated instead.
  An Inverse terminates the block, i.e. it can't be followed by a chained inverse.
-}
Inverse :
    openInverse close Statements      { Just $ Inverse (Fmt $1 $2) (reverse $3) }
  | openInverseChain close Statements { Just $ Inverse (Fmt $1 $2) (reverse $3) }
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
InverseChain :
    OpenInverseChain Statements InverseChain
      {
        let (fmt, expr, bparams) = $1
        in  Just $ InverseChain fmt expr bparams (reverse $2) $3
      }
  | Inverse                           { $1 }

OpenInverseChain :
    openInverseChain BareExpr BlockParams close { (Fmt $1 $4, $2, $3) }

-- -----------------------------------------------------------------------------
-- Partials

{-
  {{> partial helper here}} regular mustache style inline partial
  {{#> partial block helper here abc=def}}
  arbitrary handlebars content inside a partial block
  {{/partial}}
-}
Partial :
    openPartial BareExpr close     { Partial (Fmt $1 $3) $2 }
  | PartialBlock                   { $1 }

PartialBlock :
    OpenPartialBlock Statements CloseBlock
      {%
          let (fmt1, exp1) = $1
              (fmt2, exp2) = $3
              expected = exprHelper exp1
              actual = litHelper exp2
          in  if expected == actual && isJust expected
              then return (PartialBlock fmt1 fmt2 exp1 (reverse $2))
              else Left (blockError "Partial block" expected actual)
      }

OpenPartialBlock :
    openPartialBlock BareExpr close { (Fmt $1 $3, $2) }

-- -----------------------------------------------------------------------------
-- Decorators

Decorator :
    openDecorator BareExpr close   { Decorator (Fmt $1 $3) $2 }
  | DecoratorBlock                 { $1 }

DecoratorBlock :
    OpenDecoratorBlock Statements CloseBlock
      {%
          let (fmt1, exp1) = $1
              (fmt2, exp2) = $3
              expected = exprHelper exp1
              actual = litHelper exp2
          in  if expected == actual && isJust expected
              then return (DecoratorBlock fmt1 fmt2 exp1 (reverse $2))
              else Left (blockError "Decorator block" expected actual)
      }

OpenDecoratorBlock :
    openDecoratorBlock BareExpr close { (Fmt $1 $3, $2) }

-- -----------------------------------------------------------------------------
-- Raw blocks

Raw :
    openRawBlock BareExpr closeRawBlock rawContent closeRaw
      {%
         let helperName = exprHelper $2 in
         if helperName == Just $5
         then return $ RawBlock $2 $4
         else Left (rawBlockError helperName $5)
      }

-- -----------------------------------------------------------------------------
-- Exprs, literals, atoms

BareExpr :
    -- A SExp without parentheses.
    Literal Exprs Hash             { SExp $1 (reverse $2) $3 }

Expr :
    openSexp BareExpr closeSexp    { $2 }
  | Literal                        { Lit $1 }

Exprs :
    Exprs Expr                     { $2 : $1 }
  | {- empty -}                    { [] }

Literal :
    string                         { StringL $1 }
  | number                         { NumberL $1 }
  | bool                           { BooleanL $1 }
  | undef                          { UndefinedL }
  | nul                            { NullL }
  | Path                           { PathL $1 }

Path :
    dataSigil PathComponents       { DataPath $2 }
  | PathComponents                 { Path $1 }

PathComponents :
    ident sep PathComponents       { PathID $1 : PathSep $2 : $3 }
  | ident                          { PathID $1 : [] }

Hash :
    HashPairs                      { Hash $1 }

HashPairs :
    ident equals Expr HashPairs    { HashPair $1 $3 : $4 }
  | {- empty -}                    { [] }

BlockParams :
    openBlockParams Names closeBlockParams { Just $ BlockParams (reverse $2) }
  | {- empty -}                            { Nothing }

Names :
  -- Used only in BlockParams, where the LHS of each pair must be a 'simple' name
    SimpleID                       { [$1] }
  | Names SimpleID                 { $2 : $1 }

SimpleID :
    ident                          { PathL (Path [PathID $1]) }

{

newtype ParseError = ParseError { renderParseError :: Text }
  deriving (Eq, Show)

parseError ts = Left . ParseError $ "Parse error at token " <> T.pack (show (headMay ts))

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
