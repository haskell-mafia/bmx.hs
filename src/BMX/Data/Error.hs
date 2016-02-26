{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Error (
    BMXError (..)
  , renderBMXError
  , LexError (..)
  , ParseError (..)
  , renderParseError
  , FunctionError (..)
  , renderFunctionError
  , EvalError (..)
  , renderEvalError
  , indent
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           BMX.Data.Position (SrcInfo (..), renderSrcInfo)

import           P

-- | An aggregate type for the various things that can go wrong in BMX.
-- Constructors are provided for casing, though most users will probably want
-- to use 'renderBMXError' to produce something human-readable.
data BMXError
  = BMXLexError !LexError
  | BMXParseError !ParseError
  | BMXEvalError !EvalError

-- | Produce a human-readable error (as 'Text') from a 'BMXError'.
renderBMXError :: BMXError -> Text
renderBMXError = \case
  BMXLexError e -> renderLexError e
  BMXParseError e -> renderParseError e
  BMXEvalError e -> renderEvalError e


-- -----------------------------------------------------------------------------
-- Lexer errors

newtype LexError = LexError { renderLexError :: Text }
  deriving (Eq, Show)


-- -----------------------------------------------------------------------------
-- Parser errors

data ParseError = ParseError !SrcInfo !Text
  deriving (Eq)

renderParseError :: ParseError -> Text
renderParseError (ParseError loc text) = T.unlines [ header, indent 1 text ]
  where
    header = case loc of
      NoInfo -> "Parse error: "
      SrcLoc _ _ -> "Parse error between [" <> renderSrcInfo loc <> "]: "

indent :: Int -> Text -> Text
indent n t = case fmap (pre <>) (T.lines t) of
  [x] -> x
  mor -> T.unlines (filter (not . T.null) mor)
  where
    pre = T.replicate n "  "


-- -----------------------------------------------------------------------------
-- Function errors

data FunctionError
  = Mismatch !Text !Text
  | Trailing !Int
  | EOF
  | NoParams


renderFunctionError :: FunctionError -> Text
renderFunctionError = \case
  Mismatch e a -> "Type mismatch (expected " <> e <> ", got " <> a <> ")"
  Trailing i -> "Too many arguments (" <> T.pack (show i) <> " unused)"
  EOF -> "Not enough arguments"
  NoParams -> "Not enough block parameters"


-- -----------------------------------------------------------------------------
-- Evaluation errors

data EvalError
  = TypeError       !SrcInfo !Text !Text -- ^ A type error, with "expected" and "actual" fields.
  | HelperError     !SrcInfo !FunctionError -- ^ Incorrect arguments for a Helper.
  | PartialError    !SrcInfo !FunctionError -- ^ Incorrect arguments for a Partial.
  | DecoratorError  !SrcInfo !FunctionError -- ^ Incorrect arguments for a Decorator.
  | InvalidPath     !SrcInfo !Text -- ^ A traversed Path had invalid format - likely "." or ".." misuse.
  | NoSuchPartial   !SrcInfo !Text -- ^ An invoked partial was not found, and there was no failover template.
  | NoSuchDecorator !SrcInfo !Text -- ^ An invoked decorator was not found.
  | NoSuchHelper    !SrcInfo !Text -- ^ An invoked block helper was not found.
  | NoSuchValue     !SrcInfo !Text -- ^ A variable was not found, and it was unsafe to proceed.
  | ParserError     !SrcInfo !Text -- ^ An absurd case - indicative of an error in the parser.
  | Unrenderable    !SrcInfo !Text -- ^ Attempt to render an undefined, list or context.
  | ShadowValue     !SrcInfo !Text -- ^ Attempt to redefine a variable in the current context
  | ShadowPartial   !SrcInfo !Text -- ^ Attempt to redefine a partial
  | ShadowHelper    !SrcInfo !Text -- ^ Attempt to redefine a helper
  | ShadowDecorator !SrcInfo !Text -- ^ Attempt to redefine a Decorator
  | DefUndef        !SrcInfo !Text -- ^ Attempt to define a variable as 'undefined' (using withVariable)
  | UserError       !SrcInfo !Text -- ^ Custom error thrown from a helper.

ree :: SrcInfo -> Text -> Text
ree loc t = T.unlines [ header, indent 1 t ]
  where
    header = "Rendering error between [" <> renderSrcInfo loc <> "]:"

renderEvalError :: EvalError -> Text
renderEvalError = \case
  TypeError       loc e a -> ree loc $ "Type error (expected " <> e <> ", actually " <> a <> ")"
  HelperError     loc fe  -> ree loc $ "Helper misuse: " <> renderFunctionError fe
  PartialError    loc fe  -> ree loc $ "Partial misuse: " <> renderFunctionError fe
  DecoratorError  loc fe  -> ree loc $ "Decorator misuse: " <> renderFunctionError fe
  InvalidPath     loc t   -> ree loc $ "Invalid path (" <> T.pack (show t) <> " can only appear at the start of a path)"
  NoSuchPartial   loc t   -> ree loc $ "Partial '" <> t <> "' is not defined"
  NoSuchDecorator loc t   -> ree loc $ "Decorator '" <> t <> "' is not defined"
  NoSuchHelper    loc t   -> ree loc $ "Helper '" <> t <> "' is not defined"
  NoSuchValue     loc t   -> ree loc $ "Value '" <> t <> "' is not defined"
  ParserError     loc t   -> ree loc $ "Parser error: " <> t
  Unrenderable    loc t   -> ree loc $ "Invalid mustache: cannot render '" <> t <> "'"
  ShadowValue     loc t   -> ree loc $ "The local definition of value '" <> t <> "' shadows an existing binding"
  ShadowHelper    loc t   -> ree loc $ "The local definition of helper '" <> t <> "' shadows an existing binding"
  ShadowPartial   loc t   -> ree loc $ "The local definition of partial '" <> t <> "' shadows an existing binding"
  ShadowDecorator loc t   -> ree loc $ "The local definition of decorator '" <> t <> "' shadows an existing binding"
  DefUndef        loc t   -> ree loc $ "Attempt to define variable '" <> t <> "' as 'undefined' - no"
  UserError       loc t   -> ree loc $ "Error thrown in user code: " <> t
