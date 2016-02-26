{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Error (
    BMXError (..)
  , renderBMXError
  , LexError (..)
  , ParseError (..)
  , indent
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           BMX.Data.Eval (EvalError, renderEvalError)
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

newtype LexError = LexError { renderLexError :: Text }
  deriving (Eq, Show)

data ParseError = ParseError !SrcInfo !Text
  deriving (Eq)

renderParseError :: ParseError -> Text
renderParseError (ParseError loc text) = T.unlines [ header, indent 1 text ]
  where
    header = case loc of
      NoInfo -> "Parse error: "
      SrcLoc _ _ -> "Parse error between [" <> renderSrcInfo loc <> "]: "

indent :: Int -> Text -> Text
indent n = T.unlines . fmap (pre <>) . T.lines
  where
    pre = T.replicate n "  "
