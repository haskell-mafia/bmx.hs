{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Token (
    Token (..)
  , Format (..)
  , renderToken
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           P

data Token
  -- * Raw Web Content
  = Content Text
  -- * Handlebars Comment
  | Comment Text
  -- * Handlebars expression prologue
  | Open Format
  | OpenPartial Format
  | OpenPartialBlock Format
  | OpenBlock Format
  | OpenEndBlock Format
  | OpenUnescaped Format
  | OpenInverse Format
  | OpenInverseChain Format
  | OpenCommentBlock Format
  | OpenCommentInline Format
  -- * Handlebars expression epilogue
  | Close Format
  | CloseCommentBlock Format
  -- * Expressions
  | OpenSExp
  | CloseSExp
  deriving (Show, Eq)

-- | Formatting control
data Format
  = Strip
  | Verbatim
  deriving (Show, Eq)


renderToken :: Token -> Text
renderToken = \case
  Content t           -> t
  Comment t           -> t
  --
  Open f              -> "{{" <> renderFormat f <> "&"
  OpenPartial f       -> "{{" <> renderFormat f <> ">"
  OpenPartialBlock f  -> "{{" <> renderFormat f <> "#>"
  OpenBlock f         -> "{{" <> renderFormat f <> "#*"
  OpenEndBlock f      -> "{{" <> renderFormat f <> "/"
  OpenUnescaped f     -> "{{" <> renderFormat f <> "{"
  OpenInverse f       -> "{{" <> renderFormat f <> "^"
  OpenInverseChain f  -> "{{" <> renderFormat f <> "else"
  OpenCommentBlock f  -> "{{" <> renderFormat f <> "!--"
  OpenCommentInline f -> "{{" <> renderFormat f <> "!"
  --
  Close f             -> renderFormat f <> "}}"
  CloseCommentBlock f -> "--" <> renderFormat f <> "}}"
  --
  OpenSExp            -> "("
  CloseSExp           -> ")"

renderFormat :: Format -> Text
renderFormat = \case
  Strip    -> "~"
  Verbatim -> T.empty
