{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Token (
    Tokens (..)
  , Token (..)
  , Format (..)
  , renderFormat
  ) where

import           Data.Text (Text)

import           BMX.Data.Format

import           P

newtype Tokens = Tokens { unTokens :: [Token] }
  deriving (Show, Eq)

data Token
  -- Raw Web Content
  = Content Text
  | RawContent Text
  -- Handlebars Comment
  | Comment Text
  -- Handlebars expression prologue
  | Open Format
  | OpenPartial Format
  | OpenPartialBlock Format
  | OpenBlock Format
  | OpenEndBlock Format
  | OpenUnescaped Format
  | OpenInverse Format
  | OpenInverseChain Format
  | OpenRawBlock
  | OpenComment Format
  | OpenCommentBlock Format
  | OpenDecorator Format
  | OpenDecoratorBlock Format
  -- Handlebars expression epilogue
  | Close Format
  | CloseCommentBlock Format
  | CloseUnescaped Format
  | CloseRawBlock
  | CloseRaw Text
  -- Expressions
  | ID Text
  | SegmentID Text
  | String Text
  | Number Integer
  | Boolean Bool
  | Sep Char
  | OpenSExp
  | CloseSExp
  | Equals
  | Data
  | Undefined
  | Null
  | OpenBlockParams
  | CloseBlockParams
  deriving (Show, Eq)
