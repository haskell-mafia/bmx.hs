{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX (
    module X
  , templateFromText
  ) where

import           Data.Text (Text)
import           BMX.Data
import           BMX.Lexer as X (LexError (..), tokenise)
import           BMX.Parser as X (ParseError (..), parse)

import           P

templateFromText :: Text -> Either ParseError Template
templateFromText = either convert parse . tokenise
  where
    convert = Left . ParseError . lexError . renderLexError
    lexError e = "Lexing error " <> e
