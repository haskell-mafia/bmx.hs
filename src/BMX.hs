{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX (
    module X
  , renderTemplate
  , templateToText
  , templateFromText
  ) where

import           Data.Text (Text)

import           BMX.Builtin
import           BMX.Data
import           BMX.Eval (eval)
import           BMX.Lexer as X (LexError (..), tokenise)
import           BMX.Parser as X (ParseError (..), parse)

import           P

templateFromText :: Text -> Either ParseError Template
templateFromText = either convert parse . tokenise
  where
    convert = Left . ParseError . lexError . renderLexError
    lexError e = "Lexing error " <> e

renderTemplate :: Context -> Template -> (Either EvalError Page, [EvalOutput])
renderTemplate c t = runBMX (defaultState c) (eval t)

{- Proposed public interface something like this - keep EvalState opaque
renderTemplateIO :: MonadIO m => Context -> Template -> m (Either EvalError Page, [EvalWarning])

renderTemplateWith :: Context -> Partials -> Helpers -> Decorators
                   -> (Either EvalError Page, [EvalWarning])

renderTemplateWithIO :: MonadIO m => Context -> Partials -> Helpers -> Decorators
                     -> m (Either EvalError Page, [EvalWarning])
-}
