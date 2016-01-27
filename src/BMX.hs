{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX (
  -- * Templates
    Template
  , templateFromText
  , templateToText
  -- * Pages
  , Page
  , renderPage
  -- * Rendering a Template
  , renderTemplate
  , renderTemplateIO
  , EvalState
  , defaultState
  -- * Errors
  , BMXError (..)
  , renderBMXError
  -- * Evaluation context
  , Context
  , Value (..)
  , contextFromList
  , usingContext
  -- * Partials
  , Partial
  , partialFromTemplate
  , usingPartials
  -- * Helpers
  , Helper
  , usingHelpers
  -- * Decorators
  , Decorator
  , usingDecorators
  ) where

import           Control.Monad.Identity (Identity)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

import           BMX.Builtin
import           BMX.Data
import           BMX.Function
import           BMX.Lexer as X (tokenise)
import           BMX.Parser as X (parse)

import           P

-- | Lex and parse a @Template@ from some @Text@.
templateFromText :: Text -> Either BMXError Template
templateFromText = either convert (bimap BMXParseError id . parse) . tokenise
  where
    convert = Left . BMXLexError

-- | Apply a @Template@ to some @EvalState@, producing a @Page@.
--
-- All helpers, partials and decorators must be pure. Use @renderTemplateIO@
-- if IO helpers are required.
renderTemplate :: EvalState Identity -> Template -> Either BMXError Page
renderTemplate st t = bimap BMXEvalError id $ fst (runBMX st (eval t))

-- | Apply a @Template@ to some @EvalState@, producing a @Page@.
--
-- Helpers, partials and decorators may involve IO. Use @renderTemplate@ if
-- no IO helpers are to be invoked.
renderTemplateIO :: (Applicative m, MonadIO m) => EvalState m -> Template -> m (Either BMXError Page)
renderTemplateIO es t = fmap (bimap BMXEvalError id . fst) (runBMXIO es (eval t))

usingContext :: (Applicative m, Monad m) => EvalState m -> Context -> EvalState m
usingContext st c = st { evalContext = [c] }

usingPartials :: (Applicative m, Monad m) => EvalState m -> [(Text, Template)] -> EvalState m
usingPartials st kv = st { evalPartials = fmap partialFromTemplate (M.fromList kv) }

usingHelpers :: (Applicative m, Monad m) => EvalState m -> [(Text, Helper m)] -> EvalState m
usingHelpers st kv = st { evalHelpers = M.fromList kv }

usingDecorators :: (Applicative m, Monad m) => EvalState m -> [(Text, Decorator m)] -> EvalState m
usingDecorators st kv = st { evalDecorators = M.fromList kv }

partialFromTemplate :: (Applicative m, Monad m) => Template -> Partial m
partialFromTemplate = partial . eval
