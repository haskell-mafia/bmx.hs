{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A templating library in the style of <http://handlebarsjs.com Handlebars.js>,
-- embedded in Haskell for static or server-side rendering.
--


module BMX (
  -- * Differences from Handlebars
  -- $whatsnew

  -- * Templates
  -- $templates
    Template
  , templateFromText
  , templateToText

  -- * Pages
  -- $pages
  , Page
  , renderPage

  -- * Rendering a Template
  -- $rendering
  , renderTemplate
  , renderTemplateIO
  , EvalState
  , defaultState

  -- * Errors
  , BMXError (..)
  , renderBMXError

  -- * Providing data
  -- $values
  , Context
  , Value (..)
  , contextFromList
  , usingContext

  -- * Partials
  -- $partials
  , Partial
  , partialFromTemplate
  , usingPartials

  -- * Helpers
  -- $helpers
  , Helper
  , usingHelpers

  -- * Decorators
  -- $decorators
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

-- $whatsnew
--
-- BMX is considerably stricter than Handlebars. A number
-- of error-prone constructs that Handlebars accepts will result in a 'BMXError':
--
-- * Any attempt (mustache or 'Helper') to print @undefined@, @null@,
-- a list, or a 'Context' will result in an error.
-- * TBD


-- $templates BMX templates are syntactically compatible with
-- Handlebars 4.
--
-- Use 'templateFromText' to parse a 'Template' from some 'Text',
-- pretty-print it with 'templateToText', and apply it to some data
-- with 'renderTemplate' / 'renderTemplateIO'.

-- $pages Rendering a 'Template' produces a 'Page', which is little
-- more than a 'Text' field with additional formatting
-- information.
--
-- Extract the final 'Text' artefact with 'renderPage'.

-- $rendering
--
-- Apply a 'Template' to some 'EvalState' to produce a 'Page', a
-- fully-evaluated document that is ready to print.
--
-- Build up an 'EvalState' by using 'mempty' or
-- 'defaultState' as a base, and then applying 'usingContext',
-- 'usingPartials', 'usingHelpers', and 'usingDecorators' to supply
-- custom functions and data as needed.
--
-- > myEvalState = defaultState
-- >   `usingContext` coolContext
-- >   `usingPartials` [("login", loginTemplate), ("logout", logoutTemplate)]

-- $values
--
-- To make use of a 'Template', we need to supply it with data at runtime.
--
-- A 'Context' is a set of mappings from 'Text' to 'Value', i.e. local
-- variable bindings. The current 'Context' is stored in the
-- 'EvalState', and is used for all variable lookups. The initial
-- context can be set via 'usingContext'.
--
-- Values can be integers, strings, booleans, undefined, lists, or
-- nested contexts / namespaces. Use the constructors directly.

-- $partials
--
-- A 'Partial' produces a 'Page' that another 'Template' can render
-- inline. The partial has full access to the local
-- 'EvalState' when run.
--
-- Most partials will be constructed from 'Template' values using
-- 'partialFromTemplate'. However, the type is general enough to admit
-- arbitrary Haskell functions. See 'BMX.Function.partial'.

-- $helpers
--
-- A 'Helper' comes in two varieties:
--
-- * A 'BMX.Function.helper' is a function that produces a 'Value'.
--   Regular helpers can be invoked in mustache expressions, and in
--   subexpression arguments to other helpers.
--
-- * A 'BMX.Function.blockHelper' is a function that accepts two
--   'Template' parameters (roughly equivalent to @then@ and @else@
--   branches), producing a 'Page'. Block helpers can be invoked in
--   blocks, partial blocks, and inverse blocks.
--
-- A default set of helpers is provided - 'builtinHelpers'.
--
-- See <BMX-Function.html BMX.Function> for details on implementing
-- custom helpers.

-- $decorators
--
-- A 'Decorator' is a function that can make arbitrary changes to the
-- 'EvalState'. The changes made will only affect the surrounding
-- block.  Decorators are preprocessed before their containing block
-- is rendered.
--
-- A default set of decorators is provided - 'builtinDecorators'.
--
-- See <BMX-Function.html BMX.Function> for details on implementing
-- custom decorators.

-- | Lex and parse a 'Template' from some 'Text'.
templateFromText :: Text -> Either BMXError Template
templateFromText = either convert (bimap BMXParseError id . parse) . tokenise
  where
    convert = Left . BMXLexError

-- | Apply a 'Template' to some 'EvalState', producing a 'Page'.
--
-- All helpers, partials and decorators must be pure. Use 'renderTemplateIO'
-- if IO helpers are required.
renderTemplate :: EvalState Identity -> Template -> Either BMXError Page
renderTemplate st t = bimap BMXEvalError id $ fst (runBMX st (eval t))

-- | Apply a 'Template' to some 'EvalState', producing a 'Page'.
--
-- Helpers, partials and decorators may involve IO. Use @renderTemplate@ if
-- no IO helpers are to be invoked.
renderTemplateIO :: (Applicative m, MonadIO m) => EvalState m -> Template -> m (Either BMXError Page)
renderTemplateIO es t = fmap (bimap BMXEvalError id . fst) (runBMXIO es (eval t))

-- | Set the initial context in an 'EvalState'.
usingContext :: (Applicative m, Monad m) => EvalState m -> Context -> EvalState m
usingContext st c = st { evalContext = [c] }

-- | Add a named collection of partials to the 'EvalState'.
usingPartials :: (Applicative m, Monad m) => EvalState m -> [(Text, Template)] -> EvalState m
usingPartials st kv = st { evalPartials = fmap partialFromTemplate (M.fromList kv) }

-- | Add a named collection of helpers to the 'EvalState'.
usingHelpers :: (Applicative m, Monad m) => EvalState m -> [(Text, Helper m)] -> EvalState m
usingHelpers st kv = st { evalHelpers = M.fromList kv }

-- | Add a named collection of decorators to the 'EvalState'.
usingDecorators :: (Applicative m, Monad m) => EvalState m -> [(Text, Decorator m)] -> EvalState m
usingDecorators st kv = st { evalDecorators = M.fromList kv }

-- | Produce a 'Partial' from an ordinary 'Template'.
partialFromTemplate :: (Applicative m, Monad m) => Template -> Partial m
partialFromTemplate = partial . eval
