{-| The collection of builtin helpers, included in the default environment. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Helpers where

import qualified Data.Text as T

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

import           BMX.Data
import           BMX.Eval
import           BMX.Function

import           P

-- | The "noop" block helper. Discards both branches and returns mempty.
helper_noop :: (Applicative m, Monad m) => Helper m
helper_noop = BlockHelper $ \_ _ -> return mempty

-- | The "return" block helper. Renders the left branch.
helper_return :: (Applicative m, Monad m) => Helper m
helper_return = BlockHelper $ \left _ ->
  liftBMX $ eval left

-- | The "if" block helper.
helper_if :: (Applicative m, Monad m) => Helper m
helper_if = BlockHelper $ \thenp elsep -> do
  v <- value
  liftBMX $ if truthy v then eval thenp else eval elsep

-- | The "with" block helper. Accept a Context as argument.
helper_with :: (Applicative m, Monad m) => Helper m
helper_with = BlockHelper $ \thenp elsep -> do
  ctx <- optional context
  liftBMX $ maybe (eval elsep)
                  (\c -> withContext c (eval thenp))
                  ctx

-- | The "log" helper. Writes every argument to the log in a single line.
helper_log :: (Applicative m, Monad m) => Helper m
helper_log = Helper $ do
  args <- many value
  liftBMX $ do
    logs (T.unwords $ fmap renderValue args)
    return (StringV "")

builtinHelpers :: (Applicative m, Monad m) => Map Text (Helper m)
builtinHelpers = M.fromList [
    ("noop", helper_noop)
  , ("return", helper_return)
  , ("if", helper_if)
  , ("with", helper_with)
  , ("log", helper_log)
  ]

-- | The "inline" block decorator. Turns the block argument into a partial
-- with the name of the first argument.
decorator_inline :: (Applicative m, Monad m) => Decorator m
decorator_inline = BlockDecorator $ \block k -> do
  (StringV name) <- string
  liftBMX $ do
    let newPartial = Partial (eval block)
    withPartial name newPartial k


-- FIX this also doesn't belong here
-- FIX pass context in?
defaultEvalState :: (Applicative m, Monad m) => EvalState m
defaultEvalState = EvalState {
    evalContext = [testContext]
  , evalData = M.empty
  , evalHelpers = builtinHelpers
  , evalPartials = M.insert "authorid" testPartial M.empty
  , evalDecorators = M.insert "inline" decorator_inline M.empty
  }

-- FIX this must also go
testContext :: Context
testContext = Context $ M.fromList [
    ("title", StringV "My First Blog Post!")
  , ("author", ContextV . Context $ M.fromList [
                   ("id", IntV 47)
                 , ("name", StringV "Yehuda Katz")
                 ])
  , ("body", StringV "My first post. Wheeeee!")
  , ("html", StringV "<a href=\"google.com\">Cool Site</a>")
  ]

-- FIX temporary test value
testPartial :: (Applicative m, Monad m) => Partial m
testPartial = Partial . eval $
  Template
    [ ContentStmt "The author's name is "
    , Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "name" Nothing)) [] (Hash []))
    , ContentStmt " and their ID is "
    , Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "id" Nothing)) [] (Hash []))
    , ContentStmt " arg = "
    , Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "arg" Nothing)) [] (Hash []))
    ]
