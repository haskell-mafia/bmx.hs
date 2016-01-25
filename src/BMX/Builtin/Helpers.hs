{-| The collection of builtin helpers, included in the default environment. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Builtin.Helpers where

import           Data.List (zipWith)
import           Data.Text (Text)
import qualified Data.Text as T

import           BMX.Data
import           BMX.Function

import           P hiding (log, unless)

-- | The default collection of builtins.
builtinHelpers :: (Applicative m, Monad m) => [(Text, Helper m)]
builtinHelpers = [
    ("noop", noop)
  , ("if", iff)
  , ("unless", unless)
  , ("with", with)
  , ("log", log)
  , ("lookup", lookup)
  , ("each", each)
  ]

-- | The "noop" block helper. Renders the main block.
noop :: (Applicative m, Monad m) => Helper m
noop = blockHelper $ \b _ -> liftBMX (eval b)

-- | The "if" block helper. Renders the main block if the argument is truthy.
-- Otherwise, it renders the inverse block.
iff :: (Applicative m, Monad m) => Helper m
iff = blockHelper $ \thenp elsep -> do
  v <- value
  liftBMX $ if truthy v then eval thenp else eval elsep

-- | The "unless" block helper. The opposite of "if".
unless :: (Applicative m, Monad m) => Helper m
unless = blockHelper $ \thenp elsep -> do
  v <- value
  liftBMX $ if truthy v then eval elsep else eval thenp

-- | The "with" block helper. Accept a Context as argument.
with :: (Applicative m, Monad m) => Helper m
with = blockHelper $ \thenp elsep -> do
  ctx <- optional context
  liftBMX $ maybe
    (eval elsep)
    (\(ContextV c) -> withContext c (eval thenp))
    ctx

-- | The "log" helper. Writes every argument to the log in a single line.
log :: (Applicative m, Monad m) => Helper m
log = helper $ do
  args <- many value
  liftBMX $ do
    logs (T.unwords $ fmap renderValue args)
    return (StringV "")

-- | The "lookup" helper. Takes a context and a string, and looks up a
-- value in a context. Returns @undefined@ when it doesn't exist.
lookup :: (Applicative m, Monad m) => Helper m
lookup = helper $ do
  (ContextV ctx) <- context
  (StringV str) <- string
  liftBMX $ do
    mv <- withContext ctx (lookupValue (PathID str Nothing))
    return (fromMaybe UndefinedV mv)

-- | The "each" helper. Takes an iterable value (a context or a list),
-- and renders the main block for each item in the collection.
-- If the collection is empty, it renders the inverse block instead.
--
-- The special variables @first, @last, @key, @index, and this are registered
-- during the loop. These are as defined in Handlebars.
--
-- If block parameters are supplied, we also bind the first parameter to the
-- value in each loop, and the second parameter to the loop index.
each :: (Applicative m, Monad m) => Helper m
each = blockHelper $ \thenp elsep -> do
  iter <- list <|> context
  par1 <- optional param -- block param: name for current item (list), name for key (ctx)
  par2 <- optional param -- block param: name for current loop idx (list), name for val (ctx)
  -- This is the worst, mostly because of special variables.
  let go = case iter of
        ContextV c -> fmap fold (sequence (eachMap c))
        ListV l -> fmap fold (sequence (eachList l))
        v -> err (TypeError "context or list" (renderValueType v))
      -- Separate iteration cases for context and list
      eachMap c = indices 0 (fmap stepKV (contextToList c))
      eachList l = zipWith listIdx [0..] (indices 0 (fmap step l))
      -- Apply indices, first and last markers to each action
      indices 0 (k:[]) = [(index 0 . frst . last) k]
      indices 0 (k:ks@(_:_)) = index 0 (frst k) : indices 1 ks
      indices n (k:ks@(_:_)) = index n k : indices (n + 1) ks
      indices n (k:[]) = [index n (last k)]
      indices _ [] = []
      -- Register various special variables
      index i k = withData "index" (DValue (IntV i)) k
      stepKV (k,v) = withData "key" (DValue (StringV k))
        . withName par1 (StringV k) . withName par2 v . withVariable "this" v $ eval thenp
      step v = withVariable "this" v . withName par1 v $ eval thenp
      frst = withData "first" (DValue (BoolV True))
      last = withData "last" (DValue (BoolV True))
      -- Register blockparams if they were supplied
      withName Nothing _ k = k
      withName (Just (Param n)) v k = withVariable n v k
      listIdx i k = withName par2 (IntV i) k
  liftBMX $ if falsey iter then eval elsep else go
