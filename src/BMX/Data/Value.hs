{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module BMX.Data.Value (
    Context (..)
  , Value (..)
  , renderValue
  , renderValueType
  , truthy
  , falsey
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

import           P

data Context = Context (Map Text Value)
  deriving (Eq, Show)

instance Monoid Context where
  mempty = Context mempty
  mappend (Context a) (Context b) = Context (M.union a b)

data Value
  = StringV Text
  | IntV Integer
  | BoolV Bool
  | ContextV Context
  | NullV
  | UndefinedV
  deriving (Eq, Show)

renderValue :: Value -> Text
renderValue = \case
  StringV t -> t
  IntV i -> T.pack (show i)
  BoolV b -> if b then "true" else "false"
  NullV -> "null"
  UndefinedV -> "undefined"
  ContextV _ -> "(object)" -- FIX this is almost never what we want

truthy :: Value -> Bool
truthy = \case
  BoolV b -> b
  NullV -> False
  UndefinedV -> False
  StringV t -> not (T.null t)
  ContextV _ -> True
  IntV i -> i /= 0

falsey :: Value -> Bool
falsey = not . truthy

renderValueType :: Value -> Text
renderValueType = \case
  StringV _ -> "string"
  IntV _ -> "number"
  BoolV _ -> "boolean"
  ContextV _ -> "context"
  NullV -> "null"
  UndefinedV -> "undefined"
