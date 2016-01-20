{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module BMX.Data.Value (
    Context (..)
  , Value (..)
  , Param (..)
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
  | NullV
  | UndefinedV
  | ContextV Context
  | ListV [Value]
  deriving (Eq, Show)

newtype Param = Param { renderParam :: Text }
  deriving (Eq, Show)

renderValue :: Value -> Text
renderValue = \case
  StringV t -> t
  IntV i -> T.pack (show i)
  BoolV b -> if b then "true" else "false"
  NullV -> "null"
  UndefinedV -> "undefined"
  ContextV _ -> "(object)"
  ListV _ -> "(list)"

truthy :: Value -> Bool
truthy = \case
  BoolV b -> b
  NullV -> False
  UndefinedV -> False
  StringV t -> not (T.null t)
  IntV i -> i /= 0
  ContextV c -> c /= mempty
  ListV l -> not (null l)

falsey :: Value -> Bool
falsey = not . truthy

renderValueType :: Value -> Text
renderValueType = \case
  StringV _ -> "string"
  IntV _ -> "number"
  BoolV _ -> "boolean"
  NullV -> "null"
  UndefinedV -> "undefined"
  ContextV _ -> "context"
  ListV _ -> "list"
