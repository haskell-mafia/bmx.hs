{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.BMX.Orphans where

import           Data.Data
import           Data.String (IsString (..))
import           GHC.Generics

import           BMX.Data

import           P

deriving instance Generic Tokens

deriving instance Generic Token

deriving instance Generic Context
deriving instance Data Context
deriving instance Typeable Context

deriving instance Generic Value
deriving instance Data Value
deriving instance Typeable Value

deriving instance Generic BMXValue
deriving instance Data BMXValue
deriving instance Typeable BMXValue

instance IsString Chunk where
  fromString = singleChunk . fromString
