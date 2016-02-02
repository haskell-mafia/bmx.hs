{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.BMX.Orphans where

import           Data.Data
import qualified Data.Text as T
import           GHC.Generics

import           BMX.Internal

import           P

deriving instance Eq EvalError
deriving instance Eq FunctionError

deriving instance Eq BMXError
instance Show BMXError where
  show = T.unpack . renderBMXError

deriving instance Generic Template
deriving instance Data Template
deriving instance Typeable Template

deriving instance Generic Stmt
deriving instance Data Stmt
deriving instance Typeable Stmt

deriving instance Generic Expr
deriving instance Data Expr
deriving instance Typeable Expr

deriving instance Generic Literal
deriving instance Data Literal
deriving instance Typeable Literal

deriving instance Generic BlockParams
deriving instance Data BlockParams
deriving instance Typeable BlockParams

deriving instance Generic Path
deriving instance Data Path
deriving instance Typeable Path

deriving instance Generic DataPath
deriving instance Data DataPath
deriving instance Typeable DataPath

deriving instance Generic Hash
deriving instance Data Hash
deriving instance Typeable Hash

deriving instance Generic HashPair
deriving instance Data HashPair
deriving instance Typeable HashPair

deriving instance Generic Fmt
deriving instance Data Fmt
deriving instance Typeable Fmt

deriving instance Generic Tokens
deriving instance Data Tokens
deriving instance Typeable Tokens

deriving instance Generic Token
deriving instance Data Token
deriving instance Typeable Token

deriving instance Generic Page
deriving instance Data Page
deriving instance Typeable Page

deriving instance Generic Context
deriving instance Data Context
deriving instance Typeable Context

deriving instance Generic Value
deriving instance Data Value
deriving instance Typeable Value

deriving instance Generic BMXValue
deriving instance Data BMXValue
deriving instance Typeable BMXValue
deriving instance Show BMXValue
