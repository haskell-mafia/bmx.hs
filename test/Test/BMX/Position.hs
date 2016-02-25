{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.BMX.Position where

import           Data.Generics.Aliases
import           Data.Generics.Schemes

import           Test.QuickCheck
import           Test.BMX.Arbitrary ()

import           BMX.Data

import           P

-- | Sets all the positions in the template to zero, erasing all context
decontextualise :: Template -> Template
decontextualise = everywhere (mkT eraseLoc)
  where
    eraseLoc :: SrcInfo -> SrcInfo
    eraseLoc = const mempty

return []
tests = $quickCheckAll
