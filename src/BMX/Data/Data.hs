{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module BMX.Data.Data where

import           BMX.Data.Function
import           BMX.Data.Value

data DataT m
  = DValue Value
  | DPartial (PartialT m)
  | DHelper (HelperT m)
  | DDecorator (DecoratorT m)
