{- | A set of debugging helpers that should not be used in production. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Builtin.Debug where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO (stderr)

import           BMX.Data
import           BMX.Function

import           P hiding (traceM, log)

{-# WARNING debugHelpers "Do not use 'debugHelpers' in production code" #-}
debugHelpers :: (Applicative m, MonadIO m) => [(Text, Helper m)]
debugHelpers = [
    ("log", logIO)
  ]

logIO :: (Applicative m, MonadIO m) => Helper m
logIO = helper $ do
  args <- many value
  liftIO . T.hPutStrLn stderr . T.unwords $ fmap renderValue args
  return (StringV T.empty)
