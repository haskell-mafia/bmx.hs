{- | A set of debugging helpers that should not be used in production. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Builtin.Debug where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Debug.Trace (traceM)
import           System.IO (stderr)

import           BMX.Data
import           BMX.Function

import           P hiding (traceM, log)

{-# WARNING debugHelpers "Do not use 'debugHelpers' in production code" #-}
debugHelpers :: (Applicative m, MonadIO m) => Map Text (Helper m)
debugHelpers = M.fromList [
    ("trace", logTrace)
  , ("scream", logIO)
  ]

{-
  FIX choose one, delete one
-}
logTrace :: (Applicative m, Monad m) => Helper m
logTrace = Helper $ do
  args <- many value
  traceM (T.unpack . T.unwords $ fmap renderValue args)
  return (StringV T.empty)

logIO :: (Applicative m, MonadIO m) => Helper m
logIO = Helper $ do
  args <- many value
  liftIO . T.hPutStrLn stderr . T.unwords $ fmap renderValue args
  return (StringV T.empty)
