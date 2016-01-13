{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as T
import           System.IO

import           BMX

import           P

main :: IO ()
main = do
  inp <- T.getContents
  let parsed = templateFromText inp
  either (T.putStrLn . renderParseError) print parsed
