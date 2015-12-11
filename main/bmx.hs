{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as T
import           System.IO

import           BMX.Lexer

import           P

main :: IO ()
main = do
  inp <- T.getContents
  either (print . renderLexError) print (tokenise inp)
