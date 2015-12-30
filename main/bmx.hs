{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as T
import           System.IO

import           BMX.Data
import           BMX.Lexer
import           BMX.Parser

import           P

main :: IO ()
main = do
  inp <- T.getContents
  let lexed = tokenise inp
      parsed = either (const . Left $ ParseError "Lexer error") parse lexed
  either (print . renderLexError)
         print
         lexed
  either (print . renderParseError)
         (\prg -> print prg >> print (renderProgram prg))
         parsed
