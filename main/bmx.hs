{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as T
import           System.IO

import           BMX
import           BMX.Data
import           BMX.Eval
import           BMX.Helpers

import           P

main :: IO ()
main = do
  inp <- T.getContents
  let parsed = templateFromText inp
  either (T.putStrLn . renderParseError) print parsed
  let scream = T.hPutStrLn stderr
      drawResult (epage, er) = do
        either (scream . renderEvalError) (T.putStrLn . renderPage) epage
        mapM_ (scream . renderEvalOutput) er
  either (const $ return ())
         (drawResult . runBMX defaultEvalState . evalProgram)
         parsed
