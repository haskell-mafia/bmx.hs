{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.BMX.React where

import           BMX
import           BMX.React

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core.IO (testIO)

import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)
import           System.Process (readProcess)

import           Test.BMX.Arbitrary ()
import           Test.QuickCheck (Property, (===), forAllProperties, maxSuccess, quickCheckWithResult, stdArgs)

import           X.Data.Aeson (asText)

import P


foo :: Template -> [(Text, BMXValue)] -> Property
foo t vars =
  testIO . withTempDirectory "dist" "react" $ \dir -> do
    let file = dir <> "/temp.hbs.js"
    T.writeFile file $ renderReactFile "test" t
    T.putStrLn $ renderReactFile "test" t
    let t1 = renderPage <$> renderTemplate (usingContext defaultState vars) t
    _ <- readProcess "npm" ["install"] ""
    t2 <- T.strip . T.pack <$> readProcess "bin/react-render.js" [file] (T.unpack . asText . contextToJSON $ vars)
    pure $ t1 === pure t2

prop_react_tag =
  foo [bmx|<div></div>|] []

prop_react_tag_attributes =
  foo [bmx|<div class="a" type="b"></div>|] []

prop_react_mustache =
  foo [bmx|<div>{{a}}</div>|] [("a", BMXString "b")]

prop_react_each =
  foo [bmx|<div>{{#each a as |x|}}{{x.b}}{{/each}}</div>|] [("a", BMXList [BMXContext [("b", BMXString "c")]])]

prop_react_if =
  foo [bmx|<div>{{#if a}}{{a}}{{else}}c{{/if}}</div>|] [("a", BMXString "b")]

-- FIX Broken due to each in bmx, not react
{-
prop_react_parent_scope =
  foo [bmx|<div>{{#each a}}{{../b}}{{/each}}</div>|] [("a", BMXList [BMXNull]), ("b", BMXString "c")]
-}


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult stdArgs { maxSuccess = 1 }
