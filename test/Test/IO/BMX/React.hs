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


renderProp :: Template -> [(Text, BMXValue)] -> Property
renderProp t vars =
  renderProp' t [] vars

renderProp' :: Template -> [(Text, Template)] -> [(Text, BMXValue)] -> Property
renderProp' t ts vars =
  testIO $ do
    let t1 = renderPage <$> renderTemplate (defaultState `usingContext` vars `usingPartials` fmap (second partialFromTemplate) ts) t
    t2 <- renderReactIO t ts vars
    pure $ t1 === pure t2

renderReactIO :: Template -> [(Text, Template)] -> [(Text, BMXValue)] -> IO Text
renderReactIO t ts vars =
  withTempDirectory "dist" "react" $ \dir -> do
    let file = dir <> "/temp.hbs.js"
    T.writeFile file $ renderReactFile "test" t
    for_ ts $ \(n, t') ->
      -- QUESTION: Should we hardcode the js function name, or make it possible to merge them together?
      T.writeFile (dir <> "/" <> T.unpack n <> ".hbs.js") $ renderReactFile n t'
    T.putStrLn $ renderReactFile "test" t
    _ <- readProcess "npm" ["install"] ""
    T.strip . T.pack <$> readProcess "bin/react-render.js" [dir, "test"] (T.unpack . asText . contextToJSON $ vars)

-------------------------------------

prop_react_tag =
  renderProp [bmx|<div></div>|] []

prop_react_tag_attributes =
  renderProp [bmx|<div class="a" type="b"></div>|] []

prop_react_mustache =
  renderProp [bmx|<div>{{a}}</div>|] [("a", BMXString "b")]

prop_react_each =
  renderProp [bmx|<div>{{#each a as |x|}}{{x.b}}{{/each}}</div>|] [("a", BMXList [BMXContext [("b", BMXString "c")]])]

prop_react_each_else =
  renderProp [bmx|<div>{{#each a}}x{{else}}y{{/each}}</div>|] [("a", BMXList [])]

prop_react_if =
  renderProp [bmx|<div>{{#if a}}{{a}}{{else}}c{{/if}}</div>|] [("a", BMXString "b")]

prop_react_if_else =
  renderProp [bmx|<div>{{#if a}}{{a}}{{else}}c{{/if}}</div>|] [("a", BMXNull)]

prop_react_partial_hash_only =
  renderProp'
    [bmx|<div>{{>a-b d=f }}</div>|]
    [("a-b", [bmx|<div>{{d}}</div>|])]
    [("f", BMXString "y")]

prop_react_partial =
  renderProp'
    [bmx|<div>{{>a-b c d=f }}</div>|]
    [("a-b", [bmx|<div>{{d}}</div>|])]
    [("c", BMXContext [("d", BMXString "z")]), ("f", BMXString "y")]

prop_react_partial_block =
  renderProp'
    [bmx|<div>{{#>a-b}}y{{/a-b}}</div>|]
    [("a-b", [bmx|<div>{{> @partial-block }}</div>|])]
    []

prop_react_partial_block_scope =
  renderProp'
    [bmx|<div>{{#>a-b}}{{c}}{{/a-b}}</div>|]
    [("a-b", [bmx|<div>{{> @partial-block }}</div>|])]
    [("c", BMXString "d")]

prop_react_raw =
  -- Unfortunately because of an extra span we can't do a proper round-trip test here :(
  testIO $ do
    t2 <- renderReactIO
      [bmx|<div>{{{a}}}</div>|]
      []
      [("a", BMXString "<b>")]
    pure $ t2 === "<div><span><b></span></div>"

-- FIX Broken due to each in bmx, not react
{-
prop_react_parent_scope =
  renderProp [bmx|<div>{{#each a}}{{../b}}{{/each}}</div>|] [("a", BMXList [BMXNull]), ("b", BMXString "c")]
-}


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult stdArgs { maxSuccess = 1 }
