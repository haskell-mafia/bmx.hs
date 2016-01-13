import           Disorder.Core.Main

import           Test.BMX.Lexer as Lexer (tests)
import           Test.BMX.Parser as Parser (tests)
import           Test.BMX.Page as Page (tests)

main :: IO ()
main =
  disorderMain [
      Lexer.tests
    , Parser.tests
    , Page.tests
    ]
