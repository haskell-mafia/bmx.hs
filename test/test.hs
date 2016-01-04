import           Disorder.Core.Main

import           Test.BMX.Lexer as Lexer (tests)
import           Test.BMX.Parser as Parser (tests)

main :: IO ()
main =
  disorderMain [
      Lexer.tests
    -- , Parser.tests
    ]
