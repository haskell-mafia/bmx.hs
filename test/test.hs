import           Disorder.Core.Main

import           Test.BMX.Lexer as Lexer (tests)

main :: IO ()
main =
  disorderMain [
      Lexer.tests
    ]
