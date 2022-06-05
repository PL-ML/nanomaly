import NanoML.Lexer
import NanoML.Types

main :: IO ()
main = interact getTokens

getTokens :: String -> String
getTokens inp = case alexScanTokens inp of
  Left e -> error e
  Right ds -> show $ map (\(LToken (SrcSpan a b c d) _) -> ((a,b),(c,d)))  ds
