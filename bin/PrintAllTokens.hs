import NanoML.Lexer
main :: IO ()
main = interact (show . alexScanTokens)