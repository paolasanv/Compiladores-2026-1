module Main (main) where

import Analisis.Lexer(lexer)
import Analisis.Parser(parser)

main :: IO ()
main = do 
    putStrLn "Ejemplo -> " 
    putStrLn $ show $ parser $ lexer "abc := 38 + 3 * 4"
