module Main (main) where

import Analisis.Lexer(lexer)
import Analisis.Parser(parser)
import Sintesis.RI(representacionI)
import Sintesis.RIx32(codigoObjeto)

main :: IO ()
main = do 
    putStrLn "Ejemplo -> " 
    let ins = parser $ lexer "abc := 2+5*(5+56)-7"
    let (i,_) = representacionI ins
    putStrLn "Instrucciones de tres direcciones: "
    mapM_ (putStrLn . show) i
    let trad = codigoObjeto i
    mapM_ (putStrLn . show) trad
