module Main (main) where

import Analisis.Lexer(lexer)
import Analisis.Parser(parser)
import Sintesis.RI(representacionI)
import qualified Sintesis.RIx32 as X32
import qualified Sintesis.RIarm64 as ARM64

main :: IO ()
main = do 
    putStrLn "Ejemplo -> " 
    let ins = parser $ lexer "abc := 2+5*(5+56)-7"
    let (i,_) = representacionI ins
    
    putStrLn "\n===== REPRESENTACIÓN INTERMEDIA ====="
    putStrLn "Instrucciones de tres direcciones: "
    mapM_ (putStrLn . show) i
    
    putStrLn "\n===== ARQUITECTURA AT&T x86-32 ====="
    let tradx32 = X32.codigoObjeto i
    mapM_ (putStrLn . show) tradx32
    
    putStrLn "\n===== ARQUITECTURA ARM64 ====="
    let tradArm64 = ARM64.codigoObjeto i
    mapM_ (putStrLn . show) tradArm64