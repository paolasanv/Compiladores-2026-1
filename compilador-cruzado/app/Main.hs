{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Analisis.Lexer(lexer)
import Analisis.Parser(parser)
import Sintesis.RI(representacionI)
import Sintesis.RIx32(codigoObjeto32, Instx32)
import Sintesis.RIarm64(codigoObjeto64, Instarm64)
import System.Console.Haskeline
import Data.Char (toLower)

-- Definición de las arquitecturas destino soportadas por el compilador cruzado
data Arquitectura = ATnT32 | ARM64 deriving (Show, Eq) 

leerArquitectura :: String -> Maybe Arquitectura
leerArquitectura s =
    case s of
        "atnt32" -> Just ATnT32
        "arm64" -> Just ARM64
        _     -> Nothing

-- Función principal que simula al compilador cruzado
compilador ::Arquitectura  -> String -> Either [Instx32] [Instarm64]
compilador ATnT32 cadena = Left (codigoObjeto32 $ representacionI $ parser $ lexer cadena)
compilador ARM64 cadena = Right (codigoObjeto64 $ representacionI $ parser $ lexer cadena)

repl :: InputT IO ()
repl = do
    minput <- getInputLine "> "
    case minput of
        Nothing      -> return ()      
        Just ":q"    -> return ()       -- salir
        Just ""      -> repl            -- línea vacía
        Just xs      -> do
            case words xs of
                ("compilador":arquitectura:resto) -> do
                    case leerArquitectura (map toLower (stripQuotes arquitectura)) of
                        Nothing -> outputStrLn "Arquitectura no válida (use ATnT32 o ARM64)" >> repl
                        Just arq -> do
                            let cadena = stripQuotes $ unwords resto
                            let resultado = compilador arq cadena
                            case resultado of
                                Left inst -> do
                                    outputStrLn "Lenguaje ensamblador AT&T de 32 bits " 
                                    outputStrLn $ "Código fuente: " ++ cadena 
                                    outputStrLn "Código objeto (simulado):"
                                    mapM_ (outputStrLn . show) inst
                                Right inst -> do
                                    outputStrLn "Lenguaje ensamblador ARM de 64 bits" 
                                    outputStrLn $ "Código fuente: " ++ cadena 
                                    outputStrLn "Código objeto (simulado):"
                                    mapM_ (outputStrLn . show) inst
                            repl
                _ -> do
                    outputStrLn "Comando no reconocido"
                    repl

main :: IO ()
main = do
    putStrLn "\n=======  Compilador cruzado :)  =======\n"
    putStrLn "Uso: compilador <arquitectura-destino> <cadena>"
    putStrLn "Arquitecturas disponibles: ATnT32 y ARM64\n" 
    runInputT defaultSettings repl

-- Elimina comillas dobles al inicio y final, si existen
stripQuotes :: String -> String
stripQuotes s =
  case s of
    ('"':rest) | not (null rest) && last rest == '"' -> init rest
    _ -> s
