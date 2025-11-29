{-# LANGUAGE OverloadedStrings #-}


module Main (main) where

import Analisis.Lexer(lexer)
import Analisis.Parser(parser)
import Sintesis.RI(representacionI)
import Sintesis.RIx32(codigoObjeto, Instx32)
import System.Console.Haskeline
import Data.Char (toLower)

data Arquitectura = X32 | Otra deriving (Show, Eq) -- Cambiar 'Otra' por la arquitectura elegida

leerArquitectura :: String -> Maybe Arquitectura
leerArquitectura s =
    case s of
        "x32" -> Just X32
        "otra" -> Just Otra
        _     -> Nothing

compilador :: String -> Arquitectura -> Either [Instx32] [String]
compilador cadena X32 = Left (codigoObjeto $ representacionI $ parser $ lexer cadena)
compilador cadena Otra = Right ["codigo arquitectura para x64 aun no implementada"]

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
                        Nothing -> outputStrLn "Arquitectura no válida (use x32 o [otra])" >> repl
                        Just arq -> do
                            let cadena = stripQuotes $ unwords resto
                            let resultado = compilador cadena arq
                            case resultado of
                                Left inst -> do
                                    outputStrLn "Sintaxis AT&T como una representación intermedia para una arquitectura de 32 bits " 
                                    outputStrLn $ "Código fuente: " ++ cadena 
                                    outputStrLn "Código objeto (simulado):"
                                    mapM_ (outputStrLn . show) inst
                                Right inst -> do
                                    outputStrLn "Codigo para otra arquitectura (no implementada):" 
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
    putStrLn "Uso: compilador <arquitectura> <cadena>"
    putStrLn "Arquitecturas disponibles: x32 y [otra]\n" -- Cambiar 'Otra' por la arquitectura elegida
    runInputT defaultSettings repl

-- Elimina comillas dobles al inicio y final, si existen
stripQuotes :: String -> String
stripQuotes s =
  case s of
    ('"':rest) | not (null rest) && last rest == '"' -> init rest
    _ -> s