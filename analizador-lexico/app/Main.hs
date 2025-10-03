{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Console.Haskeline

import Regex.Lexer
import Regex.Parser

import Data.List (intercalate, isPrefixOf)
import System.FSNotify
import System.FilePath (takeFileName, takeDirectory)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_analizador_lexico (getDataFileName)
import System.Directory (doesFileExist)

{-
            Lexer Real
regex :: T.Text  -> RegEx
regex content = toRegex (text2String content) 

lexerIMP ::  String -> [TokenIMP]
lexerIMP input = lexerDo (minimize $ toDFA $ toNFA $ toNFAE regex) input 
-}


-- Simulación del lexer 
lexerMain :: String -> [String]
lexerMain input = ["llamada", "a", "lexer", "con", input]

-- De cadena a RegEx
toRegex :: String -> RegEx 
toRegex x = parser $ lexer x

-- Convierte archivo de texto en string separado por |
text2String :: T.Text -> String
text2String content = intercalate " | " (map T.unpack (T.lines content))

-- Cargar y convertir archivo a RegEx
loadRegexFromFile :: FilePath -> IO RegEx
loadRegexFromFile path = do
    content <- TIO.readFile path
    putStrLn "Expresión regular actualizada desde el archivo."
    let regex = toRegex (text2String content)
    putStrLn $ show regex
    return regex

-- Vigilar cambios en el archivo de expresiones regulares
watchRegexFile :: FilePath -> (RegEx -> IO ()) -> IO ()
watchRegexFile filename onUpdate = withManager $ \mgr -> do
    existeLocal <- doesFileExist filename
    ruta <- if existeLocal then return filename else getDataFileName filename
    let carpeta = takeDirectory ruta
    let archivo = takeFileName ruta

    -- Ejecutar la primera vez
    void $ onUpdate =<< loadRegexFromFile ruta

    void $ watchDir
        mgr
        carpeta
        (\e -> case e of
            Modified path _ _ -> takeFileName path == archivo
            _ -> False)
        (\_ -> do
            putStrLn $ "Cambio detectado en " ++ archivo
            void $ onUpdate =<< loadRegexFromFile ruta)

    putStrLn $ "Observando " ++ archivo ++ " por cambios..."
    forever $ threadDelay 1000000 

-- loop del REPL
replLoop :: InputT IO ()
replLoop = do
    minput <- getInputLine ">> "
    case minput of
      Nothing   -> return ()
      Just ""   -> replLoop
      Just ":q" -> outputStrLn "Chao ;)" >> return ()
      Just xs 
        | "lexer" `isPrefixOf` xs -> do
            let args = drop 6 xs
            outputStrLn $ show $ lexerMain args
            replLoop
        | otherwise -> do
            outputStrLn $ "Comando desconocido: " ++ xs
            replLoop

-- main
main :: IO ()
main = do
    putStrLn "\n ===== Analizador Léxico para IMP :) ====== "
    _ <- forkIO $ watchRegexFile "regex.txt" (\_ -> return ())
    runInputT defaultSettings replLoop
