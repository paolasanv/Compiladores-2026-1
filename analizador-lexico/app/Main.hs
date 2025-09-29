{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

import Regex.Lexer
import Regex.Parser

import Data.List (intercalate)
import System.FSNotify
import System.FilePath (takeFileName, takeDirectory)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_analizador_lexico (getDataFileName)
import System.Directory (doesFileExist)


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
    putStrLn "Archivo cargado. Expresión regular actualizada:"
    let regex = toRegex (text2String content)
    print regex
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
            putStrLn $ "\nCambio detectado en " ++ archivo
            void $ onUpdate =<< loadRegexFromFile ruta)

    putStrLn $ "Observando " ++ archivo ++ " por cambios..."
    forever $ threadDelay 1000000 

-- loop del REPL
repl :: IO ()
repl = runInputT defaultSettings (loop)
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case (words <$> minput, minput) of
        (Nothing, _) -> return ()
        (Just [], _) -> loop
        (Just [":q"], _) -> do 
          liftIO $ putStrLn "Chao ;)" 
          return () -- salir con :q
        (Just ["lexer"], _) -> do     -- llamar a la funcion lexer
          liftIO $ putStrLn "Uso: lexer <cadena>"
          loop
        (_, Just xs) -> do
          let (cmd, rest) = splitAt 5 xs
          if take 5 xs == "lexer"
            then do
              let args = drop 6 xs
              liftIO $ print $ lexerMain args
            else
              liftIO $ putStrLn $ "Comando desconocido: " ++ xs
          loop

-- main
main :: IO ()
main = do
  putStrLn "Analizador Léxico :)"
  _ <- forkIO $ watchRegexFile "regex.txt" (\_ -> return ())
  repl

