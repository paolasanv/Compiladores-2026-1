{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO, MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (forever, void)

import Regex.Lexer
import Regex.Parser
import MDD (lexerDo)
import Automatas.NFA_E (toNFAE)
import Automatas.NFA (toNFA)
import Automatas.DFA (toDFA)
import Automatas.DFA_min (minimize)

import Data.List (intercalate)
import System.FSNotify
import System.FilePath (takeFileName, takeDirectory)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Paths_analizador_lexico (getDataFileName)
import System.Directory (doesFileExist)


-- Conversión de archivo a expresión regular
toRegex :: String -> RegEx
toRegex x = parser $ lexer x

text2String :: T.Text -> String
text2String content = intercalate " | " (map T.unpack (T.lines content))

loadRegexFromFile :: IO RegEx
loadRegexFromFile = do
    existeLocal <- doesFileExist "regex.txt"
    ruta <- if existeLocal then return "regex.txt" else getDataFileName "regex.txt"
    contenido <- T.readFile ruta
    let regex = toRegex (text2String contenido)
    putStrLn $ "[Actualización] " ++ show regex
    return regex

-- Vigilar cambios en el archivo regex.txt
watchRegexFile :: MVar RegEx -> IO ()
watchRegexFile ref = withManager $ \mgr -> do
    existeLocal <- doesFileExist "regex.txt"
    ruta <- if existeLocal then return "regex.txt" else getDataFileName "regex.txt"
    let carpeta = takeDirectory ruta
    let archivo = takeFileName ruta

    -- Observador de cambios
    void $ watchDir
        mgr
        carpeta
        (\e -> case e of
            Modified path _ _ -> takeFileName path == archivo
            _ -> False)
        (\_ -> do
            putStrLn $ "\nCambio detectado en " ++ archivo
            regex' <- loadRegexFromFile
            modifyMVar_ ref (const (return regex'))
        )
    forever $ threadDelay 1000000


repl :: MVar RegEx -> IO ()
repl ref = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just "" -> loop
        Just ":q" -> liftIO (putStrLn "Chao :)")
        Just xs ->
          if take 5 xs == "lexer"
            then do
              let rawArgs = drop 6 xs
              let args = stripQuotes rawArgs
              regex <- liftIO $ readMVar ref
              liftIO $ do
                putStrLn "[Ejecución] Usando expresión regular actual:"
                print regex
                putStrLn $ "lexer se llama con: " ++ args
                let afd = minimize $ toDFA $ toNFA $ toNFAE regex
                print (lexerDo afd args)
              loop
            else do
              liftIO $ putStrLn $ "Comando desconocido: " ++ xs
              loop

    -- Elimina comillas dobles al inicio y final, si existen
    stripQuotes :: String -> String
    stripQuotes s =
      case s of
        ('"':rest) | not (null rest) && last rest == '"' -> init rest
        _ -> s

main :: IO ()
main = do
  putStrLn "\n======= Analizador Léxico :) =======\n"
  ref <- newMVar =<< loadRegexFromFile
  _ <- forkIO $ watchRegexFile ref
  repl ref