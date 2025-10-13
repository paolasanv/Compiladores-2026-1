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
import Automatas.DFA (DFA, toDFA)
import Automatas.DFA_min (minimize)

import Data.List (intercalate)
import System.FSNotify
import System.FilePath (takeFileName, takeDirectory)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Paths_analizador_lexico (getDataFileName)
import System.Directory (doesFileExist)

-- Convierte texto a expresión regular
toRegex :: String -> RegEx
toRegex x = parser $ lexer x

-- Convierte texto a String con separador |
text2String :: T.Text -> String
text2String content = intercalate " | " (map T.unpack (T.lines content))

-- Cargar y parsear el archivo regex.txt como expresión regular
loadRegexFromFile :: IO RegEx
loadRegexFromFile = do
   existeLocal <- doesFileExist "regex.txt"
   ruta <- if existeLocal then return "regex.txt" else getDataFileName "regex.txt"
   contenido <- T.readFile ruta
   let regex = toRegex (text2String contenido)
   putStrLn $ "[Actualización] Expresión regular cargada: " ++ show regex
   return regex

-- Construye un DFA mínimo desde una expresión regular
toDFAmin :: RegEx -> DFA
toDFAmin = minimize . toDFA . toNFA . toNFAE


-- Vigilar el archivo regex.txt y actualizar el MVar con el nuevo DFA
watchRegexFile :: MVar DFA -> IO ()
watchRegexFile ref = withManager $ \mgr -> do
   existeLocal <- doesFileExist "regex.txt"
   ruta <- if existeLocal then return "regex.txt" else getDataFileName "regex.txt"
   let carpeta = takeDirectory ruta
   let archivo = takeFileName ruta

   void $ watchDir
       mgr
       carpeta
       (\e -> case e of
           Modified path _ _ -> takeFileName path == archivo
           _ -> False)
       (\_ -> do
           putStrLn $ "\n[FSNotify] Cambio detectado en " ++ archivo
           regex' <- loadRegexFromFile
           let afd' = toDFAmin regex'
           modifyMVar_ ref (const (return afd'))
       )

   forever $ threadDelay 1000000

repl :: MVar DFA -> IO ()
repl ref = runInputT defaultSettings loop
 where
   loop :: InputT IO ()
   loop = do
     minput <- getInputLine "> "
     case minput of
       Nothing -> return ()
       Just "" -> loop
       Just ":q" -> liftIO $ putStrLn "Chao :)"
       Just xs ->
         if take 5 xs == "lexer"
           then do
             let rawArgs = drop 6 xs
             let args = stripQuotes rawArgs
             afd <- liftIO $ readMVar ref
             liftIO $ print (lexerDo afd args)
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
 -- Cargar regex inicial y generar AFD mínimo
 regex <- loadRegexFromFile
 ref <- newMVar (toDFAmin regex)
  -- Lanzar observador de archivos en un hilo separado
 _ <- forkIO $ watchRegexFile ref
  -- Iniciar REPL
 repl ref
