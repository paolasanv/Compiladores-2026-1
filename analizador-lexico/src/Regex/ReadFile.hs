{-# LANGUAGE OverloadedStrings #-}
module Regex.ReadFile where

import Regex.Lexer
import Regex.Parser

import Data.List (intercalate)
import System.FSNotify
import System.FilePath (takeFileName, takeDirectory)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_analizador_lexico (getDataFileName)
import System.Directory (doesFileExist)


-- De cadena a RegEx
toRegex :: String -> RegEx 
toRegex x = parser $ lexer x

-- Convierte un texto en una lista de cadenas, separando por el símbolo de concatenación
text2String :: T.Text -> String
text2String content = intercalate " | " ( map T.unpack (T.lines content))

-- Función que se ejecuta cuando el archivo es modificado
processFIle :: FilePath -> IO ()
processFIle path = do
    content <- TIO.readFile path
    putStrLn "\nArchivo procesado. Actualizando las expresiones regulares obtenidas"
    putStrLn "Expresion regular"
    print(toRegex (text2String content))

-- Funcion principal
file2RegEx :: FilePath -> IO ()
file2RegEx filename = withManager $ \mgr -> do
    -- Intentar primero con archivo local
    existeLocal <- doesFileExist filename
    ruta <- if existeLocal
            then return filename
            else getDataFileName filename

    -- Ejecutar inmediatamente la primera vez
    processFIle ruta

    -- Obtenemos la carpeta y el nombre del archivo
    let carpeta = takeDirectory ruta
    let archivo = takeFileName ruta

    -- Vigilar cambios en tiempo real
    watchDir
      mgr
      carpeta
      (\e -> case e of
          Modified path _ _ -> takeFileName path == archivo
          _ -> False)
      (\_ -> do
          putStrLn $ "Cambio detectado en: " ++ archivo
          processFIle ruta)

    putStrLn $ "Observando " ++ archivo ++ " en " ++ carpeta ++ " por cambios. Presiona Ctrl+C para salir."
    forever $ threadDelay 1000000