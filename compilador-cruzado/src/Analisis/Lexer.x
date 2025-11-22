{
{-|
Module      : Analisis.Lexer
Description : Analizador léxico simple.

Este módulo implementa un analizador léxico simple con ayuda de la biblioteca Alex.
-}
module Analisis.Lexer where

import Data.Char (isSpace)
}

%wrapper "basic"

$white   = [\x20\x09\x0A\x0D\x0C\x0B]
$digit   = 0-9
$letter  = [A-Za-z]

tokens :-

  -- Espacios
  $white+                ;

  -- Paréntesis
  \(                     { \_ -> TParenAbierto }
  \)                     { \_ -> TParenCerrado }

  -- Operadores
  \+                     { \_ -> TSuma }
  \-                     { \_ -> TResta }
  \*                     { \_ -> TMult }

  -- Asignación
  :=                     { \_ -> TAsiga }

  -- Números
  $digit+                { \s -> TNum (read s) }

  -- Identificadores
  $letter+               { \s -> TIdent s }

  -- Error léxico
  .                      { \s -> error ("Lexical error! Caracter no reconocido -> " ++ s) }

{

-- Definición de tokens
data Token 
    = TIdent String
    | TNum Int
    | TSuma
    | TResta
    | TMult
    | TParenAbierto
    | TParenCerrado
    | TAsiga
    deriving (Show, Eq)

-- Normalizar espacios 
normalizeSpaces :: String -> String
normalizeSpaces = map (\c -> if isSpace c then '\x20' else c)

-- Función de análisis léxico
lexer :: String -> [Token]
lexer = alexScanTokens . normalizeSpaces
}
