{-|
Module      : Regex.Lexer
Description : Analizador léxico para expresiones regulares.

Este módulo implementa un analizador léxico simple que transforma
una cadena en una lista de tokens que representan una expresión regular.
-}
module Regex.Lexer where

import Data.Char ( isAlpha, isDigit, isSpace )

-- | Conjunto de tokens válidos para las expresiones regulares.
data Token 
    = EmptyS           -- ^ Representa la cadena vacía (∅).
    | EpsilonS         -- ^ Representa el símbolo épsilon (ε).
    | CharacterS Char  -- ^ Un carácter alfanumérico o símbolo permitido.
    | UnionS           -- ^ Operador de unión: (|).
    | ConcatS          -- ^ Operador de concatenación: (.).
    | KleeneS          -- ^ Operador de estrella de Kleene: (*).
    | OpenParenthesis  -- ^ Paréntesis de apertura: '('.
    | CloseParenthesis -- ^ Paréntesis de cierre: ')'.
    deriving (Show, Eq)

-- | Función principal del analizador léxico.
--
-- Convierte una cadena de entrada en una lista de tokens.
-- 
lexer :: String -> [Token]
lexer x
    | null x = [EmptyS]                        -- Entrada vacía.
    | all isSpace x && not (null x) = [EpsilonS] -- Solo espacios: epsilon.
    | otherwise = lexerRegex x

-- | Función auxiliar que recorre la cadena y produce tokens
-- según los caracteres encontrados.
lexerRegex :: String -> [Token]
lexerRegex [] = []
lexerRegex ('ε':xs) = EpsilonS : lexerRegex xs
lexerRegex ('(':xs) = OpenParenthesis : lexerRegex xs
lexerRegex (')':xs) = CloseParenthesis : lexerRegex xs
lexerRegex ('.':xs) = ConcatS : lexerRegex xs
lexerRegex (a:xs)
    | isDigit a = CharacterS a : lexerRegex xs
    | isAlpha a = CharacterS a : lexerRegex xs
    | a == '|'  = UnionS : lexerRegex xs
    | a == '*'  = KleeneS : lexerRegex xs
lexerRegex (a:xs)
    | a `elem` ['+','-',':','[',']',';','=','≤','<','>','!', '"', '#', '$', '%', '&', ',', '/', '\\', '|', '{', '}', '?', '@', '^', '_', '`', '~'] 
        = CharacterS a : lexerRegex xs
lexerRegex (_:xs) = lexerRegex xs
