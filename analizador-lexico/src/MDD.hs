{-|
Module      : MDD
Description : Utiliza el AFD mínimo para construir el lexer.
Author: Leslie P. Sánchez V.

Este módulo implementa la ejecución sobre el AFD mínimo para definir la función 'lexerIMP'
la cuál es encargada de realizar el análisis léxico de una cadena de entrada.
-}
module MDD where

import Automatas.NFA_E (State)
import Automatas.DFA (DFA(..), transitions)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

data TokenIMP = TIdentifier String
              | TNumber Int
              | TPlus
              | TMinus
              | TEqual
              | TLeq
              | TTNot
              | TAnd
              | TSkip
              | TIf
              | TThen
              | TElse
              | TWhile
              | TDo
              | TIn
              | TEnd
              | TDotAndComa
              | TAssign
              | TCons 
              | TList
              | TError String
              deriving (Show, Eq)

lexerDo :: DFA -> String ->  [TokenIMP]
lexerDo _ []  = []
lexerDo dfa w@(x:xs)  
    | x `elem` [' ', '\t', '\n'] = lexerDo dfa xs -- Ignorar espacios
    | otherwise =
        case (maximalMunch dfa (start dfa) w "" Nothing) of  -- w como entrada del DFA
            Just (lexeme, rest) -> tokenizeLexeme lexeme : lexerDo dfa rest 
            Nothing -> TError ("Caracter no reconocido: " ++ [x]) : lexerDo dfa xs 

maximalMunch :: DFA -> State -> String -> String -> Maybe (String, String) -> Maybe (String, String)
maximalMunch dfa q [] substring backtracking = backtracking     -- Se consumieron todos los caracteres
maximalMunch dfa q (wj:ww) substring backtracking =             -- Hay caracteres por consumir
-- Para cada wj hacer:
--  Aplicar δ(q, wj)   
    case move dfa q wj of  
--      Si δ(q, wi) = q'                            
        Just q' ->         
--          Si q' es estado final entonces     
            if isFinal dfa q'
--             El lexema reconocido hasta ahora es wi...wj que ya es válido
--             pero hay que verificar si existe wj+1 y si el posible extender la cadena reconocida (longest match)
            then maximalMunch dfa q' ww (substring ++ [wj]) (Just (substring ++ [wj], ww)) 
--          Si q' no es estado final entonces 
--             El lexema reconocido hasta ahora es wi...wj pero este aún no es válido, hay que seguir consumiendo caracteres (maximal munch)
            else maximalMunch dfa q' ww (substring ++ [wj]) backtracking
--      Si δ(q, wj) =  ∅ regresa el último token reconocido (si existe) 
        Nothing -> backtracking 

-- Verifica si un estado es final
isFinal :: DFA -> State -> Bool
isFinal dfa q = q `elem` final dfa

-- dado un AFD, un estado q y un caracter c, regresa el estado q' tal que δ(q, c) = q' 
move :: DFA -> State -> Char -> Maybe State
move dfa q c = listToMaybe [q' | (q0, x, q') <- Set.toList (transitions dfa),q0 == q, x == c]

tokenizeLexeme :: String -> TokenIMP
tokenizeLexeme lexeme =
    case lexeme of
        "+"     -> TPlus
        "-"     -> TMinus
        "="     -> TEqual
        "<="    -> TLeq
        "not"   -> TTNot
        "and"   -> TAnd
        "skip"  -> TSkip
        "if"    -> TIf
        "then"  -> TThen
        "else"  -> TElse
        "while" -> TWhile
        "do"    -> TDo
        "in"    -> TIn
        "end"   -> TEnd
        ";"     -> TDotAndComa
        ":="    -> TAssign
        ":"     -> TCons 
        "[]"    -> TList
        _       ->
            case readMaybe lexeme of
                Just n  -> TNumber n
                Nothing -> TIdentifier lexeme
