{-|
Module      : MDD
Description : Utiliza el AFD mínimo para construir el lexer.
Author: Leslie P. Sánchez V.

Este módulo implementa la ejecución sobre el AFD mínimo para definir la función 'lexerIMP'
la cuál es encargada de realizar el análisis léxico de una cadena de entrada.
-}
module MDD where

import Automatas.NFA_E (State, toNFAE)
import Automatas.NFA (toNFA)
import Automatas.DFA (DFA(..), transitions, toDFA)
import Automatas.DFA_min (minimize)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)
import Regex.Parser

data TokenIMP = TIdentifier String
              | TNumber Int
              | TPlus
              | TMinus
              | TEqual
              | TLeq
              | TNot
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

-- Expresión regular de IMP
regex :: RegEx
regex = Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Concat (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Character 'a') (Character 'b')) (Character 'c')) (Character 'd')) (Character 'e')) (Character 'f')) (Character 'g')) (Character 'h')) (Character 'i')) (Character 'j')) (Character 'k')) (Character 'l')) (Character 'm')) (Character 'n')) (Character 'o')) (Character 'p')) (Character 'q')) (Character 'r')) (Character 's')) (Character 't')) (Character 'u')) (Character 'v')) (Character 'w')) (Character 'x')) (Character 'y')) (Character 'z')) (Kleene (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Union (Character 'a') (Character 'b')) (Character 'c')) (Character 'd')) (Character 'e')) (Character 'f')) (Character 'g')) (Character 'h')) (Character 'i')) (Character 'j')) (Character 'k')) (Character 'l')) (Character 'm')) (Character 'n')) (Character 'o')) (Character 'p')) (Character 'q')) (Character 'r')) (Character 's')) (Character 't')) (Character 'u')) (Character 'v')) (Character 'w')) (Character 'x')) (Character 'y')) (Character 'z')))) (Character '0')) (Union (Character '-') Epsilon)) (Concat (Union (Union (Union (Union (Union (Union (Union (Union (Character '1') (Character '2')) (Character '3')) (Character '4')) (Character '5')) (Character '6')) (Character '7')) (Character '8')) (Character '9')) (Kleene (Union (Union (Union (Union (Union (Union (Union (Union (Character '1') (Character '2')) (Character '3')) (Character '4')) (Character '5')) (Character '6')) (Character '7')) (Character '8')) (Character '9'))))) (Character '+')) (Character '-')) (Character '=')) (Concat (Character '=') (Character '<'))) (Concat (Concat (Character 'n') (Character 'o')) (Character 't'))) (Concat (Concat (Character 'a') (Character 'n')) (Character 'd'))) (Concat (Concat (Concat (Character 's') (Character 'k')) (Character 'i')) (Character 'p'))) (Concat (Character 'i') (Character 'f'))) (Concat (Concat (Concat (Character 't') (Character 'h')) (Character 'e')) (Character 'n'))) (Concat (Concat (Concat (Character 'e') (Character 'l')) (Character 's')) (Character 'e'))) (Concat (Concat (Concat (Concat (Character 'w') (Character 'h')) (Character 'i')) (Character 'l')) (Character 'e'))) (Concat (Character 'd') (Character 'o'))) (Concat (Concat (Character 'f') (Character 'o')) (Character 'r'))) (Concat (Character 'i') (Character 'n'))) (Concat (Concat (Character 'e') (Character 'n')) (Character 'd'))) (Character ';')) (Concat (Character ':') (Character '='))) (Character ':')) (Concat (Character '[') (Character ']'))

lexerIMP :: String -> [TokenIMP]
lexerIMP str = lexerDo dfa str
    where 
        dfa = minimize $ toDFA $ toNFA $ toNFAE regex

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
        "not"   -> TNot
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
