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
import Data.Set

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
              | TTrue 
              | TFalse
              | TDotAndComa
              | TAssign
              | TCons 
              | TList
              | TError String
              deriving (Show, Eq)

-- Lexer a llamar desde GHCi
-- La expresión regular es [a-z][a-z]*(ε | [1-9]) | 0 | (-|ε)[1-9][0-9]* | + | - | = | <= | not | and | true | false | skip | if | then | else | while | do | for | in | end
lexerIMP :: String -> [TokenIMP]
lexerIMP str = lexerDo dfa str
    where 
        dfa = DFA {states = fromList [0,1,84,94,95,116,119,121,123], alphabet = fromList "+-0123456789:;<=[]abcdefghijklmnopqrstuvwxyz", transitions = fromList [(0,'+',84),(0,'-',94),(0,'0',84),(0,'1',95),(0,'2',95),(0,'3',95),(0,'4',95),(0,'5',95),(0,'6',95),(0,'7',95),(0,'8',95),(0,'9',95),(0,':',119),(0,';',84),(0,'<',116),(0,'=',84),(0,'[',121),(0,']',123),(0,'a',1),(0,'b',1),(0,'c',1),(0,'d',1),(0,'e',1),(0,'f',1),(0,'g',1),(0,'h',1),(0,'i',1),(0,'j',1),(0,'k',1),(0,'l',1),(0,'m',1),(0,'n',1),(0,'o',1),(0,'p',1),(0,'q',1),(0,'r',1),(0,'s',1),(0,'t',1),(0,'u',1),(0,'v',1),(0,'w',1),(0,'x',1),(0,'y',1),(0,'z',1),(1,'+',123),(1,'-',123),(1,'0',123),(1,'1',84),(1,'2',84),(1,'3',84),(1,'4',84),(1,'5',84),(1,'6',84),(1,'7',84),(1,'8',84),(1,'9',84),(1,':',123),(1,';',123),(1,'<',123),(1,'=',123),(1,'[',123),(1,']',123),(1,'a',1),(1,'b',1),(1,'c',1),(1,'d',1),(1,'e',1),(1,'f',1),(1,'g',1),(1,'h',1),(1,'i',1),(1,'j',1),(1,'k',1),(1,'l',1),(1,'m',1),(1,'n',1),(1,'o',1),(1,'p',1),(1,'q',1),(1,'r',1),(1,'s',1),(1,'t',1),(1,'u',1),(1,'v',1),(1,'w',1),(1,'x',1),(1,'y',1),(1,'z',1),(84,'+',123),(84,'-',123),(84,'0',123),(84,'1',123),(84,'2',123),(84,'3',123),(84,'4',123),(84,'5',123),(84,'6',123),(84,'7',123),(84,'8',123),(84,'9',123),(84,':',123),(84,';',123),(84,'<',123),(84,'=',123),(84,'[',123),(84,']',123),(84,'a',123),(84,'b',123),(84,'c',123),(84,'d',123),(84,'e',123),(84,'f',123),(84,'g',123),(84,'h',123),(84,'i',123),(84,'j',123),(84,'k',123),(84,'l',123),(84,'m',123),(84,'n',123),(84,'o',123),(84,'p',123),(84,'q',123),(84,'r',123),(84,'s',123),(84,'t',123),(84,'u',123),(84,'v',123),(84,'w',123),(84,'x',123),(84,'y',123),(84,'z',123),(94,'+',123),(94,'-',123),(94,'0',123),(94,'1',95),(94,'2',95),(94,'3',95),(94,'4',95),(94,'5',95),(94,'6',95),(94,'7',95),(94,'8',95),(94,'9',95),(94,':',123),(94,';',123),(94,'<',123),(94,'=',123),(94,'[',123),(94,']',123),(94,'a',123),(94,'b',123),(94,'c',123),(94,'d',123),(94,'e',123),(94,'f',123),(94,'g',123),(94,'h',123),(94,'i',123),(94,'j',123),(94,'k',123),(94,'l',123),(94,'m',123),(94,'n',123),(94,'o',123),(94,'p',123),(94,'q',123),(94,'r',123),(94,'s',123),(94,'t',123),(94,'u',123),(94,'v',123),(94,'w',123),(94,'x',123),(94,'y',123),(94,'z',123),(95,'+',123),(95,'-',123),(95,'0',95),(95,'1',95),(95,'2',95),(95,'3',95),(95,'4',95),(95,'5',95),(95,'6',95),(95,'7',95),(95,'8',95),(95,'9',95),(95,':',123),(95,';',123),(95,'<',123),(95,'=',123),(95,'[',123),(95,']',123),(95,'a',123),(95,'b',123),(95,'c',123),(95,'d',123),(95,'e',123),(95,'f',123),(95,'g',123),(95,'h',123),(95,'i',123),(95,'j',123),(95,'k',123),(95,'l',123),(95,'m',123),(95,'n',123),(95,'o',123),(95,'p',123),(95,'q',123),(95,'r',123),(95,'s',123),(95,'t',123),(95,'u',123),(95,'v',123),(95,'w',123),(95,'x',123),(95,'y',123),(95,'z',123),(116,'+',123),(116,'-',123),(116,'0',123),(116,'1',123),(116,'2',123),(116,'3',123),(116,'4',123),(116,'5',123),(116,'6',123),(116,'7',123),(116,'8',123),(116,'9',123),(116,':',123),(116,';',123),(116,'<',123),(116,'=',84),(116,'[',123),(116,']',123),(116,'a',123),(116,'b',123),(116,'c',123),(116,'d',123),(116,'e',123),(116,'f',123),(116,'g',123),(116,'h',123),(116,'i',123),(116,'j',123),(116,'k',123),(116,'l',123),(116,'m',123),(116,'n',123),(116,'o',123),(116,'p',123),(116,'q',123),(116,'r',123),(116,'s',123),(116,'t',123),(116,'u',123),(116,'v',123),(116,'w',123),(116,'x',123),(116,'y',123),(116,'z',123),(119,'+',123),(119,'-',123),(119,'0',123),(119,'1',123),(119,'2',123),(119,'3',123),(119,'4',123),(119,'5',123),(119,'6',123),(119,'7',123),(119,'8',123),(119,'9',123),(119,':',123),(119,';',123),(119,'<',123),(119,'=',84),(119,'[',123),(119,']',123),(119,'a',123),(119,'b',123),(119,'c',123),(119,'d',123),(119,'e',123),(119,'f',123),(119,'g',123),(119,'h',123),(119,'i',123),(119,'j',123),(119,'k',123),(119,'l',123),(119,'m',123),(119,'n',123),(119,'o',123),(119,'p',123),(119,'q',123),(119,'r',123),(119,'s',123),(119,'t',123),(119,'u',123),(119,'v',123),(119,'w',123),(119,'x',123),(119,'y',123),(119,'z',123),(121,'+',123),(121,'-',123),(121,'0',123),(121,'1',123),(121,'2',123),(121,'3',123),(121,'4',123),(121,'5',123),(121,'6',123),(121,'7',123),(121,'8',123),(121,'9',123),(121,':',123),(121,';',123),(121,'<',123),(121,'=',123),(121,'[',123),(121,']',84),(121,'a',123),(121,'b',123),(121,'c',123),(121,'d',123),(121,'e',123),(121,'f',123),(121,'g',123),(121,'h',123),(121,'i',123),(121,'j',123),(121,'k',123),(121,'l',123),(121,'m',123),(121,'n',123),(121,'o',123),(121,'p',123),(121,'q',123),(121,'r',123),(121,'s',123),(121,'t',123),(121,'u',123),(121,'v',123),(121,'w',123),(121,'x',123),(121,'y',123),(121,'z',123),(123,'+',123),(123,'-',123),(123,'0',123),(123,'1',123),(123,'2',123),(123,'3',123),(123,'4',123),(123,'5',123),(123,'6',123),(123,'7',123),(123,'8',123),(123,'9',123),(123,':',123),(123,';',123),(123,'<',123),(123,'=',123),(123,'[',123),(123,']',123),(123,'a',123),(123,'b',123),(123,'c',123),(123,'d',123),(123,'e',123),(123,'f',123),(123,'g',123),(123,'h',123),(123,'i',123),(123,'j',123),(123,'k',123),(123,'l',123),(123,'m',123),(123,'n',123),(123,'o',123),(123,'p',123),(123,'q',123),(123,'r',123),(123,'s',123),(123,'t',123),(123,'u',123),(123,'v',123),(123,'w',123),(123,'x',123),(123,'y',123),(123,'z',123)], start = 0, final = [1,84,94,95,119]}

lexerDo :: DFA -> String ->  [TokenIMP]
lexerDo _ []  = []
lexerDo dfa w@(x:xs)  
    | x `elem` [' ', '\t', '\n'] = lexerDo dfa xs -- Ignorar espacios
    | otherwise =
        case (maximalMunch dfa (start dfa) w "" Nothing) of  -- w como entrada del DFA
            Just (lexeme, rest) -> tokenizeLexeme lexeme : lexerDo dfa rest 
            Nothing -> TError ("Caracter no reconocido: " ++ [x]) : lexerDo dfa xs 

maximalMunch :: DFA -> State -> String -> String -> Maybe (String, String) -> Maybe (String, String)
maximalMunch _ _ [] _ backtracking = backtracking     -- Se consumieron todos los caracteres
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
        "true"  -> TTrue
        "false" -> TFalse
        ";"     -> TDotAndComa
        ":="    -> TAssign
        ":"     -> TCons 
        "[]"    -> TList
        _       ->
            case readMaybe lexeme of
                Just n  -> TNumber n
                Nothing -> TIdentifier lexeme
