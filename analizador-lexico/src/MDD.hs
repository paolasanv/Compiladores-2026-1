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
              | TDotAndComa
              | TAssign
              | TCons 
              | TList
              | TError String
              deriving (Show, Eq)

-- Lexer a llamar desde GHCi
-- La expresión regular es [a-z][a-z]* | 0 | (-|ε)[1-9][0-9]* | + | - | = | <= | not | and | skip | if | then | else | while | do | for | in | end
lexerIMP :: String -> [TokenIMP]
lexerIMP str = lexerDo dfa str
    where 
        dfa = DFA {states = fromList [0,1,77,78,79,100,103,105,107], alphabet = fromList "+-0123456789:;<=[]abcdefghijklmnopqrstuvwxyz", transitions = fromList [(0,'+',77),(0,'-',78),(0,'0',77),(0,'1',79),(0,'2',79),(0,'3',79),(0,'4',79),(0,'5',79),(0,'6',79),(0,'7',79),(0,'8',79),(0,'9',79),(0,':',103),(0,';',77),(0,'<',100),(0,'=',77),(0,'[',105),(0,']',107),(0,'a',1),(0,'b',1),(0,'c',1),(0,'d',1),(0,'e',1),(0,'f',1),(0,'g',1),(0,'h',1),(0,'i',1),(0,'j',1),(0,'k',1),(0,'l',1),(0,'m',1),(0,'n',1),(0,'o',1),(0,'p',1),(0,'q',1),(0,'r',1),(0,'s',1),(0,'t',1),(0,'u',1),(0,'v',1),(0,'w',1),(0,'x',1),(0,'y',1),(0,'z',1),(1,'+',107),(1,'-',107),(1,'0',107),(1,'1',107),(1,'2',107),(1,'3',107),(1,'4',107),(1,'5',107),(1,'6',107),(1,'7',107),(1,'8',107),(1,'9',107),(1,':',107),(1,';',107),(1,'<',107),(1,'=',107),(1,'[',107),(1,']',107),(1,'a',1),(1,'b',1),(1,'c',1),(1,'d',1),(1,'e',1),(1,'f',1),(1,'g',1),(1,'h',1),(1,'i',1),(1,'j',1),(1,'k',1),(1,'l',1),(1,'m',1),(1,'n',1),(1,'o',1),(1,'p',1),(1,'q',1),(1,'r',1),(1,'s',1),(1,'t',1),(1,'u',1),(1,'v',1),(1,'w',1),(1,'x',1),(1,'y',1),(1,'z',1),(77,'+',107),(77,'-',107),(77,'0',107),(77,'1',107),(77,'2',107),(77,'3',107),(77,'4',107),(77,'5',107),(77,'6',107),(77,'7',107),(77,'8',107),(77,'9',107),(77,':',107),(77,';',107),(77,'<',107),(77,'=',107),(77,'[',107),(77,']',107),(77,'a',107),(77,'b',107),(77,'c',107),(77,'d',107),(77,'e',107),(77,'f',107),(77,'g',107),(77,'h',107),(77,'i',107),(77,'j',107),(77,'k',107),(77,'l',107),(77,'m',107),(77,'n',107),(77,'o',107),(77,'p',107),(77,'q',107),(77,'r',107),(77,'s',107),(77,'t',107),(77,'u',107),(77,'v',107),(77,'w',107),(77,'x',107),(77,'y',107),(77,'z',107),(78,'+',107),(78,'-',107),(78,'0',107),(78,'1',79),(78,'2',79),(78,'3',79),(78,'4',79),(78,'5',79),(78,'6',79),(78,'7',79),(78,'8',79),(78,'9',79),(78,':',107),(78,';',107),(78,'<',107),(78,'=',107),(78,'[',107),(78,']',107),(78,'a',107),(78,'b',107),(78,'c',107),(78,'d',107),(78,'e',107),(78,'f',107),(78,'g',107),(78,'h',107),(78,'i',107),(78,'j',107),(78,'k',107),(78,'l',107),(78,'m',107),(78,'n',107),(78,'o',107),(78,'p',107),(78,'q',107),(78,'r',107),(78,'s',107),(78,'t',107),(78,'u',107),(78,'v',107),(78,'w',107),(78,'x',107),(78,'y',107),(78,'z',107),(79,'+',107),(79,'-',107),(79,'0',79),(79,'1',79),(79,'2',79),(79,'3',79),(79,'4',79),(79,'5',79),(79,'6',79),(79,'7',79),(79,'8',79),(79,'9',79),(79,':',107),(79,';',107),(79,'<',107),(79,'=',107),(79,'[',107),(79,']',107),(79,'a',107),(79,'b',107),(79,'c',107),(79,'d',107),(79,'e',107),(79,'f',107),(79,'g',107),(79,'h',107),(79,'i',107),(79,'j',107),(79,'k',107),(79,'l',107),(79,'m',107),(79,'n',107),(79,'o',107),(79,'p',107),(79,'q',107),(79,'r',107),(79,'s',107),(79,'t',107),(79,'u',107),(79,'v',107),(79,'w',107),(79,'x',107),(79,'y',107),(79,'z',107),(100,'+',107),(100,'-',107),(100,'0',107),(100,'1',107),(100,'2',107),(100,'3',107),(100,'4',107),(100,'5',107),(100,'6',107),(100,'7',107),(100,'8',107),(100,'9',107),(100,':',107),(100,';',107),(100,'<',107),(100,'=',77),(100,'[',107),(100,']',107),(100,'a',107),(100,'b',107),(100,'c',107),(100,'d',107),(100,'e',107),(100,'f',107),(100,'g',107),(100,'h',107),(100,'i',107),(100,'j',107),(100,'k',107),(100,'l',107),(100,'m',107),(100,'n',107),(100,'o',107),(100,'p',107),(100,'q',107),(100,'r',107),(100,'s',107),(100,'t',107),(100,'u',107),(100,'v',107),(100,'w',107),(100,'x',107),(100,'y',107),(100,'z',107),(103,'+',107),(103,'-',107),(103,'0',107),(103,'1',107),(103,'2',107),(103,'3',107),(103,'4',107),(103,'5',107),(103,'6',107),(103,'7',107),(103,'8',107),(103,'9',107),(103,':',107),(103,';',107),(103,'<',107),(103,'=',77),(103,'[',107),(103,']',107),(103,'a',107),(103,'b',107),(103,'c',107),(103,'d',107),(103,'e',107),(103,'f',107),(103,'g',107),(103,'h',107),(103,'i',107),(103,'j',107),(103,'k',107),(103,'l',107),(103,'m',107),(103,'n',107),(103,'o',107),(103,'p',107),(103,'q',107),(103,'r',107),(103,'s',107),(103,'t',107),(103,'u',107),(103,'v',107),(103,'w',107),(103,'x',107),(103,'y',107),(103,'z',107),(105,'+',107),(105,'-',107),(105,'0',107),(105,'1',107),(105,'2',107),(105,'3',107),(105,'4',107),(105,'5',107),(105,'6',107),(105,'7',107),(105,'8',107),(105,'9',107),(105,':',107),(105,';',107),(105,'<',107),(105,'=',107),(105,'[',107),(105,']',77),(105,'a',107),(105,'b',107),(105,'c',107),(105,'d',107),(105,'e',107),(105,'f',107),(105,'g',107),(105,'h',107),(105,'i',107),(105,'j',107),(105,'k',107),(105,'l',107),(105,'m',107),(105,'n',107),(105,'o',107),(105,'p',107),(105,'q',107),(105,'r',107),(105,'s',107),(105,'t',107),(105,'u',107),(105,'v',107),(105,'w',107),(105,'x',107),(105,'y',107),(105,'z',107),(107,'+',107),(107,'-',107),(107,'0',107),(107,'1',107),(107,'2',107),(107,'3',107),(107,'4',107),(107,'5',107),(107,'6',107),(107,'7',107),(107,'8',107),(107,'9',107),(107,':',107),(107,';',107),(107,'<',107),(107,'=',107),(107,'[',107),(107,']',107),(107,'a',107),(107,'b',107),(107,'c',107),(107,'d',107),(107,'e',107),(107,'f',107),(107,'g',107),(107,'h',107),(107,'i',107),(107,'j',107),(107,'k',107),(107,'l',107),(107,'m',107),(107,'n',107),(107,'o',107),(107,'p',107),(107,'q',107),(107,'r',107),(107,'s',107),(107,'t',107),(107,'u',107),(107,'v',107),(107,'w',107),(107,'x',107),(107,'y',107),(107,'z',107)], start = 0, final = [1,77,78,79,103]}


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
