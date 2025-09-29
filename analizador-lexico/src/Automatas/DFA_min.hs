{-|
Module      : Automatas.DFA
Description : Autómatas finitos deterministas minimos.

Este módulo implementa el algoritmo de minimización de un 
autómata finito determinista (AFD).
-}
module Automatas.DFA_min where

import Automatas.NFA_E (State)
import Automatas.DFA (DFA(..), DeltaDFA, alphabet, final, start, states, transitions)
import qualified Data.Set as Set
import Data.Set (Set)

min :: DFA -> DFA
min dfa = 
    let dfa' = deleletInaccessible dfa 
    in undefined --aqui va el resto del algoritmo


-- Da un nuevo automata con todos sus estados alcanzabes
deleletInaccessible :: DFA -> DFA
deleletInaccessible dfa = DFA {
    states = newStates,
    alphabet = alphabet dfa,
    transitions = Set.filter (\(p, _, q) -> Set.member p newStates && Set.member q newStates) (transitions dfa) ,
    start = start dfa,
    final = [f | f <- final dfa, Set.member f newStates]  
    }
    where 
        newStates =  accessibleStates (transitions dfa)  (start dfa) 

-- Calcula los estados alcanzables desde el estado inicial
accessibleStates ::  Set DeltaDFA  -> State ->  Set State
accessibleStates trs q0 = accessible trs [q0] Set.empty 

-- Calcula los estados alanzables desde una lista de estados
accessible :: Set DeltaDFA -> [State] -> Set State -> Set State 
accessible _ [] acc = acc
accessible trs (p:xs) acc
    -- p ya fue añadido a los alcanzables
    |  Set.member p acc = accessible trs xs acc    
    -- si p es alcanzable se añaden los estados q tales que δ(p,a)=q 
    | otherwise = accessible trs ([q | (p', _, q) <- Set.toList trs, p == p'] ++ xs) (Set.insert p acc)


-- Obtener los estados equivalentes
equivalenceClasses :: DFA -> [State]
equivalenceClasses = undefined




