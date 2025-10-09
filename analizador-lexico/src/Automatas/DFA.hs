{-|
Module      : Automatas.DFA
Description : Autómatas finitos deterministas.
Author: Paola Mildred Martinez Hidalgo.

Este módulo implementa el algoritmo de conversión de un 
autómata finito no determinista (AFN) a un autómata finitos determinista (AFD).
-}
module Automatas.DFA where

import Automatas.NFA_E (State)
import Automatas.NFA (NFA(..))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type DeltaDFA = (State, Char, State)

data DFA = DFA {
    states      :: Set State,
    alphabet    :: Set Char,
    transitions :: Set DeltaDFA, -- δ: Q×Σ→Q
    start       :: State,
    final       :: [State]
} deriving (Show)

-- Construye un DFA a partir de un NFA (sin ε-transitions)
toDFA :: NFA -> DFA
toDFA nfa =
    let
        -- Se mantiene el estado inicial del NFA
        startSet = Set.singleton (startNFA nfa)

        -- Devuelve los estados alcanzables desde un conjunto de estados con un símbolo
        move :: Set State -> Char -> Set State
        move estados simbolo =
            Set.unions [ Set.fromList qs
                       | (q, a, qs) <- Set.toList (transitionsNFA nfa)
                       , q `Set.member` estados
                       , a == simbolo
                       , not (null qs)
                       ]

        -- Construcción recursiva de los estados del DFA
        build :: [Set State] -> Set (Set State) -> Map.Map (Set State, Char) (Set State)
              -> (Set (Set State), Map.Map (Set State, Char) (Set State))
        build [] visitados delta = (visitados, delta) -- si no hay más edos por procesar, devuelve los visitados y las transiciones
        build (actual:cola) visitados delta =
            let alfabeto = Set.toList (alphabetNFA nfa)
                movimientos = [ (a, move actual a) | a <- alfabeto, not (Set.null (move actual a)) ] --indica para donde moverse con cada símbolo
                nuevos = [ s | (_, s) <- movimientos, not (s `Set.member` visitados) ] -- estados que aún no se visitan
                delta' = foldr (\(a,s) acc -> Map.insert (actual,a) s acc) delta movimientos -- actualiza las transiciones
            in build (cola ++ nuevos) (Set.union visitados (Set.fromList nuevos)) delta'

        -- Cada conjunto de estados del NFA se representa por el estado mínimo del conjunto.
        representative :: Set State -> State
        representative s = Set.findMin s

        (allStates, deltaMap) = build [startSet] (Set.singleton startSet) Map.empty

        -- Convierte el Map de transiciones en un Set DeltaDFA
        deltaDFA = Set.fromList
          [ (representative qSet, a, representative qSet')
          | ((qSet,a), qSet') <- Map.toList deltaMap
          ]

        -- Calcula los estados finales del DFA, es final si contiene un estado final del NFA 
        finals = [ representative s
                 | s <- Set.toList allStates
                 , any (`elem` finalNFA nfa) (Set.toList s)
                 ]
    -- Construye el DFA final 
    in DFA {
        states = Set.map representative allStates,
        alphabet = alphabetNFA nfa,
        transitions = deltaDFA,
        start = representative startSet,
        final = finals
    }

-- Transición extendida de un DFA
deltaHat :: DFA -> State -> Char -> Maybe State
deltaHat dfa q c =
  case [q' | (q1, a, q') <- Set.toList (transitions dfa), q1 == q, a == c] of
    (q':_) -> Just q'
    []     -> Nothing