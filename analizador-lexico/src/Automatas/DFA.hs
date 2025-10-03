{-|
Module      : Automatas.DFA
Description : Autómatas finitos deterministas.

Este módulo implementa el algoritmo de conversión de un 
autómata finito no determinista (AFN) a un autómata finitos determinista (AFD).
-}
module Automatas.DFA where

import Automatas.NFA_E (State)
import Automatas.NFA (NFA)
import qualified Data.Set as Set
import Data.Set (Set)

type DeltaDFA = (State, Char, State)

data DFA = DFA {
    states      :: Set State,
    alphabet    :: Set Char,
    transitions :: Set DeltaDFA, -- δ: Q×Σ→Q
    start       :: State,
    final       :: [State]
} deriving (Show)

-- Transición extendida de un DFA
deltaHat :: DFA -> State -> Char -> State
deltaHat dfa q c =
    case [q' | (q1, a, q') <- Set.toList (transitions dfa), q1 == q, a == c] of
        (q':_) -> q'           -- si existe transición (única), la regresamos
        []     -> error $ "No hay transición definida para el estado "
                        ++ show q ++ " con símbolo " ++ show c

toDFA :: NFA -> DFA
toDFA = undefined
