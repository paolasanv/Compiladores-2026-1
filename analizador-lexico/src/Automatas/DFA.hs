{-|
Module      : Automatas.DFA
Description : Autómatas finitos deterministas.

Este módulo implementa el algoritmo de conversión de un 
autómata finito no determinista (AFN) a un autómata finitos determinista (AFD).
-}
module Automatas.DFA where

import Automatas.NFA_E (State)
import Automatas.NFA (NFA)
import Data.Set (Set)

type DeltaDFA = (State, Char, State)

data DFA = DFA {
    states      :: Set State,
    alphabet    :: Set Char,
    transitions :: Set DeltaDFA, -- δ: Q×Σ→Q
    start       :: State,
    final       :: [State]
} deriving (Show)

toDFA :: NFA -> DFA
toDFA = undefined
