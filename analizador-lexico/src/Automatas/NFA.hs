{-|
Module      : Automatas.NFA
Description : Autómatas finitos no deterministas.

Este módulo implementa el algoritmo de conversión de un 
autómata finito no determinista con transiciones épsilon (AFN-ε)
a un autómata finitos no determinista (AFN).
-}
module Automatas.NFA where

import Automatas.NFA_E (NFAE, State, Delta, Symbol, transitions, alphabet, states)
import qualified Data.Set as Set
import Data.Set (Set)

type DeltaNoE = (State, Char, [State])

data NFA = NFA {
    statesNFA      :: Set State,
    alphabetNFA    :: Set Char,
    transitionsNFA :: Set DeltaNoE,
    startNFA       :: State,
    finalNFA       :: [State]
}deriving (Show)


toNFA :: NFAE -> NFA
toNFA = undefined
