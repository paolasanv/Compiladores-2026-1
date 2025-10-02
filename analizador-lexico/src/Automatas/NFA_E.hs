{-|
Module      : Automatas.NFA_E
Description : Autómatas finitos no deterministas con transiciones épsilon.

Este módulo implementa el algoritmo de Thompson para convertir una expresión regular
en un autómata finitos no determinista con transiciones épsilon (AFN-ε).
-}
module Automatas.NFA_E where

import Regex.Parser (RegEx(..))
import Data.Set (Set)

type State = Int
type Symbol = Maybe Char
type Delta = (State, Symbol, State)

data NFAE = NFAE {
    states      :: Set State,
    alphabet    :: Set Symbol,
    transitions :: Set Delta,
    start       :: State,
    final       :: State
} deriving (Show)


toNFAE :: RegEx -> NFAE 
toNFAE = undefined
