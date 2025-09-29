{-|
Module      : Automatas.NFA
Description : Autómatas finitos no deterministas.

Este módulo implementa el algoritmo de conversión de un 
autómata finito no determinista con transitions épsilon (AFN-ε)
a un autómata finitos no determinista (AFN).
-}
module Automatas.NFA where

import Automatas.NFA_E (NFAE, State, Delta, Symbol)
import qualified Data.Set as Set
import Data.Set (Set)

type DeltaNoE = (State, Char, [State])

data NFA = NFA {
    states      :: Set State,
    alphabet    :: Set Char,
    transitions :: Set DeltaNoE,
    start       :: State,
    final       :: [State]
}deriving (Show)

toNFA :: NFAE -> NFA
toNFA = undefined

-- ε-closure(q) = { p | δ(q, ε) =  p }
stateEclosure :: State -> Set Delta -> Set State
stateEclosure q d = stateEclosureAux q (Set.toList d)

stateEclosureAux :: State -> [Delta] -> Set State
stateEclosureAux _ [] = Set.empty
stateEclosureAux q (x:xs) = case x of
                         (q1, Nothing, q2) | q1 == q -> Set.unions [Set.singleton q2, (stateEclosureAux q xs), (stateEclosureAux q2 xs)]
                         _ -> stateEclosureAux q xs

-- δ^ (q, ε) = ε-closure(q) 
-- δ^ (q, a) =  ε-closure(δ(ε-closure(q),a)) 
deltaHat :: State -> Symbol -> Set Delta -> Set State
deltaHat q c d = Set.unions [doDeltaHat x c d | x <- Set.toList (stateEclosure q d)]

doDeltaHat :: State -> Symbol -> Set Delta -> Set State
doDeltaHat q c d = doDeltaHatAux q c (Set.toList d)

doDeltaHatAux :: State -> Symbol -> [Delta] -> Set State
doDeltaHatAux _ _ [] = Set.empty
doDeltaHatAux q c (x:xs) = case x of
		    (q1, d, q2) | and [q1 == q, Just d == Just c] -> Set.unions [(Set.singleton q2), (doDeltaHatAux q c xs)]
		    _ -> doDeltaHatAux q c xs