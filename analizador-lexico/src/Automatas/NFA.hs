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

data NFA = NFA {states :: Set State,
		alphabet :: Set Char,
		transitions :: Set DeltaNoE,
		start :: State,
		final :: [State]}
		deriving (Show)
		
stateEclosure :: State -> Set Delta -> Set State
stateEclosure q d = stateEclosureAux q (Set.toList d)

stateEclosureAux :: State -> [Delta] -> Set State
stateEclosureAux _ [] = Set.empty
stateEclosureAux q (x:xs) = case x of
                         (q1, Nothing, q2) | q1 == q -> Set.unions [Set.singleton q2, (stateEclosureAux q xs), (stateEclosureAux q2 xs)]
                         _ -> stateEclosureAux q xs

doDeltaPrima :: State -> Symbol -> Set Delta -> Set State
doDeltaPrima q c d = doDeltaPrimaAux q c (Set.toList d)

doDeltaPrimaAux :: State -> Symbol -> [Delta] -> Set State
doDeltaPrimaAux _ _ [] = Set.empty
doDeltaPrimaAux q c (x:xs) = case x of
		    (q1, d, q2) | and [q1 == q, Just d == Just c] -> Set.unions [(Set.singleton q2), (doDeltaPrimaAux q c xs)]
		    _ -> doDeltaPrimaAux q c xs
		  
deltaPrima :: State -> Symbol -> Set Delta -> Set State
deltaPrima q c d = Set.unions [doDeltaPrima x c d | x <- Set.toList (stateEclosure q d)]

toNFA :: NFAE -> NFA
toNFA = undefined
