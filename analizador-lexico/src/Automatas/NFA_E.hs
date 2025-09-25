{-|
Module      : Automatas.NFA_E
Description : Pasa de expresión regular a autómata finito no determinista con transitions épsilon.
Author: Nancy del Valle

Este módulo implementa el algoritmo de Thompson para convertir una expresión regular
en un autómata finitos no determinista con transitions épsilon (AFN-ε).
-}
module Automatas.NFA_E where

import Regex.Parser (RegEx(..))
import qualified Data.Set as Set
import Data.Set (Set)

type State = Int
type Symbol = Maybe Char
type Delta = (State, Symbol, State)

data NFAE = NFAE {
    states      :: Set State,
    alphabet    :: Set Char,
    transitions :: Set Delta,
    start       :: State,
    final       :: State
} deriving (Show)


toNFAE :: RegEx -> NFAE 
toNFAE regex = thompson regex []

thompson :: RegEx -> [State] -> NFAE
thompson Empty l = NFAE {
    states = Set.fromList [q0, q1],
    alphabet = Set.empty,
    transitions = Set.empty,
    start = q0,
    final = q1}
  where 
    q0 = length l
    q1 = q0 + 1

thompson Epsilon l = NFAE {
    states = Set.fromList [q0, q1],
    alphabet = Set.singleton ' ',
    transitions = Set.singleton (q0, Just ' ', q1),
    start = q0,
    final = q1}
  where 
    q0 = length l
    q1 = q0 + 1

thompson (Character c) l = NFAE {
    states = Set.fromList [q0, q1],
    alphabet = Set.singleton c,
    transitions = Set.singleton (q0, Just c, q1),
    start = q0,
    final = q1}
  where 
    q0 = length l
    q1 = q0 + 1

thompson (Union a b) l = NFAE {
    states = Set.unions [Set.fromList [q0, q1], states mA, states mB],
    alphabet = Set.union (alphabet mA) (alphabet mB),
    transitions = Set.unions [transitions mA,transitions mB,
        Set.fromList [(q0, Nothing, start mA),
            (q0, Nothing, start mB),
            (final mA, Nothing, q1),
            (final mB, Nothing, q1)]],
    start = q0,
    final = q1}
  where 
    q0 = length l
    q1 = q0 + 1
    mA = thompson a (l ++ [q0, q1])
    mB = thompson b (l ++ [q0, q1] ++ Set.toList (states mA))

thompson (Concat a b) l = NFAE {
    states = Set.union (states mA) (states mB),
    alphabet = Set.union (alphabet mA) (alphabet mB),
    transitions = Set.unions [transitions mA,transitions mB, Set.singleton (final mA, Nothing, start mB)],
    start = start mA,
    final = final mB}
  where 
    mA = thompson a l
    mB = thompson b (l ++ Set.toList (states mA))

thompson (Kleene a) l = NFAE {
    states = Set.unions [states mA, Set.fromList [q0, q1]],
    alphabet = alphabet mA,
    transitions = Set.unions [transitions mA,
            Set.fromList [(q0, Nothing, q1),
            (q0, Nothing, start mA),
            (final mA, Nothing, q1),
            (final mA, Nothing, start mA)]],
    start = q0,
    final = q1}
  where 
    q0 = length l
    q1 = q0 + 1
    mA = thompson a (l ++ [q0, q1])
