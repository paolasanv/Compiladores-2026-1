{-|
Module      : Automatas.DFA
Description : Autómatas finitos deterministas.
Author: Paola Mildred Martinez Hidalgo.

Este módulo implementa el algoritmo de conversión de un 
autómata finito no determinista (AFN) a un autómata finitos determinista (AFD).
-}
module Automatas.DFA where

import Automatas.NFA_E (State)
<<<<<<< Updated upstream
import Automatas.NFA (NFA)
import qualified Automatas.NFA as NFA
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')
=======
import Automatas.NFA (NFA(..))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
>>>>>>> Stashed changes

type DeltaDFA = (State, Char, State)
-- Debido a que los estados en el DFA son CONJUNTOS de estados 
-- del NFA y state lo tenemos definido como int, vamos a usar un nuevo tipo
type StateSet = Set State

data DFA = DFA {
    states      :: Set State,
    alphabet    :: Set Char,
    transitions :: Set DeltaDFA, -- δ: Q×Σ→Q
    start       :: State,
    final       :: [State]
} deriving (Show)

<<<<<<< Updated upstream
-- Pasa un NFA a un DFA usando el algoritmo de subconjutos
toDFA :: NFA -> DFA
toDFA  nfa = 
    let sigma = NFA.alphabetNFA nfa
        q0Set = Set.singleton (NFA.startNFA nfa)
        
        -- Construir todos los estados y transiciones usando BFS
        (stateMap, transSet) = buildDFA [q0Set] Map.empty Set.empty sigma nfa 0
        
        -- Estado inicial es el mismo que en el NFA
        q0 = 0
        
        -- Estados finales, en este caso serán todos aquellos que contengan al menos un estado final del NFA
        finalStates = [sid | (sset, sid) <- Map.toList stateMap, 
                            any (`elem` NFA.finalNFA nfa) (Set.toList sset)]
        
        -- Todos los estados generados
        allStates = Set.fromList $ Map.elems stateMap
        
    in DFA {
        states = allStates,
        alphabet = sigma,
        transitions = transSet,
        start = q0,
        final = finalStates
    }

-- Contruimos el DFA usando el algoritmo BFS
buildDFA :: [StateSet]           -- Cola de conjuntos por procesar
         -> Map StateSet State    -- Mapeo de conjuntos a IDs de estado, como tenemos más de un edo por eso usamos Map
         -> Set DeltaDFA          -- Transiciones acumuladas
         -> Set Char              -- Alfabeto
         -> NFA                   -- NFA original
         -> State                 -- Próximo ID de estado disponible
         -> (Map StateSet State, Set DeltaDFA)
buildDFA [] stateMap trans _ _ _ = (stateMap, trans)
buildDFA (currentSet:queue) stateMap trans sigma nfa nextId =
    case Map.lookup currentSet stateMap of
        Just _ -> -- Ya procesamos este conjunto
            buildDFA queue stateMap trans sigma nfa nextId
        Nothing -> 
            let -- Asignar ID al conjunto actual
                currentId = nextId
                stateMap' = Map.insert currentSet currentId stateMap
                
                -- Para cada símbolo, calcular transiciones y nuevos conjuntos
                (newQueue, newTrans, stateMap'', nextId') = 
                    foldl' (processSymbol currentSet currentId nfa) 
                           (queue, trans, stateMap', nextId + 1) 
                           (Set.toList sigma)
                
            in buildDFA newQueue stateMap'' newTrans sigma nfa nextId'

-- Procesa un símbolo y actualiza el mapeo de estados y transiciones
processSymbol :: StateSet 
              -> State 
              -> NFA 
              -> ([StateSet], Set DeltaDFA, Map StateSet State, State)
              -> Char
              -> ([StateSet], Set DeltaDFA, Map StateSet State, State)
processSymbol currentSet currentId nfa (queue, trans, stateMap, nextId) symbol =
    let destSet = computeDestination currentSet symbol nfa
    in if Set.null destSet
       then (queue, trans, stateMap, nextId)
       else case Map.lookup destSet stateMap of
                Just destId -> 
                    -- Ya existe este conjunto, solo agregar transición
                    (queue, Set.insert (currentId, symbol, destId) trans, stateMap, nextId)
                Nothing ->
                    -- Nuevo conjunto, asignar ID, agregar a cola y crear transición
                    let destId = nextId
                        stateMap' = Map.insert destSet destId stateMap
                        trans' = Set.insert (currentId, symbol, destId) trans
                        queue' = queue ++ [destSet]
                    in (queue', trans', stateMap', nextId + 1)

-- Calcula el conjunto de estados alcanzables desde un conjunto dado con un símbolo
computeDestination :: StateSet -> Char -> NFA -> StateSet
computeDestination currentSet symbol nfa =
    let nfaTrans = NFA.transitionsNFA nfa
        -- Para cada estado en el conjunto actual, buscar transiciones con el símbolo
        reachable = [dest | state <- Set.toList currentSet,
                           (src, sym, dests) <- Set.toList nfaTrans,
                           src == state,
                           sym == symbol,
                           dest <- dests]
    in Set.fromList reachable



=======
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
>>>>>>> Stashed changes
