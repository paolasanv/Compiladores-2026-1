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
<<<<<<< Updated upstream

min :: DFA -> DFA
min dfa = 
    let dfa' = deleletInaccessible dfa 
    in undefined --aqui va el resto del algoritmo
=======
import Data.List (partition)
import Data.Maybe (fromMaybe)


minimize :: DFA -> DFA
minimize dfa = 
    let dfa'      = deleteInaccessible dfa 
        -- Clases de estados equivalentes
        classes   = Set.toList (equivalenceClasses dfa')
        -- Mapear estados a su clase
        repState c     = Set.findMin c
        repStateMap    = \st -> head [repState c | c <- classes, Set.member st c]
        -- Nuevos estados
        newStates = Set.fromList (map repState classes)
        -- Estado inicial
        newStart  = repStateMap (start dfa')
        -- Estados finales
        newFinals = [repState c | c <- classes, any (`elem` final dfa') (Set.toList c)]
        -- Transiciones
        newTrans  = Set.fromList
            [ (repState c, a, repStateMap q)
            | c <- classes
            , p <- Set.toList c
            , a <- Set.toList (alphabet dfa')
            , let q = fromMaybe p (deltaHat dfa' p a)
            ]
    in DFA {
        states = newStates,
        alphabet = alphabet dfa',
        transitions = newTrans,
        start = newStart,
        final = newFinals
    }
>>>>>>> Stashed changes


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


<<<<<<< Updated upstream
-- Obtener los estados equivalentes
equivalenceClasses :: DFA -> [State]
equivalenceClasses = undefined
=======
-- Obtiene las clases de estados equivalentes
equivalenceClasses :: DFA -> Set (Set State)
equivalenceClasses dfa = 
    let sts   = Set.toList (states dfa)
        eqPairs = equivalentStates dfa
        -- Creamos un map de clases
        classes = foldr mergePair [[st] | st <- sts] eqPairs
    in Set.fromList (map Set.fromList classes)

        
-- Une dos clases si los estados estan en una misma pareja
mergePair :: (State, State) -> [[State]] -> [[State]]
mergePair (x, y) classes =
    let (wX, rest1) = partition (elem x) classes
        (wY, rest2) = partition (elem y) rest1
        newClass = concat (wX ++ wY)
    in newClass : rest2


-- Devuelve una lista de pares de estados equivalentes
equivalentStates :: DFA -> [(State, State)]
equivalentStates dfa = 
    let nonEqualStates = nonEquivalentStates dfa
        allPairs = statesPairsTable (Set.toList (states dfa))
    -- Filtramos posibles estados equivalentes
    in filter (`notElem` nonEqualStates) allPairs


-- Devuelve una lista de pares de estados no equivalentes
nonEquivalentStates :: DFA -> [(State, State)]
nonEquivalentStates dfa = 
    let sts = Set.toList (states dfa)
        -- Calculamos primeros estados no equivalentes
        nonEqualStates = step1 dfa sts []
    -- Eliminaamos demas estados no equivalentes
    in step2 dfa nonEqualStates


-- Encuentra pares triviales no equivalentes
step1 :: DFA -> [State] -> [(State, State)] -> [(State, State)]
step1 _ [] acc = acc
step1 dfa (st1:rest) acc =
    step1 dfa rest (step1Aux dfa st1 rest acc)


-- Devuelve una lista con pares de estados formados por el de entrada, tales que uno es final y otro no
step1Aux :: DFA -> State -> [State] -> [(State, State)] -> [(State, State)]
step1Aux _ _ [] acc = acc
step1Aux dfa st1 (st2:rest) acc
    | oneFinal st1 st2 = (orderPair st1 st2) : step1Aux dfa st1 rest acc
    | otherwise        = step1Aux dfa st1 rest acc
  where
    oneFinal x y =
        (x `elem` final dfa && not (y `elem` final dfa))
        || (y `elem` final dfa && not (x `elem` final dfa))

    orderPair x y = if x < y then (x, y) else (y, x)


-- Agrega a la lista todas las parejas de estados tales que, con la misma entrada, ambos transitan a un par
-- de estados dentro la lista
step2 :: DFA -> [(State, State)] -> [(State, State)]
step2 dfa marked =
    let allPairs = statesPairsTable (Set.toList (states dfa))
        newMarked = foldl (checkStatesPairs dfa) marked allPairs
    in if length newMarked == length marked
        then marked
        else step2 dfa newMarked


-- Devuelve una lista con pares de estados agregados 
checkStatesPairs :: DFA -> [(State, State)] -> (State, State) -> [(State, State)]
checkStatesPairs dfa marked (st1, st2)
    | (st1, st2) `elem` marked || (st2, st1) `elem` marked = marked 
    | any (\a ->
        let q1 = fromMaybe st1 (deltaHat dfa st1 a)
            q2 = fromMaybe st2 (deltaHat dfa st2 a)
        in areMarked q1 q2 marked)
        (Set.toList (alphabet dfa)) =
            (st1, st2) : marked
    | otherwise = marked


-- Revisa si un par de estados esta en la lista dada
areMarked :: State -> State -> [(State, State)] -> Bool
areMarked x y marked = (x, y) `elem` marked || (y, x) `elem` marked


-- Devuelve todas las parejas de estados posibles
statesPairsTable :: [State] -> [(State, State)]
statesPairsTable [] = []
statesPairsTable (x:xs) = [(x, y) | y <- xs] ++ statesPairsTable xs

>>>>>>> Stashed changes




