{-|
Module      : Automatas.DFA
Description : Autómatas finitos deterministas minimos.

Este módulo implementa el algoritmo de minimización de un 
autómata finito determinista (AFD).
-}
module Automatas.DFA_min where

import Automatas.DFA ( DFA )

min :: DFA -> DFA
min = undefined

-- Eliminar estados inalcanzables
inaccessibleStates :: DFA -> DFA
inaccessibleStates = undefined

-- Calcular las clases de equivalencia (estados equivalentes)
equivalenceClasses :: DFA -> [[Int]]
equivalenceClasses = undefined