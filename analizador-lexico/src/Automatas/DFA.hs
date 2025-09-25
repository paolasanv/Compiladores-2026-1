{-|
Module      : Automatas.DFA
Description : Autómatas finitos deterministas.

Este módulo implementa el algoritmo de conversión de un 
autómata finito no determinista (AFN) a un autómata finitos determinista (AFD).
-}
module Automatas.DFA where

import Automatas.NFA ( NFA )

data DFA = DFA -- Definir el tipo de dato para AFD

toDFA :: NFA -> DFA
toDFA = undefined
