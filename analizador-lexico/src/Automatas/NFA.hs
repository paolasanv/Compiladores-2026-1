{-|
Module      : Automatas.NFA
Description : Autómatas finitos no deterministas.

Este módulo implementa el algoritmo de conversión de un 
autómata finito no determinista con transitions épsilon (AFN-ε)
a un autómata finitos no determinista (AFN).
-}
module Automatas.NFA where

import Automatas.NFA_E ( NFAE )

data NFA = NFA -- Definir el tipo de dato para AFN

toNFA :: NFAE -> NFA
toNFA = undefined
