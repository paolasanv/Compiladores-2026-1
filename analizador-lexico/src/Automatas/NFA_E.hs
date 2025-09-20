{-|
Module      : Automatas.NFA_E
Description : Autómatas finitos no deterministas con transiciones épsilon.

Este módulo implementa el algoritmo de Thompson para convertir una expresión regular
en un autómata finitos no determinista con transiciones épsilon (AFN-ε).
-}
module Automatas.NFA_E where

import Regex.Parser ( RegEx )


data NFAE = NFAE -- Definir el tipo de dato para AFN-ε

toNFAE :: RegEx -> NFAE 
toNFAE = undefined
