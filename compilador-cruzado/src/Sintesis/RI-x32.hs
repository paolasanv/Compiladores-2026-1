{-|
Module      : Sintesis.RI-x32
Description : Representación intermedia dependiente de una arquitectura x32 (por el momento, puede variar)

Este módulo implementa una representación intermedia dependiente de la máquina. 
-}
module Sintesis.RIx32 where
import Sintesis.RI(representacionI)


data Instx32 = undefined 

{-
codigoObjeto :: ([InsTresDir], Operando) -> [Instx32]
codigoObjeto ([], op) = -- el operando (número o variable)
codigoObjeto (ins:insR, _) = -- traducir la instrucción ins y continuar con insR
-}