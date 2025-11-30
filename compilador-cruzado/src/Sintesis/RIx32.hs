{-|
Module      : Sintesis.RI-x32
Description : Representación intermedia dependiente de una arquitectura x32 (por el momento, puede variar)

Este módulo implementa una representación intermedia dependiente de la máquina. 
El código ensamblador usado es el AT&T 32 bits
-}
module Sintesis.RIx32 where
import Sintesis.RI(InsTresDir(InsCopiado, InsUnaria, InsBinaria), Direccion(Var, Cons, Temporal))

intermedioL :: [Char]
intermedioL = ['a'..'z']

data Oper = Registro String
          | Var1 String
          | Cons1 Int
instance Show Oper where
    show (Registro s) = "%"++s
    show (Var1 s) = s
    show (Cons1 i) = "$"++show i

data Instx32 = Move Oper Oper
	     | Add Oper Oper
	     | Sub Oper Oper
	     | Mult Oper Oper
instance Show Instx32 where
    show (Move a b) = "movl "++show a++" "++show b ++";"
    show (Add a b) = "addl "++show a++" "++show b ++";"
    show (Sub a b) = "subl "++show a++" "++show b ++";"
    show (Mult a b) = "imull "++show a++" "++show b ++";"

codigoObjetoPrima :: [InsTresDir] -> Int -> [Instx32]
codigoObjetoPrima [] _ = []
codigoObjetoPrima (ins:insR) i = (fst ins32) ++ codigoObjetoPrima insR (snd ins32)
			     where ins32 = traduce ins i

codigoObjeto :: [InsTresDir] -> [Instx32]
codigoObjeto l = codigoObjetoPrima l 0


obtenerR :: Int -> String 
obtenerR i = "e"++[intermedioL !! n]++"x"
	   where n = mod (i) (length intermedioL)

operador32 :: Direccion -> Oper
operador32 (Var s) = Var1 s
operador32 (Cons i) = Cons1 i
operador32 (Temporal i) = Registro (obtenerR i)

traduce :: InsTresDir -> Int -> ([Instx32], Int)
traduce (InsCopiado a b) i = case opA of
			     (Registro s) -> ([Move opB opA], i+1)
			     (Var1 s) -> ([Move opB (Registro re), Move (Registro re) opA], i+1)
			    where opA = operador32 a 
			          opB = operador32 b 
			          re = obtenerR i
traduce (InsUnaria a op b) i = case op of
			       '+' -> case opA of
			       	      (Registro s) -> ([Add opB opA], i+1)
			     	      (Var1 s) -> ([Move opA (Registro re), Add opB (Registro re), Move (Registro re) opA], i+1)
			       '-' -> case opA of
			       	      (Registro s) -> ([Sub opB opA], i+1)
			     	      (Var1 s) -> ([Move opA (Registro re), Sub opB (Registro re), Move (Registro re) opA], i+1)
			       '*' -> case opA of
			       	      (Registro s) -> ([Mult opB opA], i+1)
			     	      (Var1 s) -> ([Move opA (Registro re), Mult opB (Registro re), Move (Registro re) opA], i+1)
			       where opA = operador32 a
			             opB = operador32 b
			             re = obtenerR i
traduce (InsBinaria a op b c) i = case op of
			          '+' -> case opA of
			       	         (Registro s) -> ([Move opB opA, Add opC opA], i+1)
			     	         (Var1 s) -> ([Move opB (Registro re), Add opC (Registro re), Move (Registro re) opA], i+1)
			          '-' -> case opA of
			       	         (Registro s) -> ([Move opB opA, Sub opC opA], i+1)
			     	         (Var1 s) -> ([Move opB (Registro re), Sub opC (Registro re), Move (Registro re) opA], i+1)
			          '*' -> case opA of
			       	         (Registro s) -> ([Move opB opA, Mult opC opA], i+1)
			     	         (Var1 s) -> ([Move opB (Registro re), Mult opC (Registro re), Move (Registro re) opA], i+1)
			          where opA = operador32 a 
			                opB = operador32 b 
			                opC = operador32 c 
			                re = obtenerR i
