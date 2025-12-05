{-|
Module      : Sintesis.RIx32
Description : Representación intermedia dependiente de una arquitectura x86 de 32 bits

Este módulo implementa una representación intermedia dependiente de la máquina. 
El módulo mantiene el nombre de x32 para hacer referencia al tamaño de palabra de 32 bits.
Se simula código dependiente para arquitectura x86 de 32 bits utilizando el lenguaje ensamblador AT&T de 32 bits

-}
module Sintesis.RIx32 where
import Sintesis.RI(InsTresDir(InsCopiado, InsUnaria, InsBinaria), Direccion(Var, Cons, Temporal))

-- Se declara una lista de letras minúsculas de la 'a' a la 'z' para poder nombrar los distintos registros e%x
intermedioL :: [Char]
intermedioL = ['a'..'z']

-- Nodos hoja correspondientes a los valores básicos de la representación en la arquitectura. 
data Oper = Registro String
          | Var1 String
          | Cons1 Int
instance Show Oper where
    show (Registro s) = "%"++s
    show (Var1 s) = s
    show (Cons1 i) = "$"++show i

-- Representación en formato de Árbol de las traducciones de las operaciones en la arquitectura
data Instx32 = Move Oper Oper
         | Add Oper Oper
         | Sub Oper Oper
         | Mult Oper Oper
instance Show Instx32 where
    show (Move a b) = "movl "++show a++" "++show b ++";"
    show (Add a b) = "addl "++show a++" "++show b ++";"
    show (Sub a b) = "subl "++show a++" "++show b ++";"
    show (Mult a b) = "imull "++show a++" "++show b ++";"

-- Función Auxiliar que recibe una lista de representaciones en 3 direcciones y el número de registros usados. Devuelve una lista de operaciones para la arquitectura
codigoObjetoPrima :: [InsTresDir] -> Int -> [Instx32]
codigoObjetoPrima [] _ = []
codigoObjetoPrima (ins:insR) i = (fst ins32) ++ codigoObjetoPrima insR (snd ins32)
        where ins32 = traduce ins i
     
-- Función externa para realizar el proceso de traducción desde otras clases. Empieza el conteo de registros usados en 0
codigoObjeto32 :: [InsTresDir] -> [Instx32]
codigoObjeto32 l = codigoObjetoPrima l 0

-- Función para obtener el registro a usar dado los anteriores registros usados.
-- Como este programa es solo una simulación de como funciona la generación de código objeto suponemos que un solo programa no usara más de 26 registros.
-- Así, no se aplica ninguna estrategia para saber si un registro esta siendo usado o no. Ni tampoco para verificar si tiene información almacenada. 
obtenerR :: Int -> String 
obtenerR i = "e"++[intermedioL !! n]++"x"
        where n = mod (i) (length intermedioL)

-- Transforma una direccion de la representación de 3 direcciones a un Oper para la arquitectura.
-- Las variables y constantes se quedan exactamente igual. Pero se supone que toda variable temporal usada será un registro en la memoria.
operador32 :: Direccion -> Oper
operador32 (Var s) = Var1 s
operador32 (Cons i) = Cons1 i
operador32 (Temporal i) = Registro (obtenerR i)

-- Traducción formal por casos
traduce :: InsTresDir -> Int -> ([Instx32], Int)
-- Para operaciones de asignación si a es una variable temporal se almacenara el valor de b directamente en un registro.
-- Por el contrario si a es una variable se almacena el valor de b en un registro y luego se vacía este lo contenido en este registro dentro de a variable a.
traduce (InsCopiado a b) i = case opA of
			     (Registro _) -> ([Move opB opA], i+1)
			     (Var1 _) -> ([Move opB (Registro re), Move (Registro re) opA], i+1)
			    where opA = operador32 a 
			          opB = operador32 b 
			          re = obtenerR i
-- Para operaciones unarias, la única que se admite en este lenguaje es la asignación negativa. Para ello si a es una variable temporal se cargara al registro 
-- correspondiente a a el valor de 0 y luego se restara b de ese registro.
-- Por el contrario si a es una variable se cargara un 0 al registro correspondiente a b, se le restara el valor de v y luego se almacenara el contenido de 
-- este registro en la variable a.
traduce (InsUnaria a op b) i = case op of
			       '-' -> case opA of
			       	      (Registro _) -> ([Move (Cons1 0) opA, Sub opB opA], i+1)
			     	      (Var1 _) -> ([Move (Cons1 0) (Registro re), Sub opB (Registro re), Move (Registro re) opA], i+1)
			       where opA = operador32 a
			             opB = operador32 b
			             re = obtenerR i
-- Para las operaciones binarias se admite la suma (+), resta (-) y multiplicación (*). Asi, si a es una variable temporal se carga el valor de b a su registro correspondiente
-- y luego se le aplica la operación con c.
-- Por el contrario, si a es una variable, se almacena b en el registro correspondiente, se le aplica la operación con c y se vacia el contenido del registro en la variable a.
traduce (InsBinaria a op b c) i = case op of
			          '+' -> case opA of
			       	         (Registro _) -> ([Move opB opA, Add opC opA], i+1)
			     	         (Var1 _) -> ([Move opB (Registro re), Add opC (Registro re), Move (Registro re) opA], i+1)
			          '-' -> case opA of
			       	         (Registro _) -> ([Move opB opA, Sub opC opA], i+1)
			     	         (Var1 _) -> ([Move opB (Registro re), Sub opC (Registro re), Move (Registro re) opA], i+1)
			          '*' -> case opA of
			       	         (Registro _) -> ([Move opB opA, Mult opC opA], i+1)
			     	         (Var1 _) -> ([Move opB (Registro re), Mult opC (Registro re), Move (Registro re) opA], i+1)
			          where opA = operador32 a 
			                opB = operador32 b 
			                opC = operador32 c 
			                re = obtenerR i
