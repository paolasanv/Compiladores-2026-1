{-|
Module      : Sintesis.RI-arm64
Description : Representación intermedia dependiente de una arquitectura arm64

Este módulo implementa una representación intermedia dependiente de la máquina. 
El código ensamblador usado esta basado en el ARM64.
-}
module Sintesis.RIarm64 where
import Sintesis.RI(representacionI, InsTresDir(InsCopiado, InsUnaria, InsBinaria), Operando(Var, Cons, Temporal))

-- Lista de números para nombrar los registros x0-x30
maxRegistros :: Int
maxRegistros = 31

-- Nodos hoja correspondientes a los valores básicos de la representación en ARM64
data Oper = Registro String
          | Var1 String
          | Cons1 Int
instance Show Oper where
    show (Registro s) = s
    show (Var1 s) = s
    show (Cons1 i) = "#"++show i

-- Representación en formato de Árbol de las traducciones de las operaciones en ARM64
data Instarm64 = Move Oper Oper
               | Add Oper Oper Oper
               | Sub Oper Oper Oper
               | Mult Oper Oper Oper
               | Neg Oper Oper
               | LoadVar Oper String    -- Cargar variable desde memoria
               | StoreVar String Oper   -- Almacenar variable en memoria

instance Show Instarm64 where
    show (Move a b) = " mov " ++ show a ++ ", " ++ show b ++ "\n"
    show (Add dest op1 op2) = " add " ++ show dest ++ ", " ++ show op1 ++ ", " ++ show op2 ++ "\n"
    show (Sub dest op1 op2) = " sub " ++ show dest ++ ", " ++ show op1 ++ ", " ++ show op2 ++ "\n"
    show (Mult dest op1 op2) = " mul " ++ show dest ++ ", " ++ show op1 ++ ", " ++ show op2 ++ "\n"
    show (Neg dest op) = " neg " ++ show dest ++ ", " ++ show op ++ "\n"
    show (LoadVar reg var) = " ldr " ++ show reg ++ ", =" ++ var ++ "\n"
    show (StoreVar var reg) = " str " ++ show reg ++ ", =" ++ var ++ "\n"

-- Función Auxiliar que recibe una lista de representaciones en 3 direcciones y el número de registros usados
-- devuelve una lista de operaciones para ARM64
codigoObjetoPrima :: [InsTresDir] -> Int -> [Instarm64]
codigoObjetoPrima [] _ = []
codigoObjetoPrima (ins:insR) i = (fst ins64) ++ codigoObjetoPrima insR (snd ins64)
    where ins64 = traduce ins i

-- Función externa para realizar el proceso de traducción, empieza el conteo de registros usados en 0
codigoObjeto :: [InsTresDir] -> [Instarm64]
codigoObjeto l = codigoObjetoPrima l 0

-- Función para obtener el registro a usar dado los anteriores registros usados
-- ARM64 tiene registros x0-x30
obtenerR :: Int -> String 
obtenerR i = "x" ++ show n
    where n = mod i maxRegistros

-- Transforma un Operando de la representación de 3 direcciones a un Oper para ARM64
-- Las variables y constantes se adaptan al formato ARM64 y las variables temporales se mapean a registros
operador64 :: Operando -> Oper
operador64 (Var s) = Var1 s
operador64 (Cons i) = Cons1 i
operador64 (Temporal i) = Registro (obtenerR i)

-- Traducción formal por casos para ARM64
traduce :: InsTresDir -> Int -> ([Instarm64], Int)

-- Si a es un registro temporal, movemos b directamente
-- Si a es una variable, movemos a un registro auxiliar y luego almacenamos
traduce (InsCopiado a b) i = case opA of
    (Registro s) -> case opB of
        (Cons1 val) -> ([Move opA opB], i+1)
        (Var1 var) -> ([LoadVar (Registro re) var, Move opA (Registro re)], i+1)
        (Registro _) -> ([Move opA opB], i+1)
    (Var1 s) -> case opB of
        (Cons1 val) -> ([Move (Registro re) opB, StoreVar s (Registro re)], i+1)
        (Var1 var) -> ([LoadVar (Registro re) var, StoreVar s (Registro re)], i+1)
        (Registro _) -> ([StoreVar s opB], i+1)
    where 
        opA = operador64 a 
        opB = operador64 b 
        re = obtenerR i

-- Operaciones unarias (negación), usamos la instrucción NEG
traduce (InsUnaria a op b) i = case op of
    '-' -> case opA of
        (Registro s) -> case opB of
            (Cons1 val) -> ([Move (Registro re) opB, Neg opA (Registro re)], i+1)
            (Var1 var) -> ([LoadVar (Registro re) var, Neg opA (Registro re)], i+1)
            (Registro _) -> ([Neg opA opB], i+1)
        (Var1 s) -> case opB of
            (Cons1 val) -> ([Move (Registro re) opB, Neg (Registro re) (Registro re), StoreVar s (Registro re)], i+1)
            (Var1 var) -> ([LoadVar (Registro re) var, Neg (Registro re) (Registro re), StoreVar s (Registro re)], i+1)
            (Registro _) -> ([Neg (Registro re) opB, StoreVar s (Registro re)], i+1)
    where 
        opA = operador64 a
        opB = operador64 b
        re = obtenerR i

-- Para las operaciones binarias aceptamos suma (+), resta(-), multiplicación(*)
-- En ARM64 usa formato de 3 operandos: INST destino, op1, op2
traduce (InsBinaria a op b c) i = 
    let inst = case op of
            '+' -> Add
            '-' -> Sub
            '*' -> Mult
            _   -> error ("Operador binario no reconocido: " ++ [op])
    in generarOpBinaria inst a b c i
    where
        -- Función auxiliar para generar operaciones binarias
        generarOpBinaria :: (Oper -> Oper -> Oper -> Instarm64) -> Operando -> Operando -> Operando -> Int -> ([Instarm64], Int)
        generarOpBinaria inst a b c i =
            let opA = operador64 a
                opB = operador64 b
                opC = operador64 c
                re = obtenerR i
                re2 = obtenerR (i+1)
                
                -- En ARM64 no todos los operadores pueden ser constantes o variables directamente,
                -- en ocasiones se tienen que cargar primero en registros.

                -- Prepara el operador B y decide qué hacer. Si es una constante necesita cargarse en un resgistro primero.
                -- Si es una variable necesita  cargarse desde la memoria.
                -- Si es un registro se puede usar directamente.
                (insB, regB) = case opB of
                    (Cons1 _) -> ([Move (Registro re) opB], Registro re)
                    (Var1 var) -> ([LoadVar (Registro re) var], Registro re)
                    (Registro _) -> ([], opB)

                -- Prepara el operador C y hace lo mismo que el operador B, pero usa un registro diferente para no 
                -- sobreescribir el valor. 
                (insC, regC) = case opC of
                    (Cons1 _) -> ([Move (Registro re2) opC], Registro re2)
                    (Var1 var) -> ([LoadVar (Registro re2) var], Registro re2)
                    (Registro _) -> ([], opC)
            
            -- En esta parte se genera la operación final.
            --si a es un registro temporal concatena intrucción de b + intrucción de c + la operación
            -- si a es una variable hace la operación en un registro auxiliar y luego guarda en memoria
            in case opA of
                (Registro s) -> 
                    (insB ++ insC ++ [inst opA regB regC], i+2)
                (Var1 s) -> 
                    (insB ++ insC ++ [inst (Registro re) regB regC, StoreVar s (Registro re)], i+2)
                _ -> error "Operando destino no válido en operación binaria"