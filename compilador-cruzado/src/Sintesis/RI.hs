{-|
Module      : Sintesis.RI
Description : Representación intermedia utilizando código de tres direcciones

Este módulo implementa una representación intermedia independiente de la máquina. 
Utiliza código de tres direcciones. Posibles instrucciones:

    a = b       >> Copiado 
    a = op b    >> Operacion unaria
    a = b op c  >> Operacion binaria

Donde:
    > a, b, c son operandos (variables, constantes, temporales)
    > op es un operador aritmetico (+, -, *)
-}
module Sintesis.RI where
import Analisis.Parser(AS(..))

data Operando = Var String    -- Identificadores del codigo fuente
    | Cons Int                -- Numeros del codigo fuente
    | Temporal Int            -- Variable temporal generado por el compilador (como t0, t1, ..) 
instance Show Operando where
    show (Var s) = s
    show (Cons i) = show i
    show (Temporal n) = "t" ++ show n

data InsTresDir = InsCopiado Operando Operando   -- a = b
    | InsUnaria Operando Char Operando           -- a = op b
    | InsBinaria Operando Char Operando Operando -- a = b op c

instance Show InsTresDir where
    show (InsCopiado a b) = show a ++ " = " ++ show b
    show (InsUnaria a op b) = show a ++ " = " ++ show op ++ " " ++ show b
    show (InsBinaria a op b c) = show a ++ " = " ++ show b ++ " " ++ show op ++ " " ++ show c

-- Generación de instrucciones con contador de temporales
generaIns :: AS -> Int -> ([InsTresDir], Operando, Int)
generaIns (Num i) n = ([], Cons i, n)
generaIns (Ident s) n = ([], Var s, n)
generaIns (Suma i d) n =
    let 
        (insA, opA, n1) = generaIns i n  -- Instrucciones del lado izquierdo
        (insB, opB, n2) = generaIns d n1 -- Instrucciones del lado derecho 
        t = Temporal n2                  -- Nuevo temporal 
        instr = InsBinaria t '+' opA opB
    in
        (insA ++ insB ++ [instr], t, n2+1)
generaIns (Resta i d) n =
    let 
        (insA, opA, n1) = generaIns i n
        (insB, opB, n2) = generaIns d n1
        t = Temporal n2 
        instr = InsBinaria t '-' opA opB
    in
        (insA ++ insB ++ [instr], t, n2+1)
generaIns (Mult i d) n =
    let 
        (insA, opA, n1) = generaIns i n
        (insB, opB, n2) = generaIns d n1
        t = Temporal n2 
        instr = InsBinaria t '*' opA opB
    in
        (insA ++ insB ++ [instr], t, n2+1)
generaIns (Asigna i e) n=
    let 
        (insE, opE, n1) = generaIns e n -- Instrucciones de la expresion
        instr = InsCopiado (Var i) opE  -- Instruccion de copiado
    in 
        (insE ++ [instr], Var i, n1)
                        
representacionI :: AS -> ([InsTresDir], Operando)
representacionI as = (a,b)
    where 
        (a,b,_) = generaIns as 0