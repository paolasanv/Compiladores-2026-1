{-|
Module      : AFNe.AFNe
Description : Pasa de regex a AFNe.

Este módulo implementa el algoritmo para connvertir de expresión regular a 
AFN con transiciones epsilon.
-}
module Regex.AFNe where

import Regex.Lexer
import Regex.Parser
import Data.List

type Estado = Int
type Simbolo = Maybe Char
type Delta = (Estado, Simbolo, [Estado])

data AFNe = AFNe {estados :: [Estado], alfabeto :: [Char], transiciones :: [Delta], inicial :: Estado, final :: Estado} deriving (Show)

regex2AFNe :: RegEx ->[Estado] ->AFNe
regex2AFNe Empty l = AFNe {estados = [q0, q1], alfabeto = [], transiciones = [], inicial = q0, final = q1}
		     where q0 = length l
                           q1 = (length l) + 1
regex2AFNe Epsilon l = AFNe {estados = [q0, q1], alfabeto = [' '], transiciones = [(q0, Just ' ', [q1])], inicial = q0, final = q1}
                      where q0 = length l
                            q1 = (length l) + 1 
regex2AFNe (Character c) l = AFNe {estados = nub(l ++ [q0, q1]), alfabeto = [c], transiciones = [(q0, Just c, [q1])], inicial = q0, final = q1}  
			    where q0 = length l
				  q1 = (length l) + 1
regex2AFNe (Union a b) l =  AFNe {estados = nub (estados mB ++ [q0, q1]), alfabeto = nub (alfabeto mA ++ alfabeto mB), transiciones = (transiciones mA ++ transiciones mB ++ [(q0, Nothing, [inicial mA]), (q0, Nothing, [inicial mB]), (final mA, Nothing, [q1]), (final mB, Nothing, [q1])]), inicial = q0, final = q1}  
                            where mA = regex2AFNe a (l ++ [q0,q1])
				  mB = regex2AFNe b estadosN
				  estadosN = nub(l ++ estados mA)
				  q0 = length l
                                  q1 = (length l) + 1
regex2AFNe (Concat a b) l =  AFNe {estados = nub (estados mB ++ estados mA), alfabeto = nub (alfabeto mA ++ alfabeto mB), transiciones = (transiciones mA ++ transiciones mB ++ [(final mA, Nothing, [inicial mB])]), inicial = inicial mA, final = final mB}
                            where mA = regex2AFNe a l
                                  mB = regex2AFNe b estadosN
				  estadosN = nub(l ++ estados mA)
regex2AFNe (Kleene a) l =  AFNe {estados = nub (estados mA ++ [q0, q1]), alfabeto = nub (alfabeto mA), transiciones = (transiciones mA ++ [(q0, Nothing, [inicial mA]), (final mA, Nothing, [q1]), (q0, Nothing, [q1]), (q1, Nothing, [inicial mA])]), inicial = q0, final = q1}
                            where mA = regex2AFNe a (l ++ [q0, q1])
                                  q0 = length l
				  q1 = (length l) + 1

