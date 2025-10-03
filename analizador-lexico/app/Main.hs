module Main where

import qualified Data.Set as Set
import Automatas.DFA       
import Automatas.DFA_min   

-- Definir el DFA según la imagen
exampleDFA :: DFA
exampleDFA = DFA
  { states = Set.fromList [0,1,2,3,4,5]
  , alphabet = Set.fromList ['a','b']
  , transitions = Set.fromList
      [ (0,'a',1)
      , (0,'b',2)
      , (1,'a',3)
      , (1,'b',4)
      , (2,'a',4)
      , (2,'b',3)
      , (3,'a',5)
      , (3,'b',5)
      , (4,'a',5)
      , (4,'b',5)
      , (5,'a',5)
      , (5,'b',5)
      ]
  , start = 0
  , final = [1,2,5]
  }

-- Test: minimizar el DFA
main :: IO ()
main = do
    putStrLn "DFA original:"
    print exampleDFA

    let minDfa = minDFA exampleDFA

    putStrLn "\nDFA minimizado:"
    print minDfa
