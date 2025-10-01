module Main (main) where

import Regex.ReadFile
import Automatas.NFA_E(toNFAE)  
import Automatas.NFA(toNFA)

main :: IO ()
main =  file2RegEx "regex.txt"
