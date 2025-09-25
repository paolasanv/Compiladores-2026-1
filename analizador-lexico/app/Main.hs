module Main (main) where

import Regex.ReadFile
import Automatas.NFA_E(toNFAE)  

main :: IO ()
main =  file2RegEx "regex.txt"
