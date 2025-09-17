module Main (main) where

import Regex.Interp

main :: IO ()
main = file2RegEx "regex.txt"
