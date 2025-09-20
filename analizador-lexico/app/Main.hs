module Main (main) where

import Regex.ReadFile

main :: IO ()
main = file2RegEx "regex.txt"
