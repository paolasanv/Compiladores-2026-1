module Main where

import Test.Hspec
import qualified MDDspec

main :: IO ()
main = hspec $ do
  MDDspec.spec