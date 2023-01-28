module Main where

import qualified System.Environment
import qualified Test.DocTest

main :: IO ()
main = do
    args <- System.Environment.getArgs
    Test.DocTest.doctest
        $ "./Main.hs"
        : "./lib/"
        : args
