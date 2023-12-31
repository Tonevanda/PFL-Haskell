module Main where

import Datastructures
import Interpreter
import Compiler
import Tests


main :: IO ()
main = do
    runAllAssemblerTests
    runAllParserTests
