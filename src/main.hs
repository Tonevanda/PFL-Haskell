module Main where

import Datastructures
import Interpreter
import Compiler
import Tests


-- main will just run all the tests, the function that processes the Code, Stack and State is the run function
main :: IO ()
main = do
    --runAllAssemblerTests
    runAllParserTests