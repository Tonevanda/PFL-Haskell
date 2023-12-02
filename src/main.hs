module Main where

import PFLTests

-- main will just run all the tests, the function that processes the Code, Stack and State is the run function
main :: IO ()
main = do
  -- Run the assembler tests
  putStrLn "Running Assembler Tests:\n"
  putStrLn $ "Test 1: " ++ show (testAssembler [Push 10, Push 4, Push 3, Sub, Mult] == ("-10", ""))
  putStrLn $ "Test 2: " ++ show (testAssembler [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"] == ("", "a=3,someVar=False,var=True"))
  -- Add more tests as needed

  -- Run the parser tests
  putStrLn "\nRunning Parser Tests:\n"
  putStrLn $ "Test 1: " ++ show (testParser "x := 5; x := x - 1;" == ("", "x=4"))
  putStrLn $ "Test 2: " ++ show (testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("", "y=2"))
  -- Add more tests as needed



-- Part 1 of the assignment

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackValueToString (reverse stack))

state2Str :: State -> String
state2Str state = intercalate "," . sort $ map pairToString (toList state)

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined -- TODO

-- Part 2
-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO