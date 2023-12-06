import DataStructures
import Instructions
import Data.List (intercalate, sort)
import Data.Map (toList)
import qualified Data.Map.Strict as HashMap
import Data.Bool (Bool(False))
import Data.ByteString.Char8 (putStrLn)

--import PFLTests

-- main will just run all the tests, the function that processes the Code, Stack and State is the run function
main :: IO ()
main = do
    (newCode, newStack, newState) <- run (code, stack, state)
    Prelude.putStrLn "Done!"
    Prelude.putStrLn $ "Code: " ++ show newCode
    Prelude.putStrLn $ "Stack: " ++ stack2Str newStack
    Prelude.putStrLn $ "State: " ++ state2Str newState
    where
        code = [Loop [Tru] [Noop]]
        stack = createEmptyStack
        state = createEmptyState



-- Part 1 of the assignment

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackValueToString stack)

state2Str :: State -> String
state2Str state = intercalate "," . sort $ map pairToString (toList state)



run :: (Code, Stack, State) -> IO (Code, Stack, State)
run ([], stack, state) = return ([], stack, state)
run (instruction:remainingCode, stack, state) = do
    Prelude.putStrLn $ "Current instruction: " ++ show instruction
    Prelude.putStrLn $ "Remaining code: " ++ show (instruction:remainingCode)
    case instruction of
        Push n -> run (remainingCode, push (Left n) stack, state)
        Add -> run (remainingCode, add stack, state)
        Mult -> run (remainingCode, mult stack, state)
        Sub -> run (remainingCode, sub stack, state)
        Tru -> run (remainingCode, push (Right True) stack, state)
        Fals -> run (remainingCode, push (Right False) stack, state)
        Equ -> run (remainingCode, eq stack, state)
        Le -> run (remainingCode, le stack, state)
        --And -> run (remainingCode, and stack, state)
        --Neg -> run (remainingCode, neg stack, state)
        Fetch key -> run (remainingCode, fetch key stack state, state)
        Store key -> let (newStack, newState) = store key stack state in run (remainingCode, newStack, newState)
        Noop -> let (newStack, newState) = noop stack state in run (remainingCode, newStack, newState)
        Branch code1 code2 -> let (remainingCode, newStack) = branch code1 code2 stack in run (remainingCode, newStack, state)
        Loop code1 code2 -> run (loop code1 code2, stack, state)

-- Part 2
-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
--compA = undefined -- TODO

-- compB :: Bexp -> Code
--compB = undefined -- TODO

-- compile :: Program -> Code
--compile = undefined -- TODO

-- parse :: String -> Program
--parse = undefined -- TODO
