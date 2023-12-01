-- This file contains the data structures and respective functions to aid development of this project

module DataStructures where

import qualified Data.Map.Strict as HashMap
import Data.List (intercalate, sort)
import Data.Map (toList)

-- This defines the Instructions data type and the type Code, composed of Instructions 
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


-- This defines the Stack data type and functions to operate on Stacks
data StackValue = IntValue Integer | TT | FF deriving Show
type Stack = [StackValue]

stackValueToString :: StackValue -> String
stackValueToString (IntValue i) = show i
stackValueToString TT = "True"
stackValueToString FF = "False"

push :: StackValue -> Stack -> Stack
push x stack = x : stack

pop :: Stack -> (Maybe StackValue, Stack)
pop []     = (Nothing, [])
pop (x:xs) = (Just x, xs)

createEmptyStack :: Stack
createEmptyStack = []

-- This defines the State data type (Storage) and functions to operate on it
type Key = String
type Value = StackValue

type State = HashMap.Map Key Value

pairToString :: (String, StackValue) -> String
pairToString (key, IntValue i) = key ++ "=" ++ show i
pairToString (key, TT) = key ++ "=True"
pairToString (key, FF) = key ++ "=False"

createEmptyState :: State
createEmptyState = HashMap.empty

insertIntoState :: Key -> Value -> State -> State
insertIntoState = HashMap.insert

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackValueToString (reverse stack))
