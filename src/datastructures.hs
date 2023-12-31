module Datastructures where

import qualified Data.Map.Strict as HashMap
import Data.List (intercalate, sort)
import Data.Map (toList)

--------- INTERPRETER --------------

-- This defines the Instructions data type and the type Code, composed of Instructions 
data Inst =
  Push Integer 
  | Add 
  | Mult 
  | Sub 
  | Tru 
  | Fals 
  | Equ
  | Le 
  | And 
  | Neg 
  | Fetch String 
  | Store String 
  | Noop 
  | Branch Code Code 
  | Loop Code Code
  deriving Show

type Code = [Inst]

-- This defines the Stack data type and functions to operate on Stacks
data StackValue = IntValue Integer | TT | FF deriving (Show, Eq)
type Stack = [StackValue]

stackValueToString :: StackValue -> String
stackValueToString (IntValue i) = show i
stackValueToString TT = "True"
stackValueToString FF = "False"

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackValueToString stack)

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

lookupState :: String -> State -> Maybe Value
lookupState = HashMap.lookup

state2Str :: State -> String
state2Str state = intercalate "," . sort $ map pairToString (toList state)


--------- COMPILER --------------

-- Arithmetic expressions
data Aexp = 
    Num Integer             -- a number
    | Var String            -- a variable
    | AddExp Aexp Aexp      -- addition
    | SubExp Aexp Aexp      -- subtraction
    | MultExp Aexp Aexp     -- multiplication
    deriving Show

-- Boolean expressions
data Bexp = 
    Tr                      -- true
    | Fls                   -- false
    | Not Bexp              -- negation
    | AndExp Bexp Bexp      -- logical and
    | LeExp Aexp Aexp       -- less than or equal to
    | EquExp Bexp Bexp      -- boolean equality
    | DoubleEqu Aexp Aexp   -- arithmetic equality
    deriving Show

-- Statements
data Stm =
    Assign String Aexp              -- assignment
    | If Bexp Program Program       -- if statement
    | While Bexp Program            -- while loop
    deriving Show

type Program = [Stm]

type Parser a = [String] -> (a)
