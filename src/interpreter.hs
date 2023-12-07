module Interpreter where

import Datastructures
import Data.Map.Strict as HashMap
import System.Exit (exitSuccess, exitFailure)

--- Receives the stack and returns the updated stack
add :: Stack -> Stack
add stack = let (val1, stackAfterPop1) = pop stack
            in case val1 of
                Nothing -> stack
                Just (IntValue i1) ->
                    let (val2, stackAfterPop2) = pop stackAfterPop1
                    in case val2 of
                        Nothing -> stack
                        Just (IntValue i2) -> pushToStack (IntValue (i1 + i2)) stackAfterPop2
                        _ -> stack
                _ -> stack


-- Receives the stack and returns the updated stack
mult :: Stack -> Stack
mult stack = let (val1, stackAfterPop1) = pop stack
            in case val1 of
                Nothing -> stack
                Just (IntValue i1) ->
                    let (val2, stackAfterPop2) = pop stackAfterPop1
                    in case val2 of
                        Nothing -> stack
                        Just (IntValue i2) -> pushToStack (IntValue (i1 * i2)) stackAfterPop2
                        _ -> stack
                _ -> stack

-- Receives the stack and returns the updated stack
sub :: Stack -> Stack
sub stack = let (val1, stackAfterPop1) = pop stack
            in case val1 of
                Nothing -> stack
                Just (IntValue i1) ->
                    let (val2, stackAfterPop2) = pop stackAfterPop1
                    in case val2 of
                        Nothing -> stack
                        Just (IntValue i2) -> pushToStack (IntValue (i1 - i2)) stackAfterPop2
                        _ -> stack
                _ -> stack

-- Receives the stack and returns the updated stack
eq :: Stack -> Stack
eq stack = let (val1, stackAfterPop1) = pop stack
           in case val1 of
               Nothing -> stack
               Just (IntValue i1) ->
                   let (val2, stackAfterPop2) = pop stackAfterPop1
                   in case val2 of
                       Nothing -> stack
                       Just (IntValue i2) -> if i1 == i2 then pushToStack TT stackAfterPop2 else pushToStack FF stackAfterPop2
                       _ -> stack
               Just TT ->
                   let (val2, stackAfterPop2) = pop stackAfterPop1
                   in case val2 of
                       Nothing -> stack
                       Just TT -> pushToStack TT stackAfterPop2
                       Just FF -> pushToStack FF stackAfterPop2
                       _ -> stack
               Just FF ->
                   let (val2, stackAfterPop2) = pop stackAfterPop1
                   in case val2 of
                       Nothing -> stack
                       Just FF -> pushToStack TT stackAfterPop2
                       Just TT -> pushToStack FF stackAfterPop2
                       _ -> stack

-- Receives the stack and returns the updated stack
le :: Stack -> Stack
le stack = let (val1, stackAfterPop1) = pop stack
            in case val1 of
                Nothing -> stack
                Just (IntValue i1) ->
                    let (val2, stackAfterPop2) = pop stackAfterPop1
                    in case val2 of
                        Nothing -> stack
                        Just (IntValue i2) ->
                            let value = if i1 <= i2 then TT else FF
                            in pushToStack value stackAfterPop2
                        _ -> stack
                _ -> stack

-- Receives the stack and returns the updated stack
and :: Stack -> Stack
and stack = let (val1, stackAfterPop1) = pop stack
            in case val1 of
                Nothing -> stack
                Just TT ->
                    let (val2, stackAfterPop2) = pop stackAfterPop1
                    in case val2 of
                        Nothing -> stack
                        Just TT -> pushToStack TT stackAfterPop2
                        Just FF -> pushToStack FF stackAfterPop2
                Just FF ->
                    let (val2, stackAfterPop2) = pop stackAfterPop1
                    in case val2 of
                        Nothing -> stack
                        Just FF -> pushToStack FF stackAfterPop2
                        Just TT -> pushToStack FF stackAfterPop2
                _ -> stack

-- Receives the stack and returns the updated stack
neg :: Stack -> Stack
neg stack = let (val1, stackAfterPop1) = pop stack
            in case val1 of
                Nothing -> stack
                Just TT -> pushToStack FF stackAfterPop1
                Just FF -> pushToStack TT stackAfterPop1
                _ -> stack

-- Receives a StackValue and the stack and returns the updated stack
push :: Either Integer Bool -> Stack -> Stack
push n = pushToStack value
    where value = case n of
                    Right True -> TT
                    Right False -> FF
                    Left i -> IntValue i

-- Receives a Key, the stack and the state and returns the updated stack
fetch :: Key -> Stack -> State -> Stack
fetch key stack state = case HashMap.lookup key state of
                          Just value -> pushToStack value stack
                          Nothing -> stack

-- Receives a Key, the stack and the state and returns the updated stack and state
store :: Key -> Stack -> State -> (Stack, State)
store key stack state = let (value, stackAfterPop) = pop stack
                        in case value of
                            Nothing -> (stackAfterPop, state)
                            Just value -> (stackAfterPop, insertIntoState key value state)

-- Receives 2 code flows and the stack and returns one of the code flows and the updated stack
branch :: Code -> Code -> Stack -> (Code, Stack)
branch c1 c2 stack = let (value, stackAfterPop) = pop stack
                        in case value of
                            Nothing -> (c1,stackAfterPop)
                            Just value -> let code = if value == TT then c1 else c2
                                            in (code, stackAfterPop)

-- Receives 2 code flows, the stack and the state and returns the remaining code flow, the updated stack and updated state
loop :: Code -> Code -> Code
loop c1 c2 = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]

-- Dummy function that receives the stack and state and returns them both
noop :: Stack -> State -> (Stack, State)
noop stack state = (stack, state)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (instruction:remainingCode, stack, state) =
    case instruction of
        Push n -> run (remainingCode, push (Left n) stack, state)
        Add -> run (remainingCode, add stack, state)
        Mult -> run (remainingCode, mult stack, state)
        Sub -> run (remainingCode, sub stack, state)
        Tru -> run (remainingCode, push (Right True) stack, state)
        Fals -> run (remainingCode, push (Right False) stack, state)
        Equ -> run (remainingCode, eq stack, state)
        Le -> run (remainingCode, le stack, state)
        And -> run (remainingCode, Interpreter.and stack, state)
        Neg -> run (remainingCode, Interpreter.neg stack, state)
        Fetch key -> run (remainingCode, fetch key stack state, state)
        Store key -> let (newStack, newState) = store key stack state in run (remainingCode, newStack, newState)
        Noop -> let (newStack, newState) = noop stack state in run (remainingCode, newStack, newState)
        Branch code1 code2 -> let (remainingCode, newStack) = branch code1 code2 stack in run (remainingCode, newStack, state)
        Loop code1 code2 -> run (loop code1 code2, stack, state)