module Interpreter where

import Datastructures
import Data.Map.Strict as HashMap

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

run (Push n:remainingCode, stack, state) = run (remainingCode, IntValue n:stack, state)

run (Add:remainingCode, IntValue i1:IntValue i2:stack, state) = run (remainingCode, IntValue (i1 + i2):stack, state)
run (Add:remainingCode, _:_:stack, state) = error "Runtime error: Invalid operation"

run (Mult:remainingCode, IntValue i1:IntValue i2:stack, state) = run (remainingCode, IntValue (i1 * i2):stack, state)
run (Mult:remainingCode, _:_:stack, state) = error "Runtime error: Invalid operation"

run (Sub:remainingCode, IntValue i1:IntValue i2:stack, state) = run (remainingCode, IntValue (i1 - i2):stack, state)
run (Sub:remainingCode, _:_:stack, state) = error "Runtime error: Invalid operation"

run (Tru:remainingCode, stack, state) = run (remainingCode, TT:stack, state)

run (Fals:remainingCode, stack, state) = run (remainingCode, FF:stack, state)

run (Equ:remainingCode, value1:value2:stack, state) = run (remainingCode, value:stack, state)
    where value = if value1 == value2 then TT else FF

run (Le:remainingCode, IntValue i1:IntValue i2:stack, state) = run (remainingCode, value:stack, state)
    where value = if i1 <= i2 then TT else FF
run (Le:remainingCode, _:_:stack, state) = error "Runtime error: Invalid operation"

run (And:remainingCode, TT:TT:stack, state) = run (remainingCode, TT:stack, state)
run (And:remainingCode, TT:FF:stack, state) = run (remainingCode, FF:stack, state)
run (And:remainingCode, FF:TT:stack, state) = run (remainingCode, FF:stack, state)
run (And:remainingCode, FF:FF:stack, state) = run (remainingCode, FF:stack, state)
run (And:remainingCode, IntValue i:stack, state) = error "Runtime error: Invalid operation"

run (Neg:remainingCode, TT:stack, state) = run (remainingCode, FF:stack, state)
run (Neg:remainingCode, FF:stack, state) = run (remainingCode, TT:stack, state)
run (Neg:remainingCode, IntValue i:stack, state) = error "Runtime error: Invalid operation"

run (Fetch key:remainingCode, stack, state) =
    run (remainingCode, newStack, state) 
    where newStack = case HashMap.lookup key state of
                          Just value -> value:stack
                          Nothing -> error "Runtime error: Invalid operation"

run (Store key:remainingCode, stacktop:stack, state) = run (remainingCode, stack, insertIntoState key stacktop state)

run (Branch code1 code2:remainingCode, TT:remainingStack, state) = run (code1, remainingStack, state)
run (Branch code1 code2:remainingCode, FF:remainingStack, state) = run (code2, remainingStack, state)
run (Branch code1 code2:remainingCode, IntValue i:remainingStack, state) = error "Runtime error: Invalid operation"

run (Loop code1 code2:remainingCode, stack, state) = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]], stack, state)

run (Noop:remainingCode, stack, state) = run (remainingCode, stack, state)
