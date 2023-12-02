module Instructions where 

import DataStructures


--- Receives the stack and returns the updated stack
add :: Stack -> Stack
add stack = let (val1, stackAfterPop1) = pop stack
            in case val1 of
                Nothing -> stack
                Just (IntValue i1) ->
                    let (val2, stackAfterPop2) = pop stackAfterPop1
                    in case val2 of
                        Nothing -> stack
                        Just (IntValue i2) -> push (IntValue (i1 + i2)) stackAfterPop2
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
                        Just (IntValue i2) -> push (IntValue (i1 * i2)) stackAfterPop2
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
                        Just (IntValue i2) -> push (IntValue (i1 - i2)) stackAfterPop2
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
                        Just (IntValue i2) -> 
                            let value = if i1 == i2 then TT else FF
                            in push value stackAfterPop2
                        _ -> stack
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
                            in push value stackAfterPop2
                        _ -> stack
                _ -> stack

-- Receives a StackValue and the stack and returns the updated stack
--push :: StackValue -> Stack -> Stack

-- Receives a Key, the stack and the state and returns the updated stack
--fetch :: Key -> Stack -> State -> Stack

-- Receives a Key, the stack and the state and returns the updated stack and state
--store :: Key -> Stack -> State -> Stack -> State

-- Receives 2 code flows and the stack and returns one of the code flows and the updated stack
--branch :: Code -> Code -> Stack -> Code -> Stack

-- Receives 2 code flows, the stack and the state and returns the remaining code flow, the updated stack and updated state
--loop :: Code -> Code -> Stack -> State -> Code -> Stack -> State

-- ??????
--noop :: 