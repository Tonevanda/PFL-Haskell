module Tests where

import Datastructures
import Interpreter

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)


--testParser :: String -> (String, String)
--testParser programCode = (stack2Str stack, state2Str state)
--  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Parser tests
--testParser "x := 5; x := x - 1;" == ("","x=4")
--testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
--testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
--testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
--testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
--testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
--testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")