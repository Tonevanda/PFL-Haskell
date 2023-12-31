module Tests where

import Datastructures
import Interpreter
import Compiler

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

runAllAssemblerTests :: IO ()
runAllAssemblerTests = do
  putStrLn "Assembler tests:"
  putStr "1: " >> print (testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10",""))
  putStr "2: " >> print (testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True"))
  putStr "3: " >> print (testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False"))
  putStr "4: " >> print (testAssembler [Push (-20),Tru,Fals] == ("False,True,-20",""))
  putStr "5: " >> print (testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20",""))
  putStr "6: " >> print (testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20",""))
  putStr "7: " >> print (testAssembler [Push (-20),Push (-21), Le] == ("True",""))
  putStr "8: " >> print (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4"))
  putStr "9: " >> print (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1"))
  
  --These should throw run-time errors:
  --putStr "10: " >> print (testAssembler [Push 1,Push 2,And])
  --putStr "11: " >> print (testAssembler [Tru,Tru,Store "y", Fetch "x",Tru])

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


runAllParserTests :: IO ()
runAllParserTests = do
  putStrLn ""
  print "Parser tests:"
  putStr "1: " >> print (testParser "x := 5; x := x - 1;" == ("","x=4"))
  putStr "2: " >> print (testParser "x := 0 - 2;" == ("","x=-2"))
  putStr "3: " >> print (testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2"))
  putStr "4: " >> print (testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1"))
  putStr "5: " >> print (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2"))
  putStr "6: " >> print (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4"))
  putStr "7: " >> print (testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68"))
  putStr "8: " >> print (testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34"))
  putStr "9: " >> print (testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1"))
  putStr "10: " >> print (testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2"))
  putStr "11: " >> print (testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6"))
  putStr "12: " >> print (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1"))
  putStr "13: " >> print (testParser "x:= 3-1;" == ("","x=2"))
  putStr "14: " >> print (testParser "x:=3-1-1; y:=x+1;" == ("","x=1,y=2"))
  putStr "15: " >> print (testParser "x:=3-1*3+(2-2*1+1-1);" == ("","x=0"))
  putStr "16: " >> print (testParser "x_x:=3-1*3+(2-2*1+1-1);" == ("","x_x=0"))
  putStr "17: " >> print (testParser "if (True) then () else ();" == ("",""))
  putStr "18: " >> print (testParser "x := 1; y := 2; z := y+x;" == ("","x=1,y=2,z=3"))
  putStr "19: " >> print (testParser "a := 1071; b := 462; while (not(a == 0)) do (temp := a; while (a <= b) do (b := b - a;); a := b; b := temp;); gcd := b;" == ("","a=0,b=21,gcd=21,temp=21"))
  putStr "20: " >> print (testParser "x := 5; if x <= 5 then y := 1; else (if x <= 10 then y := 2; else (if x <= 15 then y := 3; else y := 4;););" == ("","x=5,y=1"))
  putStr "21: " >> print (testParser "x := 10; if x <= 5 then y := 1; else (if x <= 10 then y := 2; else (if x <= 15 then y := 3; else y := 4;););" == ("","x=10,y=2"))
  putStr "22: " >> print (testParser "x := 15; if x <= 5 then y := 1; else (if x <= 10 then y := 2; else (if x <= 15 then y := 3; else y := 4;););" == ("","x=15,y=3"))
  putStr "23: " >> print (testParser "x := 20; if x <= 5 then y := 1; else (if x <= 10 then y := 2; else (if x <= 15 then y := 3; else y := 4;););" == ("","x=20,y=4"))
  putStr "24: " >> print (testParser "x := 5; if x <= 5 then y := 1; else (if x <= 10 then y := 2; else (if x <= 15 then y := 3; else y := 4;);); z := y;" == ("","x=5,y=1,z=1"))
  putStr "25: " >> print (testParser "x := 10; if x <= 5 then y := 1; else (if x <= 10 then y := 2; else (if x <= 15 then y := 3; else y := 4;);); z := y;" == ("","x=10,y=2,z=2"))
  putStr "26: " >> print (testParser "x := 15; if x <= 5 then y := 1; else (if x <= 10 then y := 2; else (if x <= 15 then y := 3; else y := 4;);); z := y;" == ("","x=15,y=3,z=3"))
  putStr "27: " >> print (testParser "x := 20; if x <= 5 then y := 1; else (if x <= 10 then y := 2; else (if x <= 15 then y := 3; else y := 4;);); z := y;" == ("","x=20,y=4,z=4"))
  putStr "28: " >> print (testParser "x := 5; if x <= 5 then y := 1; else (if x <= 10 then y := 2; else (if x <= 15 then y := 3; else y := 4;);); z := y; if z <= 1 then w := 1; else (if z <= 2 then w := 2; else (if z <= 3 then w := 3; else w := 4;););" == ("","w=1,x=5,y=1,z=1"))
  putStr "29: " >> print (testParser "x := 10; if x <= 5 then y := 1; else (if x <= 10 then y := 2; else (if x <= 15 then y := 3; else y := 4;);); z := y; if z <= 1 then w := 1; else (if z <= 2 then w := 2; else (if z <= 3 then w := 3; else w := 4;););" == ("","w=2,x=10,y=2,z=2"))
  putStr "30: " >> print (testParser "x := 15; if x <= 5 then y := 1; else (if x <= 10 then y := 2; else (if x <= 15 then y := 3; else y := 4;);); z := y; if z <= 1 then w := 1; else (if z <= 2 then w := 2; else (if z <= 3 then w := 3; else w := 4;););" == ("","w=3,x=15,y=3,z=3"))
