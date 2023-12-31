module Compiler where

import Datastructures
import Auxiliary

-- Compiles an arithmetic expression into a list of instructions
compA :: Aexp -> Code
compA (Num i) = [Push i]
compA (Var var) = [Fetch var]
compA (AddExp a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (SubExp a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (MultExp a1 a2) = compA a2 ++ compA a1 ++ [Mult]

-- Compiles a boolean expression into a list of instructions
compB :: Bexp -> Code
compB Tr = [Tru]
compB Fls = [Fals]
compB (Not bexp) = compB bexp ++ [Neg]
compB (AndExp b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (LeExp a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (EquExp a1 a2) = compB a2 ++ compB a1 ++ [Equ]
compB (DoubleEqu a1 a2) = compA a2 ++ compA a1 ++ [Equ]

-- Main function for compiling a program
compile :: Program -> Code
compile program = concatMap compileStm program
    where
        compileStm :: Stm -> Code
        compileStm (Assign var aexp) = compA aexp ++ [Store var]
        compileStm (If bexp stm1 stm2) = compB bexp ++ [Branch (compile stm1) (compile stm2)]
        compileStm (While bexp program) = [Loop (compB bexp) (compile program)]

-- Parses an arithmetic expression
parseAexp :: Parser Aexp
parseAexp tokens = case nextValidAToken (tokens,[]) "-" of
    (firstSegment, "-":secondSegment) -> SubExp (parseAexp firstSegment) (parseAexp secondSegment)
    _ -> case nextValidAToken (tokens,[]) "+" of
        (firstSegment, "+":secondSegment) -> AddExp (parseAexp firstSegment) (parseAexp secondSegment)
        _ -> case nextValidAToken (tokens,[]) "*" of
            (firstSegment, "*":secondSegment) -> MultExp (parseAexp firstSegment) (parseAexp secondSegment)
            _ -> case break isAllNumbers (reverse tokens) of
                (_, number:firstSegment) | check (reverse firstSegment) -> Num (read number)
                _ -> case break isAllLetters (reverse tokens) of
                    (_, name:firstSegment) | check (reverse firstSegment) -> Var name
                    _->case break (== "(") (reverse tokens) of
                        (middleAfter, "(":_) -> case break (==")") (reverse middleAfter) of
                            (middle, ")":_) -> parseAexp middle

-- Parses a boolean expression
parseBexp :: Parser Bexp
parseBexp tokens = case nextValidToken (tokens,[]) "and" of
    (firstSegment, "and":secondSegment) -> AndExp  (parseBexp firstSegment) (parseBexp secondSegment)
    _ -> case nextValidToken (tokens,[]) "=" of
        (firstSegment, "=":secondSegment) -> EquExp (parseBexp firstSegment) (parseBexp secondSegment)
        _ -> case nextValidToken (tokens,[]) "not" of
            (_,"not":secondSegment) -> Not (parseBexp secondSegment)
            _ -> case nextValidToken (tokens,[]) "==" of
                (firstSegment, "==":secondSegment) -> DoubleEqu (parseAexp firstSegment) (parseAexp secondSegment)
                _ -> case nextValidToken (tokens,[]) "<=" of
                    (firstSegment, "<=":secondSegment) -> LeExp (parseAexp firstSegment) (parseAexp secondSegment)
                    _ -> case break (== "True") (reverse tokens) of
                        (_, "True":firstSegment) | check (reverse firstSegment) -> Tr
                        _ -> case break (== "False") (reverse tokens) of
                            (_, "False":firstSegment) | check (reverse firstSegment) -> Fls
                            _->case break (== "(") (reverse tokens) of
                                (middleAfter, "(":_) -> case break (==")") (reverse middleAfter) of
                                    (middle, ")":_) -> parseBexp middle

-- Parses an if statement
parseIf :: Parser Program
parseIf ("if":tokens) = case break (=="then") tokens of
    (ifStm , rest) -> case breakOnElse ([],rest) 0 of
        ("then":"(":thenStm', "else":"(":elseStm') ->
            let ")":thenStm = reverse thenStm'
                (elseStm,")":";":next) = breakOnParenthesis ([],elseStm') 0
            in If (parseBexp ifStm) (parseProgram (reverse thenStm)) (parseProgram elseStm):parseProgram next
        ("then":"(":thenStm', "else":elseStm) ->
            let ")":thenStm = reverse thenStm'
            in [If (parseBexp ifStm) (parseProgram (reverse thenStm)) [parseAssign elseStm]]
        ("then":thenStm, "else":"(":elseStm') ->
            let (elseStm,")":";":next) = breakOnParenthesis ([],elseStm') 0
            in (If (parseBexp ifStm) [parseAssign thenStm] (parseProgram elseStm):parseProgram next)
        ("then":thenStm, "else":elseStm) -> case break (==";") elseStm of
            (elseStm, ";":next) -> If (parseBexp ifStm) [parseAssign thenStm] [parseAssign elseStm]:parseProgram next

-- Parses a while statement
parseWhile :: Parser Program
parseWhile ("while":tokens) = case break (== "do") tokens of
    (whileStm, "do":"(":after) ->
        let (doStm,")":";":next) = breakOnParenthesis ([],after) 0
        in (While (parseBexp whileStm) (parseProgram doStm):parseProgram next)
    (whileStm, "do":doStm) -> [While (parseBexp whileStm) [parseAssign doStm]]

-- Parses an assignment statement
parseAssign :: Parser Stm
parseAssign (token:":=":tokens) = Assign token (parseAexp tokens)

-- Main helper function for parsing a program
parseProgram :: Parser Program
parseProgram [] = []
parseProgram (firstToken:tokens)
    | firstToken == "if" = parseIf (firstToken:tokens)
    | firstToken == "while" = parseWhile (firstToken:tokens)
    | otherwise = case break (== ";") (firstToken:tokens) of
        (firstStm, ";":second) -> parseAssign firstStm: parseProgram second

-- Parses a string with code into a program
parse :: String -> Program
parse programCode = parseProgram (lexer programCode)

-- Lexes a string into a list of tokens
lexer :: String -> [String]
lexer [] = []
lexer (':':'=':cs) = ":=" : lexer cs
lexer ('<':'=':cs) = "<=" : lexer cs
lexer ('=':'=':cs) = "==" : lexer cs
lexer ('>':'=':cs) = ">=" : lexer cs
lexer (c:cs)
        | c `elem` " +-*;()=" = if c == ' ' then lexer cs else [c] : lexer cs
        | otherwise = let (word, rest) = span (`notElem` " +-*;()=") (c:cs)
                                    in word : lexer rest
