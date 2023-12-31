module Compiler where

import Datastructures
import Data.Char (isDigit, isAlpha)
import Text.Parsec (tokens, token)
import Control.Arrow (Arrow(first), ArrowChoice (right))

compA :: Aexp -> Code
compA (Num i) = [Push i]
compA (Var var) = [Fetch var]
compA (AddExp a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (SubExp a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (MultExp a1 a2) = compA a2 ++ compA a1 ++ [Mult]

compB :: Bexp -> Code
compB Tr = [Tru]
compB Fls = [Fals]
compB (Not bexp) = compB bexp ++ [Neg]
compB (AndExp b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (LeExp a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (EquExp a1 a2) = compB a1 ++ compB a2 ++ [Equ]
compB (DoubleEqu a1 a2) = compA a1 ++ compA a2 ++ [Equ]

compile :: Program -> Code
compile program = concatMap compileStm program
    where
        compileStm :: Stm -> Code
        compileStm (Assign var aexp) = compA aexp ++ [Store var]
        compileStm (If bexp stm1 stm2) = compB bexp ++ [Branch (compile stm1) (compile stm2)]
        compileStm (While bexp program) = [Loop (compB bexp) (compile program)]



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


isAllLetters :: String -> Bool
isAllLetters = all isAlpha

isAllNumbers :: String -> Bool
isAllNumbers = all isDigit

type Parser a = [String] -> (a)

parseProgram :: Parser Program
parseProgram [] = []
parseProgram (firstToken:tokens)
    | firstToken == "if" = parseIf (firstToken:tokens)
    | firstToken == "while" = parseWhile (firstToken:tokens)
    | otherwise = case break (== ";") (firstToken:tokens) of
        (firstStm, ";":second) -> parseAssign firstStm: parseProgram second


parseWhile :: Parser Program
parseWhile ("while":tokens) = case break (== "do") tokens of
    (whileStm, "do":"(":after) ->
        let (doStm,")":";":next) = breakOnParenthesis ([],after) 0
        in (While (parseBexp whileStm) (parseProgram doStm):parseProgram next)
    (whileStm, "do":doStm) -> [While (parseBexp whileStm) [parseAssign doStm]]



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
                                    _ -> Fls
                                _->Fls

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
                            _ -> Num 0
                        _-> Num 0


breakOnElse :: ([String], [String]) -> Int -> ([String], [String])
breakOnElse (_, []) _ = ([], [])
breakOnElse (left, x:right) count
    | x == "if" = breakOnElse (left ++ [x], right) (count+1)
    | x == "else" && (count /= 0) = breakOnElse (left ++ [x], right) (count-1)
    | x == "else" = (left, x:right)
    | otherwise = breakOnElse (left ++ [x], right) count

breakOnParenthesis :: ([String], [String]) -> Int -> ([String], [String])
breakOnParenthesis (left, []) _ = (left, [])
breakOnParenthesis (left, x:right) count
    | x == "(" = breakOnParenthesis (left ++ [x], right) (count+1)
    | x == ")" && (count /= 0) = breakOnParenthesis (left ++ [x], right) (count-1)
    | x == ")" = (left, x:right)
    | otherwise = breakOnParenthesis (left ++ [x], right) count


nextValidToken :: ([String],[String]) -> String -> ([String],[String])
nextValidToken ([], x:right) token
    | x==token = ([], x:right)
nextValidToken ([], right) _ = ([], right)
nextValidToken ("(":left, []) token = nextValidToken (init left , [last left]) token
nextValidToken (left, []) token = nextValidToken (init left , [last left]) token
nextValidToken (left, x:right) token
    | x == token && check left = (left , x:right)
    | otherwise = nextValidToken (init left , last left:x:right) token

nextValidAToken :: ([String],[String]) -> String -> ([String],[String])
nextValidAToken ([], x:right) token
    | x==token = ([], x:right)
nextValidAToken ([], right) _ = ([], right)
nextValidAToken (left, []) token = nextValidAToken (init left , [last left]) token
nextValidAToken (left, x:right) token
    | x == token && check left = (left , x:right)
    | otherwise = nextValidAToken (init left , last left:x:right) token

check :: [String] -> Bool
check [] = True
check (x:xs)
    | x == "(" = check' xs 1
    | otherwise = check xs

check' :: [String] -> Int -> Bool
check' [] count = count <= 0
check' (x:xs) count
    | x == "(" = check' xs (count + 1)
    | x == ")" = check' xs (count - 1)
    | otherwise = check' xs count

parseAssign :: Parser Stm
parseAssign (token:":=":tokens) = Assign token (parseAexp tokens)

parse :: String -> Program
parse programCode = parseProgram (lexer programCode)
