module Compiler where

import Datastructures
import Data.Char (isDigit)
import Text.Parsec (tokens)

compA :: Aexp -> Code
compA (Num i) = [Push i]
compA (Var var) = [Fetch var]
compA (AddExp a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubExp a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (MultExp a1 a2) = compA a1 ++ compA a2 ++ [Mult]

compB :: Bexp -> Code
compB Tr = [Tru]
compB Fls = [Fals]
compB (Not bexp) = compB bexp ++ [Neg]
compB (AndExp b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (LeExp a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (EquExp a1 a2) = compB a1 ++ compB a2 ++ [Equ]
compB (DoubleEqu a1 a2) = compA a1 ++ compA a2 ++ [Equ]

compile :: Program -> Code
compile program = concatMap compileStm program
    where
        compileStm :: Stm -> Code
        compileStm (Assign var aexp) = compA aexp ++ [Store var]
        compileStm (If bexp stm1 stm2) = compB bexp ++ [Branch (compileStm stm1) (compileStm stm2)]
        compileStm (While bexp stm) = [Loop (compB bexp) (compileStm stm)]
        compileStm NoopStm = []


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


type Parser a = [String] -> (a, [String])

{-
parseAexp :: Parser Aexp
parseAexp tokens = case parseTerm tokens of
    Just (term, "-":tokens') -> case parseAexp tokens' of
        Just (aexp, tokens'') -> Just (SubExp term aexp, tokens'')
        _ -> Just (term, tokens')
    Just (term, tokens') -> Just (term, tokens')
    _ -> Nothing
-}


check :: [String] -> Bool
check [] = True
check (x:xs)
    | x == "(" = check' xs 1
    | otherwise = check xs

check' :: [String] -> Int -> Bool
check' [] count = count < 0
check' (x:xs) count
    | x == "(" = check' xs (count + 1)
    | x == ")" = check' xs (count - 1)
    | otherwise = check' xs count

parseAexp :: Parser Aexp
parseAexp tokens = undefined

parseBexp :: Parser Bexp
parseBexp tokens = case break (== "and") (reverse tokens) of
    (secondSegment, "and":firstSegment) | check (reverse firstSegment) -> (AndExp  (fst (parseBexp (reverse firstSegment))) (fst(parseBexp (reverse secondSegment))),[])
    _ -> case break (== "=") (reverse tokens) of 
        (secondSegment, "=":firstSegment) | check (reverse firstSegment) -> (EquExp (fst (parseBexp (reverse firstSegment))) (fst (parseBexp (reverse secondSegment))),[])
        _ -> case break (== "not") (reverse tokens) of
            (secondSegment, "not":firstSegment) | check (reverse firstSegment) -> (Not (fst (parseBexp (reverse secondSegment))),[])
            _ -> case break (== "==") (reverse tokens) of
                (secondSegment, "==":firstSegment) | check (reverse firstSegment) -> (DoubleEqu (fst (parseAexp (reverse firstSegment))) (fst (parseAexp (reverse secondSegment))),[])
                _ -> case break (== "<=") (reverse tokens) of
                    (secondSegment, "<=":firstSegment) | check (reverse firstSegment) -> (LeExp (fst (parseAexp (reverse firstSegment))) (fst (parseAexp (reverse secondSegment))),[])
                    _ -> case break (== "True") (reverse tokens) of
                        (secondSegment, "True":firstSegment) | check (reverse firstSegment) -> (Tr,[])
                        _ -> case break (== "False") (reverse tokens) of
                            (secondSegment, "False":firstSegment) | check (reverse firstSegment) -> (Fls,[])
                            _->case break (== "(") (reverse tokens) of
                                (middleAfter, "(":_) -> case break(==")") (reverse middleAfter) of
                                    (middle, ")":_) -> parseBexp middle
                                    _ -> (Fls,[])
                                _->(Fls,[])
                            
{-
parseIf :: Parser Program
parseIf ("if":tokens) = case break (=="then") tokens of
    (ifStm , rest) -> case parseBexp ifStm of
        Just (bexp, _) -> case parseProgram rest of
            
        _ -> Nothing

    _-> Nothing
    
    
    
    parseBexp tokens of
    Just (bexp, "then":tokens') -> case parseProgram tokens' of
        Just (program1, "else":tokens'') -> case parseProgram tokens'' of
            Just (program2, tokens''') -> Just ([If bexp program1 program2], tokens''')
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
-}
{-
parseProgram :: Parser Program
parseProgram [] = Just ([], [])
parseProgram firstToken:tokens
    | firstToken == "if" = case parseIf firstToken:tokens
    | firstToken == "while" = case parsewhile firstToken:tokens
    | otherwise = case break (== ";") firstToken:tokens of
        (firstStm, secondStm) -> case parseProgram secondStm of
            Just (program, tokens') -> Just (firstStm:program, tokens')
            _ -> Just ([firstStm], tokens')
        _ -> Nothing

-}
{-

parseTerm :: Parser Aexp
parseTerm (token:tokens)
    | all isDigit token = Just (Num (read token), tokens)
    | otherwise = Just (Var token, tokens)


parseStm :: Parser Stm
parseStm (token:":=":tokens) = case parseAexp tokens of
    Just (aexp, tokens') -> Just (Assign token aexp, tokens')
    _ -> Nothing
parseStm _ = Nothing
parseProgram tokens = case parseStm tokens of
    Just (stm, ";":tokens') -> case parseProgram tokens' of
        Just (program, tokens'') -> Just (stm:program, tokens'')
        _ -> Just ([stm], tokens')
    Just (stm, tokens') -> Just ([stm], tokens')
    _ -> Nothing

parse :: String -> Maybe Program
parse = fmap fst . parseProgram . lexer -}
