module Compiler where

import Datastructures
import Data.Char (isDigit)

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
compB (EquExp a1 a2) = compA a1 ++ compA a2 ++ [Equ]

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

{-
type Parser a = [String] -> Maybe (a, [String])

parseAexp :: Parser Aexp
parseAexp tokens = case parseTerm tokens of
    Just (term, "-":tokens') -> case parseAexp tokens' of
        Just (aexp, tokens'') -> Just (SubExp term aexp, tokens'')
        _ -> Just (term, tokens')
    Just (term, tokens') -> Just (term, tokens')
    _ -> Nothing

parseTerm :: Parser Aexp
parseTerm (token:tokens)
    | all isDigit token = Just (Num (read token), tokens)
    | otherwise = Just (Var token, tokens)

parseBexp :: Parser Bexp
parseBexp tokens = case parseAexp tokens of
    Just (aexp1, "=":tokens') -> case parseAexp tokens' of
        Just (aexp2, tokens'') -> Just (EquExp aexp1 aexp2, tokens'')
        _ -> Nothing
    _ -> Nothing

parseStm :: Parser Stm
parseStm (token:":=":tokens) = case parseAexp tokens of
    Just (aexp, tokens') -> Just (Assign token aexp, tokens')
    _ -> Nothing
parseStm _ = Nothing

parseProgram :: Parser Program
parseProgram [] = Just ([], [])
parseProgram tokens = case parseStm tokens of
    Just (stm, ";":tokens') -> case parseProgram tokens' of
        Just (program, tokens'') -> Just (stm:program, tokens'')
        _ -> Just ([stm], tokens')
    Just (stm, tokens') -> Just ([stm], tokens')
    _ -> Nothing

parse :: String -> Maybe Program
parse = fmap fst . parseProgram . lexer
-}