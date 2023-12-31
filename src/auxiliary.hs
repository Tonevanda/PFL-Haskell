module Auxiliary where

import Data.Char (isDigit, isAlpha)

isAllLetters :: String -> Bool
isAllLetters = all isAlpha

isAllNumbers :: String -> Bool
isAllNumbers = all isDigit

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

-- Used to find the next valid token in the list of tokens
nextValidToken :: ([String],[String]) -> String -> ([String],[String])
nextValidToken ([], x:right) token
    | x==token = ([], x:right)
nextValidToken ([], right) _ = ([], right)
nextValidToken ("(":left, []) token = nextValidToken (init left , [last left]) token
nextValidToken (left, []) token = nextValidToken (init left , [last left]) token
nextValidToken (left, x:right) token
    | x == token && check left = (left , x:right)
    | otherwise = nextValidToken (init left , last left:x:right) token

-- Similar to nextValidToken, but used strictly for parsing arithmetic expressions
nextValidAToken :: ([String],[String]) -> String -> ([String],[String])
nextValidAToken ([], x:right) token
    | x==token = ([], x:right)
nextValidAToken ([], right) _ = ([], right)
nextValidAToken (left, []) token = nextValidAToken (init left , [last left]) token
nextValidAToken (left, x:right) token
    | x == token && check left = (left , x:right)
    | otherwise = nextValidAToken (init left , last left:x:right) token

-- Checks if the parenthesis are balanced
check :: [String] -> Bool
check = (== 0) . foldl updateCount 0
  where
    updateCount count "(" = count + 1
    updateCount count ")" = max 0 (count - 1)
    updateCount count _   = count
