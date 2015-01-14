module Main where

import Lexical
import Scanner

import Data.Char(readLitChar)
import Text.ParserCombinators.ReadP(eof, many, ReadP, readS_to_P, readP_to_S)

import System.Directory

strParser :: ReadP String
strParser = do
    str <- many (readS_to_P readLitChar)
    eof
    return str
trans :: String -> String
trans = fst . head . (readP_to_S strParser)
-----------------------------------------------------------------------------------------------------------------------------
fixLength = (build scanKeyword) <=> (build scanOperator) <=> (build scanSeparator) <=> (build scanBool) <=> (build scanNull) :: Lex
literal = (build scanChar) <=> (build scanString) <=> (build scanDecimalInteger) :: Lex
leftover = (build scanIdentifier) <=> (build scanEolComment) <=> (build scanBlockComment) <=> (build scanWhitespace) :: Lex
lexer = fixLength <|> literal <|> leftover
-------------------------------------- Helpers ------------------------------------------------------------------------------
testTokens :: IO [(String, String)]
testTokens = do
    f <- readFile "res/token.test"
    let nl = zip [0..] (lines f)
    return (zip [trans t | (n, t) <- nl, mod n 2 == 0] [t | (n, t) <- nl, mod n 2 == 1])

testFiles :: IO [(String, String)]
testFiles = do
    f <- getDirectoryContents "../assignment_testcases/a1"
    let files = ["../assignment_testcases/a1/" ++ file | file <- f, file /= ".", file /= ".."]
    contents <- mapM readFile files
    return (zip contents files)

printPairs :: [(String, String)] -> IO()
printPairs ((lexeme, token):ls) = do
    putStrLn lexeme
    putStrLn token
    if ls == []
    then return ()
    else printPairs ls

lexerRunner :: [(String, String)] -> IO()
lexerRunner ((lexeme, token):ls) = do
    putStr (show (lexeme, token))
    putStr " => "
    putStrLn (show (lexeme |> lexer))
    if ls == []
    then return ()
    else lexerRunner ls
-----------------------------------------------------------------------------------------------------------------------------
main :: IO()
main = do
    pairs <- testTokens
    lexerRunner pairs
    --putStrLn (show pairs)