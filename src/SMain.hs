module Main where

import Lexical
import Scanner

import Data.Maybe
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
testEFiles :: IO [(String, String)]
testEFiles = do
    f <- getDirectoryContents "../assignment_testcases/a1"
    let files = ["../assignment_testcases/a1/" ++ file | file <- f, file /= ".", file /= "..", take 2 file == "Je"]
    contents <- mapM readFile files
    return (zip contents files)
testVFiles :: IO [(String, String)]
testVFiles = do
    f <- getDirectoryContents "../assignment_testcases/a1"
    let files = ["../assignment_testcases/a1/" ++ file | file <- f, file /= ".", file /= "..", take 2 file /= "Je"]
    contents <- mapM readFile files
    return (zip contents files)

printList :: Show a => [a] -> IO()
printList [] = return ()
printList (x:ls) = do
    putStrLn (show x)
    printList ls

lexerRunner :: [(String, String)] -> IO()
lexerRunner ((lexeme, token):ls) = do
    putStr (show (lexeme, token))
    putStr " => "
    putStrLn (show (lexeme |> lexer))
    if ls == []
    then return ()
    else lexerRunner ls

scannerRunner :: Int -> Int -> (String, String) -> IO [(Token, TokenInfo)]
scannerRunner _ _ ([], _) = return []
scannerRunner ln col (fc, fn) = do
    let m = fc |> lexer
    if isNothing m
    then return [(Token FAILURE (head (splitOneOf "\n" fc)), TI fn ln col)]
    else do
        let Just tk = m
        let multiline = if (elem '\n' (lexeme tk)) then splitOneOf "\n" (lexeme tk) else []
        let nextLn = ln + (length multiline)
        let nextCol = if multiline == [] then col + (length (lexeme tk)) else (length (last multiline))
        rst <- scannerRunner nextLn nextCol ((drop (length (lexeme tk)) fc), fn)
        return ((tk, TI fn ln col):rst)

-----------------------------------------------------------------------------------------------------------------------------
main :: IO()
main = do
    pairs <- testTokens
    efiles <- testEFiles
    vfiles <- testVFiles
    fileResults <- mapM (scannerRunner 1 1) vfiles
    let res = map (\items -> filter (\(tk, fn) -> elem (tokenType tk) [FAILURE]) items) fileResults
    --putStrLn (show res)
    
    fileResults <- mapM (scannerRunner 1 1) efiles
    let res = map (\items -> filter (\(tk, fn) -> elem (tokenType tk) [FAILURE]) items) fileResults
    printList (zip (map snd efiles) res)
    --lexerRunner pairs
    --putStrLn (show pairs)