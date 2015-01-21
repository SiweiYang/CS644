module Main where

import Lexical
import Scanner

import Data.List
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

testSingleFile :: IO (String, String)
testSingleFile = do
    --let file = "../assignment_testcases/a1/J1_1_Cast_MultipleCastOfSameValue_1.java"
    let file = "../assignment_testcases/a1/Je_1_Escapes_1DigitOctal_1.java"
    content <- readFile file
    return (content, file)

printList :: Show a => [a] -> IO()
printList [] = return ()
printList (x:ls) = do
    putStrLn (show x)
    printList ls

-----------------------------------------------------------------------------------------------------------------------------
main :: IO()
main = do
    --pairs <- testTokens
    --efiles <- testEFiles
    --vfiles <- testVFiles
    --fileResults <- mapM (scannerRunner 0 0) vfiles
    --let res = map (\items -> filter (\(tk, fn) -> elem (tokenType tk) [FAILURE]) items) fileResults
    --putStrLn (show res)

    singlefile <- testSingleFile
    fileResults <- (scannerRunner 0 0) singlefile
    let res = map (\(tk, tkinfo) -> (tk, ln tkinfo, col tkinfo)) fileResults
    putStrLn (foldl (\acc t -> acc ++ (show t) ++ "\n") "" res)
    --let res = map (\items -> filter (\(tk, fn) -> elem (tokenType tk) [FAILURE]) items) fileResults
    --printList (zip (map snd singlefile) res)
    
    --fileResults <- mapM (scannerRunner 0 0) efiles
    --let res = map (\items -> filter (\(tk, fn) -> elem (tokenType tk) [FAILURE]) items) fileResults
    --printList (zip (map snd efiles) res)
    --lexerRunner pairs
    --putStrLn (show pairs)