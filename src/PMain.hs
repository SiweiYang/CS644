module Main where 

import Lexical
import Scanner
import Parser

import System.Directory

--------------------------------------------------------------------
testSingleFile :: IO (String, String)
testSingleFile = do
    let file = "../assignment_testcases/a1/J1_1_Cast_MultipleCastOfSameValue_1.java"
    --let file = "../assignment_testcases/a1/Je_1_Escapes_1DigitOctal_1.java"
    content <- readFile file
    return (content, file)

testVFiles :: IO [(String, String)]
testVFiles = do
    f <- getDirectoryContents "../assignment_testcases/a1"
    let files = ["../assignment_testcases/a1/" ++ file | file <- f, file /= ".", file /= "..", take 2 file /= "Je"]
    contents <- mapM readFile files
    return (zip contents files)

testDFA = do
    dfa <- readLR1
    singlefile <- testSingleFile
    tokenList <- (scannerRunner 0 0) singlefile
    let filtedToken = filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace])) tokenList
    let astList = map tokenToAST filtedToken
    return (run (dfa, astList))
    
main :: IO ()
main = do
    dfa <- readLR1
    files <- testVFiles
    tokenByFiles <- mapM (scannerRunner 0 0) files
    let tokenByFilesFiltered = map (filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace]))) tokenByFiles
    let astByFiles = map (map tokenToAST) tokenByFilesFiltered
    let resultByFiles = map (\ast -> run (dfa, ast)) astByFiles
    let res = zip (map snd resultByFiles) (map snd files)
    
    putStrLn (show (filter (\(r, f) -> length r == 0) res))
	--putStrLn (foldl (\acc x -> acc ++ show x ++ "\n") "" listC)