module Main where 

import Lexical
import Scanner
import Parser

import AST

import System.Directory

--------------------------------------------------------------------
testSingleFile :: IO (String, String)
testSingleFile = do
    let file = "../assignment_testcases/a1/J1_IntRange_MinNegativeInt.java"
    content <- readFile file
    return (content, file)

testVFiles :: IO [(String, String)]
testVFiles = do
    f <- getDirectoryContents "../assignment_testcases/a1"
    let files = ["../assignment_testcases/a1/" ++ file | file <- f, file /= ".", file /= "..", take 2 file /= "Je", take 1 file /= "."]
    contents <- mapM readFile files
    return (zip contents files)

testDFA = do
    dfa <- readLR1
    singlefile <- testSingleFile
    tokenList <- (scannerRunner 0 0) singlefile
    let filtedToken = filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace])) tokenList
    let astList = map tokenToAST filtedToken
    let (dfa', astList') = run (dfa, astList)
    return (run (dfa', astList' ++ [AST "EOF" []]))

getPackageNode a = production ((units (fst a)) !! 3) !! 1
getImportsNode a = (units (fst a)) !! 2
getModifiersNode a = (production $ (production $ (units $ fst a) !! 1) !! 0) !! 3
getClassBodyNode a = (production $ (production $ (units $ fst a) !! 1) !! 0) !! 0
getClassBodyDecNode a = (production $ (production $ getClassBodyNode a) !! 1) !! 0

testAST = do
    a <- testDFA
    let cu = buildAST $ units (fst a)
    let cons = constructors $ definition cu
    let flds = fields $ definition cu
    let mtds = methods $ definition cu
    return (definition cu)

main :: IO ()
main = do
    dfa <- readLR1
    files <- testVFiles
    tokenByFiles <- mapM (scannerRunner 0 0) files
    let tokenByFilesFiltered = map (filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace]))) tokenByFiles
    let astByFiles = map (map tokenToAST) tokenByFilesFiltered
    let resultByFiles = map (\ast -> run (dfa, ast ++ [AST "EOF" []])) astByFiles
    let astByFiles = map (\a -> buildAST $ units (fst a)) resultByFiles
    putStrLn (show astByFiles)
    
    let res = zip (map snd resultByFiles) (map snd files)
    let pass = filter (\(r, f) -> length r == 0) res
    
    putStrLn ("test passed: " ++ (show (length pass)) ++ "/" ++ (show (length res)))
    putStrLn (show (filter (\(r, f) -> length r > 0) res))
	--putStrLn (foldl (\acc x -> acc ++ show x ++ "\n") "" listC)