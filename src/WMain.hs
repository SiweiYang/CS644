module Main where

import Lexical
import Scanner
import Parser
import Weeder

import AST

import System.Directory

--------------------------------------------------------------------
testVFiles :: IO [(String, String)]
testVFiles = do
    f <- getDirectoryContents "assignment_testcases/a1"
    let files = ["assignment_testcases/a1/" ++ file | file <- f, file /= ".", file /= "..", take 2 file /= "Je", take 1 file /= "."]
    contents <- mapM readFile files
    return (zip contents files)

testEFiles :: IO [(String, String)]
testEFiles = do
    f <- getDirectoryContents "assignment_testcases/a1"
    let files = ["assignment_testcases/a1/" ++ file | file <- f, file /= ".", file /= "..", take 2 file == "Je"]
    contents <- mapM readFile files
    return (zip contents files)

testValidFiles :: IO ()
testValidFiles = do
    dfa <- readLR1
    files <- testVFiles
    tokenByFiles <- mapM (scannerRunner 0 0) files
    let tokenByFilesFiltered = zip (map (filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace]))) tokenByFiles) (map snd files)
    let tokenByFilesStillValid = filter (not . any (\token -> (tokenType $ fst token) == FAILURE) . fst) tokenByFilesFiltered

    let astByFiles = map (\(tokens, file) -> (map tokenToAST tokens, file)) tokenByFilesStillValid
    let resultByFiles = map (\(ast, file) -> (run (dfa, ast ++ [AST "EOF" []]), file)) astByFiles

    let validParsed = filter (\x -> (length . snd $ fst x)==0) resultByFiles

    let fileAsts = map (\x -> ((buildAST . units . fst $ fst x), snd x)) validParsed

    let unweeded = filter (\x -> (weed (snd x) (fst x)) == Nothing) fileAsts
    let weeded = filter (\x -> (weed (snd x) (fst x)) /= Nothing) fileAsts

    putStrLn $ ("Valid Tests Weeded: " ++ (show $ length weeded) ++ "/" ++ (show $ length fileAsts))
    putStrLn $ foldl (\acc x -> acc ++ x ++ "\n") "" (map snd weeded)

testInvalidFiles :: IO ()
testInvalidFiles = do
    dfa <- readLR1
    files <- testEFiles
    tokenByFiles <- mapM (scannerRunner 0 0) files
    let tokenByFilesFiltered = zip (map (filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace]))) tokenByFiles) (map snd files)
    let tokenByFilesStillValid = filter (not . any (\token -> (tokenType $ fst token) == FAILURE) . fst) tokenByFilesFiltered

    let astByFiles = map (\(tokens, file) -> (map tokenToAST tokens, file)) tokenByFilesStillValid
    let resultByFiles = map (\(ast, file) -> (run (dfa, ast ++ [AST "EOF" []]), file)) astByFiles

    let validParsed = filter (\x -> (length . snd $ fst x)==0) resultByFiles

    let fileAsts = map (\x -> ((buildAST . units . fst $ fst x), snd x)) validParsed

    let unweeded = filter (\x -> (weed (snd x) (fst x)) == Nothing) fileAsts
    let weeded = filter (\x -> (weed (snd x) (fst x)) /= Nothing) fileAsts

    putStrLn $ ("Invalid Tests Weeded: " ++ (show $ length weeded) ++ "/" ++ (show $ length fileAsts))
    putStrLn $ foldl (\acc x -> acc ++ x ++ "\n") "" (map snd unweeded)

main :: IO ()
main = do
    testValidFiles
    testInvalidFiles
