module Main where 

import Data.Maybe
import Data.Map hiding (map, filter)

import Lexical
import Scanner
import Parser

import AST
import Environment
import TypeDatabase

import System.Directory

readDFA :: IO DFA
readDFA = do
    contentStr <- readFile "../res/joos1w.lr1"
    let content = lines contentStr
    let numOfTerminal = read (head content) :: Int
    let terminalList = take numOfTerminal (drop 1 content)
    let numOfNonterminal = read (content !! (1 + numOfTerminal)) :: Int
    let nonterminalList = take numOfNonterminal (drop (2 + numOfTerminal) content)
    let root = content !! (1 + numOfTerminal + 1 + numOfNonterminal)
    let numOfProduction = read (content !! (1 + numOfTerminal + 1 + numOfNonterminal + 1)) :: Int
    let productionList = map (buildProduction . words) (take numOfProduction (drop (1 + numOfTerminal + 1 + numOfNonterminal + 1 + 1) content))
    let numOfState = read (content !! (1 + numOfTerminal + 1 + numOfNonterminal + 1 + 1 + numOfProduction)) :: Int
    let numOfTransition = read (content !! (1 + numOfTerminal + 1 + numOfNonterminal + 1 + 1 + numOfProduction + 1)) :: Int
    let transitionList = map (buildTransition . words) (take numOfTransition (drop (1 + numOfTerminal + 1 + numOfNonterminal + 1 + 1 + numOfProduction + 1 + 1) content))
    let transitionMap = fromList(transitionList)
    --return (terminalList, nonterminalList, root, productionList, numOfState, transitionMap)
    return (DFA [0] [] numOfState transitionMap productionList)

--------------------------------------------------------------------

testVFiles :: IO [(String, String)]
testVFiles = do
    f <- getDirectoryContents "../assignment_testcases/a1"
    let files = ["../assignment_testcases/a1/" ++ file | file <- f, file /= ".", file /= "..", take 2 file /= "Je", take 1 file /= "."]
    contents <- mapM readFile files
    return (zip contents files)

testLibFiles :: IO [(String, String)]
testLibFiles = do
    f <- getDirectoryContents "../assignment_testcases/stdlib2"
    let files = ["../assignment_testcases/stdlib2/" ++ file | file <- f, file /= ".", file /= ".."]
    contents <- mapM readFile files
    return (zip contents files)

testDFA = do
    dfa <- readDFA
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

{-
testAST = do
    a <- testDFA
    let cu = buildAST $ units (fst a)
    let cons = constructors $ definition cu
    let flds = fields $ definition cu
    let mtds = methods $ definition cu
    return (definition cu)
-}

-------------------------------------------------------------

testSingleFile :: IO (String, String)
testSingleFile = do
    let file = "../assignment_testcases/a1/J1_siwei.java"
    --let file = "../assignment_testcases/a1/Je_1_PackagePrivate_Class.java"
    content <- readFile file
    return (content, file)


--testAST :: IO ()
testAST = do
    dfa <- readDFA
    singlefile <- testSingleFile
    tokenByFile <- scannerRunner 0 0 singlefile
    let tokenByFileFiltered = filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace])) tokenByFile
    let astByFile = (file (snd (head tokenByFileFiltered)), map tokenToAST tokenByFileFiltered)
    let resultByFile = (\(fn, ast) -> (fn, run (dfa, ast ++ [AST "EOF" []]))) astByFile
    let astByFile = (\(fn, a) -> (fn, buildAST $ units (fst a), snd a)) resultByFile
    let (_, cu, _) = astByFile
    --putStrLn (show astByFile)
    return cu
    --putStrLn "Sdfsdf"

testENV = do
    cu <- testAST
    return (buildEnvironment cu)

testTD = do
    dfa <- readDFA
    files <- testLibFiles
    tokenByFiles <- mapM (scannerRunner 0 0) files
    let tokenByFilesFiltered = map (filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace]))) tokenByFiles
    let astByFiles = map (\x -> (file (snd (head x)), map tokenToAST x)) tokenByFilesFiltered
    let resultByFiles = map (\(fn, ast) -> (fn, run (dfa, ast ++ [AST "EOF" []]))) astByFiles
    let astByFiles = map (\(fn, a) -> (fn, buildAST $ units (fst a))) resultByFiles
    let envByFiles = map (\(fn, a) -> (fn, buildEnvironment a)) astByFiles
    let imps = map (visibleImports . snd) astByFiles
    let envs = map snd envByFiles
    let Just db = (buildTypeEntryFromEnvironments (TN (PKG []) []) envs)
    let renvs = map (\(env, imp) -> refineEnvironmentWithType (traverseTypeEntryWithImports db imp) (Root []) env) (zip envs imps)
    let Just idb = (buildInstanceEntryFromEnvironments (TN (PKG []) []) (map fromJust renvs))
    
    --return (idb, traverseTypeEntryWithImports db [["java", "lang", "*"]])
    return (idb, traverseInstanceEntry' idb idb)
    --(db, q) <- testTD
    --change the Integer class to have a Integer field named next
    --q ["java", "lang", "Integer", "next", "next", "next", "next", "next"]
    
    
    --return (imps, renvs)
    --(imps, renvs) <- testTD
    --renv !! 6
    
    --return (buildTypeEntry (TN (PKG []) []) env)

main :: IO ()
main = do
    dfa <- readDFA
    files <- testVFiles
    tokenByFiles <- mapM (scannerRunner 0 0) files
    let tokenByFilesFiltered = map (filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace]))) tokenByFiles
    let astByFiles = map (\x -> (file (snd (head x)), map tokenToAST x)) tokenByFilesFiltered
    let resultByFiles = map (\(fn, ast) -> (fn, run (dfa, ast ++ [AST "EOF" []]))) astByFiles
    let astByFiles = map (\(fn, a) -> (fn, buildAST $ units (fst a))) resultByFiles
    putStrLn (show astByFiles)
    
    --let res = zip (map snd resultByFiles) (map snd files)
    --let pass = filter (\(r, f) -> length r == 0) res
    --putStrLn ("test passed: " ++ (show (length pass)) ++ "/" ++ (show (length res)))
    --putStrLn (show (filter (\(r, f) -> length r > 0) res))
	--putStrLn (foldl (\acc x -> acc ++ show x ++ "\n") "" listC)
