module Main where 

import Data.Map hiding (map, null, filter, partition)
import Data.List
import Data.Maybe

import System.Environment
import System.Directory
import System.Exit
import System.IO

import Lexical
import Scanner
import Parser

import AST
import Environment
import Hierarchy
import TypeDatabase
import TypeLinking
import Weeder

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

{-
testVFiles :: IO [(String, String)]
testVFiles = do
    f <- getDirectoryContents "../assignment_testcases/a1"
    let files = ["../assignment_testcases/a1/" ++ file | file <- f, file /= ".", file /= "..", take 2 file /= "Je", take 1 file /= "."]
    contents <- mapM readFile files
    return (zip contents files)
-}

testLibFiles :: IO [(String, String)]
testLibFiles = do
    f <- getDirectoryContents "../assignment_testcases/stdlib2"
    let files = ["../assignment_testcases/stdlib2/" ++ file | file <- f, file /= ".", file /= ".."]
    contents <- mapM readFile files
    return (zip contents files)

{-
testDFA = do
    dfa <- readDFA
    singlefile <- testSingleFile
    tokenList <- (scannerRunner 0 0) singlefile
    let filtedToken = filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace])) tokenList
    let astList = map tokenToAST filtedToken
    let (dfa', astList') = run (dfa, astList)
    return (run (dfa', astList' ++ [AST "EOF" []]))
-}

getPackageNode a = production ((units (fst a)) !! 3) !! 1
getImportsNode a = (units (fst a)) !! 2
getModifiersNode a = (production $ (production $ (units $ fst a) !! 1) !! 0) !! 3
getClassBodyNode a = (production $ (production $ (units $ fst a) !! 1) !! 0) !! 0
getClassBodyDecNode a = (production $ (production $ getClassBodyNode a) !! 1) !! 0


testTD = do
    dfa <- readDFA
    files <- testLibFiles
    tokenByFiles <- mapM (scannerRunner 0 0) files
    let tokenByFilesFiltered = map (filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace]))) tokenByFiles
    let astByFiles = map (\x -> (file (snd (head x)), map tokenToAST x)) tokenByFilesFiltered
    let resultByFiles = map (\(fn, ast) -> (fn, run (dfa, ast ++ [AST "EOF" []]))) astByFiles
    let astByFiles = map (\(fn, a) -> (fn, buildAST $ units (fst a))) resultByFiles
    let envByFiles = map (\(fn, a) -> (fn, buildEnvironment a)) astByFiles
    let envs = map snd envByFiles
    let dbType = fromJust $ buildTypeEntryFromEnvironments (TN (PKG []) []) envs
    let dbInstance = fromJust $ buildInstanceEntryFromEnvironments (TN (PKG []) []) envs
    --let tn = traverseInstanceEntry' dbInstance dbInstance ["java", "lang", "Boolean"]
    let tn = traverseTypeEntry dbType ["java", "lang", "Boolean"]
    putStrLn (show tn)
    --putStrLn (show dbInstance)
    --putStrLn (show dbType)

    --return envs
    --env <- testENV
    --return (buildTypeEntry (TN (PKG []) []) env)

main :: IO ()
main = do
  -- Get the files to compile from the args
  fileNames <- getArgs
  hPutStrLn stderr (show fileNames)
  main' fileNames

main' :: [String] -> IO ()
main' givenFileNames = do
  let allFileNames = givenFileNames ++ ["./res/ObjectInterface.java"]

  -- Read their contents
  fileContents <- mapM readFile allFileNames

  -- Create content/filename pairs
  let files = zip fileContents allFileNames

  -- LEXER
  tokenByFiles <- mapM (scannerRunner 0 0) files

  -- Now we strip out the comment & whitespace tokens because they aren't needed by the parser
  let tokenByFilesFiltered = zip (map (filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace]))) tokenByFiles) (map snd files)
  -- The presence of a FAILURE token means something went wrong during lexing
  -- Let's split the files into 2 groups - valid and invalid
  let (tokenByFilesInvalid, tokenByFilesValid) = partition (any (\token -> (tokenType $ fst token) == FAILURE) . fst) tokenByFilesFiltered

  if null tokenByFilesInvalid then do
    hPutStrLn stderr "Scanning OK"
  else do
    let badToken = find (\token -> (tokenType $ fst token) == FAILURE) (fst (head tokenByFilesInvalid))
    let badLocation = snd (fromJust badToken)
    let error = (file badLocation) ++ "\nLine:" ++ (show $ ln badLocation) ++ "\nColumn:" ++ (show $ col badLocation) ++ "\nContents:" ++ (lexeme . fst $ fromJust badToken)
    hPutStrLn stderr $ "Lexical error!"
    hPutStrLn stderr $ "Unexpected sequence: " ++ (show $ fromJust badToken)
    exitWith (ExitFailure 42)

  let astByFiles = map (\(tokens, file) -> (map tokenToAST tokens, file)) tokenByFilesValid

  -- PARSER
  dfa <- readLR1
  let resultByFiles = map (\(ast, file) -> (run (dfa, ast ++ [AST "EOF" []]), file)) astByFiles

  let (validParsed, invalidParsed) = partition (\x -> (length . snd $ fst x)==0) resultByFiles

  if null invalidParsed then do
    hPutStrLn stderr "Parsing OK"
  else do
    let errorToken = (last . snd . fst $ head invalidParsed)
    hPutStrLn stderr "Parse error!"
    hPutStrLn stderr $ "Unexpected token following: " ++ (show (content . head . units . fst . fst $ head invalidParsed))
    exitWith (ExitFailure 42)


  -- AST GENERATION
  let fileAsts = map (\x -> ((buildAST . units . fst $ fst x), snd x)) validParsed

  -- WEEDING
  let weedResults = map (\x -> weed (snd x) (fst x)) fileAsts
  let weeded = filter isJust weedResults

  if (length weeded) == 0 then do
    hPutStrLn stderr "Weeding: OK"
  else do
    hPutStrLn stderr "Weeding error!"
    hPutStrLn stderr $ fromJust (head weeded)
    exitWith (ExitFailure 42)

  -- ENVIRONMENT CONSTRUCTION
  let fileEnvironments = map (\x -> (buildEnvironment $ fst x, snd x)) fileAsts
  let fileEnvironmentWithImports = map (\x -> (visibleImports $ fst x, buildEnvironment $ fst x, snd x)) fileAsts

  let (validEnvironments, invalidEnvironments) = partition (\x -> case (fst x) of {ENVE -> False; _ -> True}) fileEnvironments

  if not $ null invalidEnvironments then do
    hPutStrLn stderr $ "Environment error in file" ++ (snd $ head invalidEnvironments)
    exitWith (ExitFailure 42)
  else do
    hPutStrLn stderr "Environment: OK"

  -- Type Linking
  let mtypeDB = buildTypeEntryFromEnvironments nativeTypes (map fst validEnvironments)
  if isNothing mtypeDB then do
    hPutStrLn stderr "Environment DB building error!"
    exitWith (ExitFailure 42)
  else do
    hPutStrLn stderr "Environment DB: OK"
  let Just typeDB = mtypeDB

  let listImpEnvFns = map (\(imp, env, fn) -> (imp, refineEnvironmentWithType typeDB imp (Root []) env, fn)) fileEnvironmentWithImports
  if length [Nothing | (_, Nothing, _) <- listImpEnvFns] > 0 then do
    hPutStrLn stderr "Environment Refine error!"
    exitWith (ExitFailure 42)
  else do
    hPutStrLn stderr "Environment Refine: OK"

  -- Scope Checking
  let scopeCheck = filter (\(imp, Just env, fn) -> checkSameNameInEnvironment env) listImpEnvFns
  if not $ null scopeCheck then do
    hPutStrLn stderr $ "Scope checking error in files: " ++ (show (map (\(imp, Just env, fn) -> fn) scopeCheck))
    exitWith (ExitFailure 42)
  else do
    hPutStrLn stderr "Scope Checking: OK"

  let mdb = (buildInstanceEntryFromEnvironments nativeTypes (map (\(imp, Just env, fn) -> env) listImpEnvFns))

  if isNothing mdb then do
    hPutStrLn stderr "Instance DB building error!"
    exitWith (ExitFailure 42)
  else do
    hPutStrLn stderr "Instance DB: OK"
  let Just db = mdb

  -- HIERARCHY CHECKING
  let hierarchyResults = checkHierarchies (map fst fileAsts) db

  if isJust hierarchyResults then do
    hPutStrLn stderr "Hierarchy error!"
    hPutStrLn stderr $ fromJust hierarchyResults
    exitWith (ExitFailure 42)
  else do
    hPutStrLn stderr "Hierarchy: OK"

  -- Update DB with inheritance relations
  --hPutStrLn stderr (show fileNames)
  let cn = dumpDBNodes db
  let relations = [((typeToName . localType . symbol) node, ["java","lang","Object"]:(map (typeToName . localType . symbol) (getClassHierarchyForSymbol node db))) | node <- cn]
  --hPutStrLn stderr (show relations)

  let mdb' = updateDBWithInheritances db relations
  if isNothing mdb' then do
    hPutStrLn stderr "Inheritance DB building error!"
    exitWith (ExitFailure 42)
  else do
    hPutStrLn stderr "Inheritance DB: OK"
  let Just db' = mdb'

  let failures = filter (\(imp, Just env, fn) ->  typeLinkingCheck db' imp env == []) (filter (\(imp, Just env, fn) -> (reverse (take 14 (reverse fn))) == "short_int.java") listImpEnvFns)
  if length failures > 0 then do
    hPutStrLn stderr "Type Linking error!"
    --hPutStrLn stderr (show failures)
    hPutStrLn stderr (show $ map (\(imp, Just env, fn) -> fn) failures)
    exitWith (ExitFailure 42)
  else do
    hPutStrLn stderr "Type Linking: OK"

