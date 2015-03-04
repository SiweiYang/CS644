module Main where

import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

import AST
import Environment
import Hierarchy
import Lexical
import Parser
import Scanner
import TypeDatabase
import Weeder

main :: IO ()
main = do
  -- Get the files to compile from the args
  fileNames <- getArgs

  -- Read their contents
  fileContents <- mapM readFile fileNames

  -- Create content/filename pairs
  let files = zip fileContents fileNames

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

  let (validEnvironments, invalidEnvironments) = partition (\x -> case (fst x) of {ENVE -> False; _ -> True}) fileEnvironments

  if not $ null invalidEnvironments then do
    hPutStrLn stderr $ "Environment error in file" ++ (snd $ head invalidEnvironments)
    exitWith (ExitFailure 42)
  else do
    hPutStrLn stderr "Environment: OK"

  let globalEnvironment = buildTypeEntryFromEnvironments (TN (PKG []) []) (map fst validEnvironments)

  if isNothing globalEnvironment then do
    hPutStrLn stderr "Environment building error!"
    exitWith (ExitFailure 42)
  else do
    hPutStrLn stderr "Type DB: OK"

  -- HIERARCHY CHECKING
  let hierarchyResults = checkHierarchies (map fst fileAsts) (fromJust globalEnvironment)

  if isJust hierarchyResults then do
    hPutStrLn stderr "Hierarchy error!"
    hPutStrLn stderr $ fromJust hierarchyResults
    exitWith (ExitFailure 42)
  else do
    hPutStrLn stderr "Hierarchy: OK"
    exitSuccess

