module Main where

import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

import AST
import Lexical
import Parser
import Scanner
import Weeder

main :: IO ()
main = do
  -- Get the single file to check from the args
  args <- getArgs
  let fileName = args !! 0

  -- Read its contents
  fileContents <- readFile fileName

  -- Even though there's just 1 file we'll put it in a list. This more closely matches A2 and makes
  -- error handling easier.
  let files = [(fileContents, fileName)]

  -- LEXER
  tokenByFiles <- mapM (scannerRunner 0 0) files
  let tokenByFilesFiltered = zip (map (filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace]))) tokenByFiles) (map snd files)

  let tokenByFilesStillValid = filter (not . any (\token -> (tokenType $ fst token) == FAILURE) . fst) tokenByFilesFiltered

  if (length tokenByFilesStillValid) == 1 then do
    hPutStrLn stderr "Scanning complete.."
  else do
    let badToken = find (\token -> (tokenType $ fst token) == FAILURE) (fst (head tokenByFilesFiltered))
    let badLocation = snd (fromJust badToken)
    let error = (file badLocation) ++ "\nLine:" ++ (show $ ln badLocation) ++ "\nColumn:" ++ (show $ col badLocation) ++ "\nContents:" ++ (lexeme . fst $ fromJust badToken)
    hPutStrLn stderr $ "Lexical error!"
    hPutStrLn stderr $ "Unexpected sequence: " ++ (show $ fromJust badToken)
    exitWith (ExitFailure 42)

  let astByFiles = map (\(tokens, file) -> (map tokenToAST tokens, file)) tokenByFilesStillValid

  -- PARSER
  dfa <- readLR1
  let resultByFiles = map (\(ast, file) -> (run (dfa, ast ++ [AST "EOF" []]), file)) astByFiles

  let validParsed = filter (\x -> (length . snd $ fst x)==0) resultByFiles

  if (length validParsed) == 1 then do
    hPutStrLn stderr "Parsing complete.."
  else do
    let errorToken = (last . snd . fst $ head resultByFiles)
    hPutStrLn stderr "Parse error!"
    hPutStrLn stderr $ "Unexpected token following: " ++ (show (content . head . units . fst . fst $ head resultByFiles))
    exitWith (ExitFailure 42)


  -- AST GENERATION
  let fileAsts = map (\x -> ((buildAST . units . fst $ fst x), snd x)) validParsed

  -- WEEDING
  let weedResults = map (\x -> weed (snd x) (fst x)) fileAsts
  let weeded = filter isJust weedResults

  if (length weeded) == 0 then do
    hPutStrLn stderr "Weeding complete..."
    hPutStrLn stderr "Input is valid!"
    exitSuccess
  else do
    hPutStrLn stderr "Weeding error!"
    hPutStrLn stderr $ fromJust (head weeded)
    exitWith (ExitFailure 42)
