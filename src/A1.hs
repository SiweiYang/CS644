module Main where

import System.Environment
import System.Exit

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

  let astByFiles = map (\(tokens, file) -> (map tokenToAST tokens, file)) tokenByFilesStillValid

  -- PARSER
  dfa <- readLR1
  let resultByFiles = map (\(ast, file) -> (run (dfa, ast ++ [AST "EOF" []]), file)) astByFiles

  let validParsed = filter (\x -> (length . snd $ fst x)==0) resultByFiles

  -- AST GENERATION
  let fileAsts = map (\x -> ((buildAST . units . fst $ fst x), snd x)) validParsed

  -- WEEDING
  let unweeded = filter (\x -> (weed (snd x) (fst x)) == Nothing) fileAsts

  if (length unweeded) == 1
    then exitSuccess
    else exitWith (ExitFailure 42)
