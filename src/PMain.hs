module Main where 

import Lexical
import Scanner
import Parser

--------------------------------------------------------------------
testSingleFile :: IO (String, String)
testSingleFile = do
    let file = "../assignment_testcases/a1/J1_1_Cast_MultipleCastOfSameValue_1.java"
    --let file = "../assignment_testcases/a1/Je_1_Escapes_1DigitOctal_1.java"
    content <- readFile file
    return (content, file)

testDFA = do
    dfa <- readLR1
    singlefile <- testSingleFile
    tokenList <- (scannerRunner 0 0) singlefile
    let filtedToken = filter (\(tk, fn) -> not (elem (tokenType tk) [Comment, WhiteSpace])) tokenList
    let astList = map tokenToAST filtedToken
    return (run (dfa, astList))
    
main :: IO ()
main = do
    res <- testDFA
    let finalAST = snd res
    putStrLn (show finalAST)
	--putStrLn (foldl (\acc x -> acc ++ show x ++ "\n") "" listC)