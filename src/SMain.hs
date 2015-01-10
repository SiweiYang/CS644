module Main where

import Data.List.Split(splitOn)
import Data.Char(readLitChar)
import Text.ParserCombinators.ReadP(eof, many, ReadP, readS_to_P, readP_to_S)

strParser :: ReadP String
strParser = do
    str <- many (readS_to_P readLitChar)
    eof
    return str
--trans :: String -> [(String, String)]
trans :: String -> String
trans = fst . head . (readP_to_S strParser)

printPairs :: [(String, String)] -> IO()
printPairs ((lexeme, token):ls) = do
    putStrLn lexeme
    putStrLn token
    if ls == []
    then return ()
    else printPairs ls

main :: IO()
main = do
    f <- readFile "res/token.test"
    let nl = zip [0..] (lines f)
    let pairs = zip [trans t | (n, t) <- nl, mod n 2 == 0] [t | (n, t) <- nl, mod n 2 == 1]
    printPairs pairs
    --putStrLn (show pairs)