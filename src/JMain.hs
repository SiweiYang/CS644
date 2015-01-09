module Main where
import Scanner

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fileName = args !! 0
  fileContents <- readFile fileName
  putStrLn $ show $ scanTokens fileContents
