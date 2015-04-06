module Generation where

import Data.Map (fromList)
import Data.Char (ord)

import Data.Char (readLitChar)
import Text.ParserCombinators.ReadP (eof, many, ReadP, readS_to_P, readP_to_S)

strParser :: ReadP String
strParser = do
  str <- many (readS_to_P readLitChar)
  eof
  return str

trans :: String -> String
trans = fst . head . (readP_to_S strParser)

generateForStrings :: [String] -> ([(String, String)], [(String, String)])
generateForStrings strs = (pair1, pair2)
  where
    pair1 = [(str, "literal_string_" ++ show i) | (str, i) <- zip strs [0..]]
    pair2 = map generateDataForString pair1

generateDataForString :: (String, String) -> (String, String)
generateDataForString (str, lb) = (str, "global " ++ lb ++ "\n" ++ lb ++ ":\n" ++ (concat vals))
  where
    vals = ["dd " ++ (show (ord ch)) ++ "\n" | ch <- tail $ init $ trans str]
