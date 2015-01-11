module Scanner where
import Lexical

import Data.List
import Data.List.Split
import Data.Maybe

-- Line terminators are whitespace AND can't be present in strings
lineTerminators = ['\f', '\r', '\n']
whitespaceCharacters = [' ', '\t'] ++ lineTerminators

-- We want to be sure that keyword and bool/null literals don't tokenize as valid identifiers
invalidIdentifiers = keywords ++ ["true", "false", "null"]

-- Identifiers are composed of these javaLetters and javaDigits
javaLetters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$"
javaLettersAndDigits = javaLetters ++ "0123456789"

-- Reserved keywords that turn into keyword tokens but can't be identifiers
keywords = ["boolean", "break", "byte", "case", "catch", "char", "class",
  "const", "continue", "default", "do", "double", "else", "extends", "final",
  "finally", "float", "for", "goto", "if", "implements",
  "import", "instanceof", "int", "interface", "long", "native", "new",
  "package", "private", "protected", "public", "return", "short", "static",
  "strictfp", "super", "switch", "synchronized", "this", "throw", "throws",
  "transient", "try", "void", "volatile", "while"]

-- Valid operators in joos
operators = [">>>=", ">>>", ">>=", "<<=", "++", "+=", "%=", "&&", "&=", "*=",
             "/=", "!=", "--", "-=", "||", "|=", ">>", ">=", "=>", "==", "<=",
             "<<", "^=", "+", "-", "%", "&", "*", "/", "?", "!", ":", "|", "<",
             "~", "^"]

-- Characters that are consideed 'separators'
separators = "(){}[];,."

scanners = [scanBool, scanNull, scanWhitespace, scanKeyword, scanOperator, scanIdentifier,
            scanSeparator, scanEolComment, scanBlockComment]

scanBool :: String -> Maybe (String, String)
scanBool string
  | isPrefixOf "true" string = Just ("BOOL", "true")
  | isPrefixOf "false" string = Just ("BOOL", "false")
  | otherwise = Nothing

scanEolComment :: String -> Maybe (String, String)
scanEolComment string
  | isPrefixOf "//" string =
    let comment = takeWhile (\char -> not (char `elem` lineTerminators)) string
    in Just ("COMMENT", comment)
  | otherwise = Nothing

scanIdentifier :: String -> Maybe (String, String)
scanIdentifier string
  | not (leadingChar `elem` javaLetters) = Nothing
  | lex `elem` invalidIdentifiers = Nothing
  | otherwise = Just ("IDENTIFIER", (leadingChar : rest))
  where leadingChar = head string
        rest = takeWhile (\char -> char `elem` javaLettersAndDigits) (tail string)
        lex = leadingChar : rest

scanKeyword :: String -> Maybe (String, String)
scanKeyword = scanWhitelist keywords "KEYWORD"

scanBlockComment :: String -> Maybe (String, String)
scanBlockComment ('/':'*':rest)
  | comment == Nothing = Nothing
  | otherwise = Just $ ("COMMENT", "/*" ++ (fromJust comment))
  where comment = grabComment rest
        grabComment "" = Nothing
        grabComment ('*':'/':xs) = Just "*/"
        grabComment (x:xs) = fmap (x:) $ grabComment xs
scanBlockComment _ = Nothing


scanNull :: String -> Maybe (String, String)
scanNull string
  | isPrefixOf "null" string = Just ("NULL", "null")
  | otherwise = Nothing

scanOperator :: String -> Maybe (String, String)
scanOperator = scanWhitelist operators "OPERATOR"

scanSeparator :: String -> Maybe (String, String)
scanSeparator string = if (head string) `elem` separators
  then Just ("SEPARATOR", [head string])
  else Nothing

-- Lets you check for any string within a list of strings, returning a token with the given type
-- with the longest match
-- Used for both operators and keywords
scanWhitelist :: [String] -> String -> String -> Maybe (String, String)
scanWhitelist whitelist tokenType string
  | longestMatch == "" = Nothing
  | otherwise = Just (tokenType, longestMatch)
  where matches = filter (\keyword -> isPrefixOf keyword string) whitelist
        longestMatch = case matches of
          [] -> ""
          otherwise -> maximumBy (\a b -> length a `compare` length b) matches

scanWhitespace :: String -> Maybe (String, String)
scanWhitespace string
  | content == [] = Nothing
  | otherwise = Just ("WHITESPACE", content)
  where content = takeWhile (\char -> char `elem` whitespaceCharacters) string

scanToken :: String -> (String, String)
scanToken string = token
  where tokens = mapMaybe (\scanner -> scanner string) scanners
        token  = case tokens of
          [] -> ("ERR", [head string])
          otherwise -> maximumBy (\(_, a) (_, b) -> (length a) `compare` (length b)) tokens

scanLine :: Int -> String -> [Token]
scanLine _ "" = []
scanLine lineNum string = token : scanLine lineNum (drop (tokenLength token) string)
  where (tokenKind, tokenContent) = scanToken string
        token = Token tokenKind tokenContent lineNum

scanTokens :: String -> [Token]
scanTokens string = foldr (++) [] . map (\(line, content) -> scanLine line content) $ zip [0..] [string]
