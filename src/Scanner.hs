module Scanner where
import Lexical

import Data.Char
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

decimalDigits = ['0'..'9']
octalDigits = ['0'..'7']

-- Reserved keywords that turn into keyword tokens but can't be identifiers
keywords = ["boolean", "break", "byte", "case", "catch", "char", "class",
  "const", "continue", "default", "do", "double", "else", "extends", "final",
  "finally", "float", "for", "goto", "if", "implements",
  "import", "instanceof", "int", "interface", "long", "native", "new",
  "package", "private", "protected", "public", "return", "short", "static",
  "strictfp", "super", "switch", "synchronized", "this", "throw", "throws",
  "transient", "try", "void", "volatile", "while"]

-- Valid operators in joos
operators = ["-", "+", "*", "%", "/", "=",
             "<", ">", "<=", ">=", "==", "!=",
             "&", "|", "!", "&&", "||"]

escapeChars = "btnfr\"'\\"

-- Characters that are consideed 'separators'
separators = "(){}[];,."

scanners = [scanBool, scanNull, scanWhitespace, scanKeyword, scanOperator, scanIdentifier,
            scanSeparator, scanEolComment, scanBlockComment, scanDecimalInteger, scanChar,
            scanString]

scanBool :: String -> Maybe (String, String)
scanBool string
  | isPrefixOf "true" string = Just ("BOOL", "true")
  | isPrefixOf "false" string = Just ("BOOL", "false")
  | otherwise = Nothing

scanChar :: String -> Maybe (String, String)
scanChar ('\'':xs)
  | javaChar == "" = Nothing
  | rest /= [] && head rest == '\'' = Just ("CHAR", '\'' : javaChar ++ '\'' : [])
  where getLoneChar = getJavaCharacter '\''
        javaChar = getLoneChar xs
        rest = fromJust $ stripPrefix javaChar xs
scanChar _ = Nothing

scanString :: String -> Maybe (String, String)
scanString ('"':xs)
  | rest /= [] && head rest == '"' = Just ("STRING", '"' : content ++ '"' : [])
  | otherwise = Nothing
  where content = getJavaString xs
        rest =  fromJust $ stripPrefix content xs
scanString _ = Nothing

getJavaString :: String -> String
getJavaString "" = ""
getJavaString ('"':_) = ""
getJavaString string =
  if null newChar then ""
  else newChar ++ getJavaString (fromJust $ stripPrefix newChar string)
  where getStringChar = getJavaCharacter '"'
        newChar = getStringChar string

getJavaCharacter :: Char -> String -> String
getJavaCharacter _ ('\\':x:xs)
  | x `elem` octalDigits = '\\' : takeWhile (`elem` octalDigits) (x:xs)
  | x `elem` escapeChars = '\\' : x : []
  | otherwise = ""
getJavaCharacter delimeter (x:xs)
  | x `elem` (delimeter:'\\':lineTerminators) = ""
  | otherwise = [x]
getJavaCharacter _ "" = ""

scanEolComment :: String -> Maybe (String, String)
scanEolComment string
  | isPrefixOf "//" string =
    let comment = takeWhile (\char -> not (char `elem` lineTerminators)) string
    in Just ("COMMENT", comment)
  | otherwise = Nothing

scanBlockComment :: String -> Maybe (String, String)
scanBlockComment ('/':'*':rest)
  | comment == Nothing = Nothing
  | otherwise = Just $ ("COMMENT", "/*" ++ (fromJust comment))
  where comment = grabComment rest
        grabComment "" = Nothing
        grabComment ('*':'/':xs) = Just "*/"
        grabComment (x:xs) = fmap (x:) $ grabComment xs
scanBlockComment _ = Nothing

scanIdentifier :: String -> Maybe (String, String)
scanIdentifier string
  | not (leadingChar `elem` javaLetters) = Nothing
  | lex `elem` invalidIdentifiers = Nothing
  | otherwise = Just ("IDENTIFIER", (leadingChar : rest))
  where leadingChar = head string
        rest = takeWhile (`elem` javaLettersAndDigits) (tail string)
        lex = leadingChar : rest

scanDecimalInteger :: String -> Maybe (String, String)
scanDecimalInteger ('0':xs) = Just ("DEC_INTEGER", "0")
scanDecimalInteger (x:xs)
  | x `elem` decimalDigits =
    let lex = x : takeWhile (`elem` decimalDigits) xs
    in Just ("DEC_INTEGER", lex)
  | otherwise = Nothing

scanKeyword :: String -> Maybe (String, String)
scanKeyword = scanWhitelist keywords "KEYWORD"

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
  where content = takeWhile (`elem` whitespaceCharacters) string

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
