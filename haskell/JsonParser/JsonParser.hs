import System.Environment (getArgs)
import Data.Char (isDigit, isAlpha)

-- TODO:
-- \", 
-- \t, 
-- \r, 
-- \n
-- \\
-- \uXXXX
-- xeX

------------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------------

data Parser = Parser {
    file :: String,
    str :: String,
    line :: Int,
    col :: Int,
    json :: Json
}

type Json = [JsonTuple]
type JsonTuple = (String, JsonValue)
type JsonArray = [JsonValue]

data JsonValue = Null
    | Boolean Bool
    | Number Double
    | String String
    | Array JsonArray
    | Object Json
    deriving Eq

instance Show JsonValue where
    show Null = "null"
    show (Boolean x) = show x
    show (Number x) = show x
    show (String x) = show x
    show (Array x) = show x
    show (Object x) = show x

------------------------------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------------------------------

parseError :: Parser -> a
parseError p = 
    error $ "<Json>: " ++ show (line p) ++ ":" ++ show (col p) ++ 
            ": Invalid input: couldn't parse token `" ++ show (head' p) ++ "`"

missingTokenError :: Char -> Parser -> a
missingTokenError tk p = 
    error $ "<Json>: " ++ show (line p) ++ ":" ++ show (col p) ++
            ": Invalid input: missing token `" ++ [tk] ++ "`"

expectError :: String -> Parser -> a
expectError tk p = 
    error $ "<Json>: "++ show (line p) ++ ":" ++ show (col p)
            ++ ":\nInvalid input: Expected token `" ++ 
            tk ++ "` but got `" ++ [head' p] ++ "` instead"

unexpectedTokenError :: String -> Parser -> a
unexpectedTokenError tk p = 
    error $ "<Json>: " ++ show (line p) ++ ":" ++ show (col p) 
            ++ ": Unexpected token: `" ++ tk ++ "`"
                    
emptyStringError :: a
emptyStringError = error "<Json>: Invalid input: empty string"

------------------------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------------------------

isNumber :: Char -> Bool
isNumber x = isDigit x || x == '-' || x == '.'

trimLeft :: Parser -> Parser
trimLeft p
    | null' p = p
    | otherwise = let x = head' p in  
        if x == ' ' || x == '\n' || x == '\r' || x == '\t' then
            trimLeft (move p)
        else
            p

readWhile :: (Char -> Bool) -> Parser -> (String, Parser)
readWhile f p = (reverse s, p')
    where
        (s, p') = go [] p
        go acc p
            | null' p = (acc, p)
            | f (head' p) = go (head' p:acc) (move p)
            | otherwise = (acc, p)

null' :: Parser -> Bool
null' p = null (str p)

head' :: Parser -> Char
head' = head . str

tail' :: Parser -> String
tail' = tail . str

moveColumn :: Parser -> Parser
moveColumn p
    | null' p = p
    | otherwise = 
        Parser { 
            file = file p,
            str = tail' p,
            line = line p,
            col = col p + 1,
            json = json p
        }

moveLine :: Parser -> Parser
moveLine p
    | null' p = p
    | otherwise =
        Parser {
            file = file p,
            str = tail' p,
            line = line p + 1,
            col = 1,
            json = json p
        }

move :: Parser -> Parser
move p
    | head' p == '\n' = moveLine p
    | otherwise = moveColumn p

------------------------------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------------------------------

initParser :: String -> Parser
initParser s = Parser { file = "", str = s, col = 1, line = 1, json = [] }

expectToken :: String -> Parser -> Parser 
expectToken t = go t t . trimLeft
    where
        go _ [] p = p
        go tk (t:ts) p
            | head' p == t = go tk ts (move p)
            | otherwise = expectError tk p

parseNumber :: Parser -> (Double, Parser)
parseNumber p
    | null' p = emptyStringError
    | otherwise = let (t, s') = readWhile isNumber p in (read t, s')

parseString :: Parser -> (String, Parser)
parseString p
    | null' p = emptyStringError
    | otherwise = 
        let p' = expectToken "\"" p
            (name, p'') = readWhile (/='"') p'
        in  (name, expectToken "\"" p'')

parseBoolean :: Parser -> (Bool, Parser)
parseBoolean p
    | null' p = emptyStringError
    | otherwise = do 
        let (tk, p') = readWhile isAlpha p
        if tk == "true" then
            (True, p')
        else if tk == "false" then
            (False, p')
        else
            unexpectedTokenError tk p

parseNull :: Parser -> (JsonValue, Parser)
parseNull p
    | null' p = emptyStringError
    | otherwise = do
        let (tk, p') = readWhile isAlpha p
        if tk == "null" then
            (Null, p')
        else
            unexpectedTokenError tk p

parseArray :: Parser -> (JsonArray, Parser)
parseArray = parseMultipleValues ('[', ']') parseValue

parseObject :: Parser -> (Json, Parser)
parseObject = parseMultipleValues ('{', '}') parseTuple

parseTuple :: Parser -> (JsonTuple, Parser)
parseTuple p =
    let (name, p') = parseString p
        (value, p'') = parseValue $ expectToken ":" p'
    in  ((name, value), p'')

parseValue :: Parser -> (JsonValue, Parser)
parseValue = go . trimLeft
    where
        go p
            | null' p = emptyStringError
            | head' p == '\"' = let (v, p') = parseString p in (String v, p')
            | isNumber (head' p) = let (v, p') = parseNumber p in (Number v, p')
            | head' p == 'f' || head' p == 't' = let (v, p') = parseBoolean p in (Boolean v, p')
            | head' p == 'n' = parseNull p
            | head' p == '[' = let (v, p') = parseArray p in (Array v, p')
            | head' p == '{' = let (v, p') = parseObject p in (Object v, p')
            | otherwise = parseError p

parseMultipleValues :: (Char, Char) -> (Parser -> (a, Parser)) -> Parser -> ([a], Parser)
parseMultipleValues (i, j) f = go . expectToken [i]
    where
        go p
            | null' p = emptyStringError
            | otherwise = do
                let (t, p') = f p
                    p'' = trimLeft p'

                if null' p'' then
                    missingTokenError j p''
                else if head' p'' == ',' then
                    let (ts, p''') = go (move p'')
                    in  (t:ts, p''')
                else
                    let p''' = expectToken [j] p''
                    in  seq p''' ([t], p''')

parseJson :: String -> Json
parseJson = fst . parseObject . initParser

------------------------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------------------------

main :: IO ()
main = getContents >>= (mapM_  print . parseJson)
