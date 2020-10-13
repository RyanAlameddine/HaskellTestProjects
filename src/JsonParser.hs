module JsonParser where
import qualified Data.Map as Map
import Data.Functor ()
import Data.Char ()
import Data.Bifunctor ( Bifunctor(second) )
import Control.Applicative ( Alternative(..) )

--parser :: String -> a
--parser :: String -> (String, a)
--parser :: String -> Maybe (String, a)
--parser :: String -> Either String (String, a)
--parser :: String -> Either (Int, Int, String) (String, a)
--type Parser a = String -> Maybe (String, a)

-- function from input to Maybe (rest of input, value)
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }


-- instance Functor Parser where
--     fmap f (Parser p) = Parser $ \input -> do
--                     (input', x) <- p input
--                     Just (input', f x)     

             
instance Functor Parser where
    fmap f (Parser p) = 
        let maybeMap maybeVal = fmap (second f) maybeVal 
        in Parser $ maybeMap . p

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser f) <*> (Parser a) = Parser $ \input -> do
                            (input', func) <- f input 
                            (input'', val) <- a input'
                            Just (input'', func val)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser first) <|> (Parser second) = Parser $ \input -> first input <|> second input



-- >>> :t runParser
-- runParser :: forall a. Parser a -> String -> Maybe (String, a)

-- takes in a character and returns a parser which parses that character
char :: Char -> Parser Char
char x = Parser f
    where 
        f (y:ys)
            | y == x = Just (ys, x)
        f _ = Nothing

-- takes in a string and returns a parser which parses that string
string :: String -> Parser String
string = sequenceA . map char

digit :: Parser Char
digit = anyCharOf ['0'..'9']

int :: Parser Integer
int = read <$> some digit

-- spanParser :: (Char -> Bool) -> Parser String
-- spanParser f = Parser $ \input -> 
--                 let (token, rest) = span f input 
--                 in Just (rest, token)

-- notEmpty :: Parser [a] -> Parser [a]
-- notEmpty (Parser p) = Parser $ \input -> do
--                         (input', (x:xs)) <- p input
--                         Just (input', (x:xs))

choice :: [Parser a] -> Parser a
choice = foldl1 (<|>)

between :: Parser a -> Parser b -> Parser c -> Parser b
between a b c = a *> b <* c

anyCharOf :: [Char] -> Parser Char
anyCharOf chars = choice (map char chars)

separateBy :: Parser a -> Parser b -> Parser [b]
separateBy separator value = (:) <$> value <*> many (separator *> value) <|> pure []


-- JSON SPECIFIC REGION

data JsonValue 
    = JsonNull
    | JsonBool Bool
    | JsonNumber Integer
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject (Map.Map String JsonValue)
    deriving (Show, Eq)

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ string "null"

jsonBool :: Parser JsonValue
jsonBool = (JsonBool True <$ string "true") <|> (JsonBool False <$ string "false")

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> int

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> between sqrBracketO elements sqrBracketC
    where
        elements = btwnWhite $ separateBy comma jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> between curlyBracketO mappedPairs curlyBracketC
    where
        pairs = (\key _ value -> (key, value)) <$> stringLiteral <*> colon <*> jsonValue
        separatedPairs = separateBy comma pairs
        mappedPairs = Map.fromList <$> separatedPairs


jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject


whitespaceChars :: Parser Char
whitespaceChars = anyCharOf ['\t', '\n', '\r', '\f', '\v']

whitespace :: Parser String
whitespace = many whitespaceChars

whitespace1 :: Parser String
whitespace1 = some whitespaceChars

--parser surrounded by any number of whitespace
btwnWhite :: Parser a -> Parser a
btwnWhite p = between whitespace p whitespace

--parser for char surrounded by any number of whitespace
charBtwnWhite :: Char -> Parser Char
charBtwnWhite c = btwnWhite $ char c

stringLiteral :: Parser String
stringLiteral = between doubleQuote (many stringChar) doubleQuote

stringChar :: Parser Char
stringChar = Parser f
    where
        f (y:ys)
            | y /= '"' = Just (ys, y)
        f _ = Nothing


doubleQuote   = char '"'
curlyBracketO = charBtwnWhite '{'
curlyBracketC = charBtwnWhite '}'
sqrBracketO   = charBtwnWhite '['
sqrBracketC   = charBtwnWhite ']'
colon         = charBtwnWhite ':'
comma         = charBtwnWhite ','