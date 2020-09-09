module JsonParser where
import qualified Data.Map as Map

--parser :: String -> a
--parser :: String -> (String, a)
--parser :: String -> Maybe (String, a)
--parser :: String -> Either String (String, a)
--parser :: String -> Either (Int, Int, String) (String, a)
--type Parser a = String -> Maybe (String, a)

newtype Parser a = Parser 
    { runParser :: String -> Maybe (String, a) }

-- >>> :t runParser
-- runParser :: Parser a -> String -> Maybe (String, a)

-- takes in a character and returns a parser which parses that character
charParser :: Char -> Parser Char
charParser x = Parser f
    where 
        f (y:ys)
            | y == x = Just (ys, x)
        f _ = Nothing

-- takes in a string and returns a parser which parses that string
--stringParser :: String -> Parser String
--stringParser x = sequenceA $ map charParser x







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
jsonNull = undefined

jsonValue :: Parser JsonValue
jsonValue = undefined