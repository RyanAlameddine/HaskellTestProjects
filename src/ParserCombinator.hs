module ParserCombinator where

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

-- function from input to Either ParserError or (rest of input, value)
newtype Parser a = Parser { runParser :: Input -> Either ParseError (Input, a) }

data ParseError = ParseError Int String deriving (Show)
data Input      = Input      Int String deriving (Show)
   
instance Functor Parser where
    fmap f (Parser p) = 
        let maybeMap maybeVal = fmap (second f) maybeVal 
        in Parser $ maybeMap . p

instance Applicative Parser where
    pure x = Parser $ \input -> Right (input, x)
    (Parser f) <*> (Parser a) = Parser $ \input -> do
                            (input', func) <- f input 
                            (input'', val) <- a input'
                            Right (input'', func val)

instance Alternative Parser where
    empty = Parser $ const $ Left $ ParseError 0 "empty"
    (Parser first) <|> (Parser second) = Parser $ \input -> first input <|> second input

instance Alternative Either ParseError where
    empty = Left $ ParseError 0 "empty"
    x <|> r = r


-- >>> :t runParser
-- runParser :: Parser a -> String -> Maybe (String, a)

-- takes in a character and returns a parser which parses that character
char :: Char -> Parser Char
char x = Parser f
    where 
        f :: Input -> Either ParseError (Input, Char)
        f (Input c (y:ys))
            | y == x = Right $ (Input (c + 1) ys, x)
        f (Input c _) = Left $ ParseError c ("Could not find char '" ++ [x] ++ "'")

-- takes in a string and returns a parser which parses that string
string :: String -> Parser String
string = traverse char --string = sequenceA . map char

digit :: Parser Char
digit = anyCharOf ['0'..'9']

int :: Parser Integer
int = read <$> some digit

choice :: [Parser a] -> Parser a
choice = foldl1 (<|>)

between :: Parser a -> Parser b -> Parser c -> Parser b
between a b c = a *> b <* c

anyCharOf :: [Char] -> Parser Char
anyCharOf chars = choice (map char chars)

separateBy :: Parser a -> Parser b -> Parser [b]
separateBy separator value = (:) <$> value <*> many (separator *> value) <|> pure []
