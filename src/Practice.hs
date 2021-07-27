{-# LANGUAGE ScopedTypeVariables #-}

module Practice where

import           Data.Char
import           Data.List
import qualified Data.Map      as Map
import           System.Random

import Data.Function ( on )
--Practice code inspired by Learn You A Haskell for Great Good
--This is simply my attempt to learn haskell, not an actual project of any sorts

doubleMe x = x + x

length' :: (Num b) => [a] -> b
length' []     = 0
length' (_:xs) = 1 + length' xs

calculateBMIs weightHeightPair = [weight / height | (weight, height) <- weightHeightPair]

maximum' :: (Ord a) => [a] -> a
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

--standard implementation
reverse'' :: [a] -> [a]
reverse'' l =  rev l []
  where
    rev []     ys = ys
    rev (x:xs) ys = rev xs (x:ys)

repeat' :: a -> [a]
repeat' x = x : repeat' x

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection [] ys = []
intersection (x:xs) ys
    | el x ys && notEl x ts = x:ts
    | otherwise = ts
    where
        ts = intersection xs ys

el :: Eq t => t -> [t] -> Bool
el x [] = False
el x (y:ys)
    | x == y = True
    | otherwise = el x ys

notEl :: Eq t => t -> [t] -> Bool
notEl x ys = not (el x ys)

initials :: String -> String -> String
initials (f:fs) (l:ls) = [f, '.', l, '.']

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

replicate' :: (Integral i) => i -> a -> [a]
replicate' 0 _     = []
replicate' count x = x : replicate' (count - 1) x

take' :: (Integral i) => i -> [a] -> [a]
take' _ [] = []
take' count (x:xs)
    | count <= 0 = []
    | otherwise  = x:take' (count - 1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (head:list) = smaller ++ [head] ++ greater
    where
        smaller = quicksort [x | x <- list, x < head]
        greater = quicksort [x | x <- list, x >= head]

mergesort :: (Ord a) => [a] -> [a]
mergesort [x] = [x]
mergesort xs = combine (mergesort (take h xs)) (mergesort (drop h xs))
    where
        h = length xs `div` 2
        combine [] ys = ys
        combine xs [] = xs
        combine (x:xs) (y:ys)
            | x < y     = x:combine xs (y:ys)
            | otherwise = y:combine (x:xs) ys



isUpperAlphanum = (`elem` ['A'..'Z'])
isUpperAlphanum' = (`elem` ['A' .. 'Z'])

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' f (x:xs)
    | f x       = x : filter' f xs
    | otherwise = filter' f xs

--map'' :: (a -> b) -> [a] -> [b]
--map'' _ [] = []
--map'' f xs = foldr (\x acc -> (f x) : acc) [] xs

--modules chapter start

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

intersperse' :: a -> [a] -> [a]
intersperse' _ []     = []
intersperse' y (x:xs) = x : y : intersperse' y xs

intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ []        = []
intercalate' xs (ys:yss) = xs ++ ys ++ intercalate' xs yss

ceasarCypherE :: Int -> String -> String
ceasarCypherE offset = map (chr . (+ offset) . ord)

ceasarCypherD :: Int -> String -> String
ceasarCypherD = ceasarCypherE . negate

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing

-- typeclasses chapter start:

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surfaceArea :: Shape -> Float
surfaceArea (Circle _ r) = pi * r ^ 2
surfaceArea (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) * abs (y1 - y2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dX dY = Circle (Point (x + dX) (y + dY)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dX dY = Rectangle (Point (x1 + dX) (y1 + dY)) (Point (x2 + dX) (y2 + dY))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     } deriving (Eq, Show, Read)

-- >>> read "Just 't'" :: Maybe Char
-- Just 't'

data Vector t = Vector t t t deriving(Show, Read, Eq)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vMult` m = Vector (i*m) (j*m) (k*m)

vDot :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `vDot` (Vector l m n) = i*l + j*m + k*n

-- type String = [Char]

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- >>> 3 :-: (5 :-: Empty)
-- 3 :-: (5 :-: Empty)
--

infixr 5  +-+
(+-+) :: [a] -> [a] -> [a]
[]     +-+ ys = ys
(x:xs) +-+ ys = x : (xs +-+ ys)

data BST a = EmptyNode | Node a (BST a) (BST a) deriving (Show, Read, Eq)

singleton :: a -> BST a
singleton x = Node x EmptyNode EmptyNode

treeInsert :: (Ord a) => a -> BST a -> BST a
treeInsert x EmptyNode = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> BST a -> Bool
treeElem x EmptyNode = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

instance Functor BST where
    fmap f EmptyNode = EmptyNode
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)



treeSym :: BST a -> Bool
treeSym EmptyNode = True
treeSym (Node _ l r) = sym l r 

sym :: BST a -> BST a -> Bool
sym EmptyNode EmptyNode = True
sym (Node _ l1 r1) (Node _ l2 r2) 
    | sym l1 r2 && sym r1 l2 = True
sym _ _ = False

-- checkValid :: (Ord a, Bounded a) => BST a -> Bool
-- checkValid bst = checkHelper bst minBound maxBound

-- checkHelper :: (Ord a) => BST a -> a -> a -> Bool
-- checkHelper EmptyNode _ _ = True
-- checkHelper (Node value leftChild rightChild) lower upper = check (<=) leftChild lower value && check (>) rightChild value upper
--     where
--         check condition EmptyNode _ _= True
--         check condition node@(Node childValue _ _) childLower childUpper = conditionMet && noParentalOverstep && childrenValid
--             where
--                 conditionMet = condition childValue value
--                 noParentalOverstep = childValue > childLower && childValue <= childUpper
--                 childrenValid = checkHelper node childLower childUpper

checkValid :: (Ord a, Bounded a) => BST a -> Bool
checkValid bst = checkHelper bst minBound maxBound

checkHelper :: (Ord a) => BST a -> a -> a -> Bool
checkHelper EmptyNode _ _ = True
checkHelper (Node value leftChild rightChild) lower upper = check (<=) leftChild lower value && check (>) rightChild value upper
    where
        check condition EmptyNode _ _= True
        check condition node@(Node childValue _ _) childLower childUpper = conditionMet && noParentalOverstep && childrenValid
            where
                conditionMet = condition childValue value
                noParentalOverstep = childValue > childLower && childValue <= childUpper
                childrenValid = checkHelper node childLower childUpper

infinode :: Int -> BST Int
infinode i = Node 0 (infinode $ i - 1) EmptyNode 

-- >>> foldr treeInsert EmptyNode [8,6,4,1,7,3,5]
-- Node 5 (Node 3 (Node 1 EmptyNode EmptyNode) (Node 4 EmptyNode EmptyNode)) (Node 7 (Node 6 EmptyNode EmptyNode) (Node 8 EmptyNode EmptyNode))
--

class Eq' a where
    (=-=) :: a -> a -> Bool
    (/-=) :: a -> a -> Bool
    x =-= y = not (x /-= y)
    x /-= y = not (x =-= y)

data TrafficLight = Red | Yellow | Green

instance Eq' TrafficLight where
    Red =-= Red = True
    Green =-= Green = True
    Yellow =-= Yellow = True
    _ =-= _ = False

instance Show TrafficLight where
    show Red    = "Red light"
    show Yellow = "Yellow light"
    show Green  = "Green light"

-- >>> [Red, Yellow, Green]
-- [Red light,Yellow light,Green light]

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = (firstCoin, secondCoin, thirdCoin)
    where
        (firstCoin , newGen)  = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin , _)       = random newGen'


-- instance (Monoid w) => Monad (Writer w) where  
--     return x = Writer (x, mempty)  
--     (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')  

-- >>> runWriter (return 3 :: Writer String Int) 
-- (3,"")   
-- >>> runWriter (return 3 :: Writer (Sum Int) Int)  
-- (3,Sum {getSum = 0}) 
-- >>> runWriter (return 3 :: Writer (Product Int) Int)
-- (3,Product {getProduct = 1})  

gcf :: Int -> Int -> Int  
gcf a b   
    | b == 0    = a  
    | otherwise = gcf b (a `mod` b)  