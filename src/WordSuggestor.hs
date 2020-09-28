module WordSuggestor where

import Data.Function (on)
import Data.List (maximumBy)


--NOTE: This is my own naive implementation of a word suggestor. To find a better algorithm, check out
--      my soundex implemenation at Soundex.hs

--The searchFor function is the main function of this file. Given a list of words and an input, 
--it will output the most likely word matches even if there are spelling errors and the word
--isn't complete.



dictionary :: [String]
dictionary = ["hello", "heller", "there", "thorough", "though", "general", "kenobi" ]


--evaluate the value of each word match, the pick the word of all items with the maximum value
searchFor :: [String] -> String -> [String]
dict `searchFor` word = allHighestValues
    where
        values = map (evaluateWord word) dict
        highestValue = maximum (map snd values)
        allHighestValues = map fst (filter (\(_, v) -> v == highestValue) values)

--estimate the value of the word match
evaluateWord :: (Num a, Ord a) => String -> String -> (String, a)
evaluateWord input target = (target, eval input target 0)
    where 
        eval :: (Num a, Ord a) => String -> String -> a -> a
        eval (x:xs) (y:ys) v
            | x == y    = eval xs ys (v + 1) 
            | otherwise = failEval (x:xs) (y:ys) v
        eval [] ys v = v
        eval xs [] v = v

        --if a match failed, check if the input skipped a letter, added a random letter, or just got a letter wrong
        failEval :: (Num a, Ord a) => String -> String -> a -> a
        failEval (x:xs) (y:ys) v = maximum [eval xs (y:ys) v, eval (x:xs) ys v, eval xs ys v]
        failEval xs ys v = eval xs ys v --if one string is empty pass back to eval
