module ReversePolishCalculator where

solveRPN :: (Read a, Num a) => String -> a
solveRPN = process [] . words

process :: (Num a, Read a) => [a] -> [String] -> a
process [y] [] = y
process (y1:y2:ys) ("+":xs) = process ((y2 + y1):ys) xs 
process (y1:y2:ys) ("-":xs) = process ((y2 - y1):ys) xs 
process (y1:y2:ys) ("*":xs) = process ((y2 * y1):ys) xs
--process val ("/":xs) (y1:y2:ys) = process (val + y2 / y1) xs ys
process ys (x:xs) = process (read x:ys) xs
