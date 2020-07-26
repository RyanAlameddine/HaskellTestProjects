module Main where

import Lib

main :: IO ()
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

respondPalindromes = 
    unlines.map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") .lines
    where   isPalindrome xs = xs == reverse xs  