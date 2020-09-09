module Main where

import Lib
import System.Environment
import System.IO  
import Data.List  
--import System.Directory

todoMain = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  

add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks 

remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    --removeFile fileName  
    --renameFile tempName fileName  

main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  

seqMain = do  
    rs <- sequence [getLine, getLine, getLine]
    print rs

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

respondPalindromes = 
    unlines.map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") .lines
    where   isPalindrome xs = xs == reverse xs  