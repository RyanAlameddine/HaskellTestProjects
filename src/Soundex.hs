module Soundex where

--Soundex algorithm, use the searchFor method to find matches

import Data.Char (toLower)

dictionary :: [SdxCode]
dictionary = ["ryan", "christine", "anna", "emily", "essam", "sofia", "paloma", "camillo" ]

type SdxCode = String
type Term = String

--pick all the soundex code matches
searchFor :: [SdxCode] -> Term -> [Term]
searchFor d word = map fst $ filter filterFunc dict
    where
        filterFunc x = snd x == snd (soundex lowerWord)
        dict = map (soundex . map toLower) d
        lowerWord = map toLower word


soundex :: Term -> (Term, SdxCode)
soundex (x:xs) = (x:xs, reverse $ sdx False xs [x])

sdx :: Bool -> Term -> SdxCode -> SdxCode 
sdx _ [] s = (take 4 . (++ repeat '0')) . reverse $ s
sdx b (x:xs) s
    | letterCheck "bfpv"     x = sdxAddChar '1'
    | letterCheck "cgjkqsxz" x = sdxAddChar '2'
    | letterCheck "dt"       x = sdxAddChar '3'
    | letterCheck "l"        x = sdxAddChar '4'
    | letterCheck "mn"       x = sdxAddChar '5'
    | letterCheck "r"        x = sdxAddChar '6'
    | otherwise = sdx True xs s
    where
        sdxAddChar :: Char -> SdxCode
        sdxAddChar c = sdx False xs (if head s == c then s else c:s)
            
letterCheck :: [Char] -> Char -> Bool
letterCheck letters = (`elem` letters)