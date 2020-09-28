module WordSuggestor where


--Soundex algorithm, use the searchFor method to find matches

dictionary = map soundex ["hello", "heller", "there", "thorough", "though", "general", "kenobi" ]

--evaluate the soundex of each word match, the pick the word of all items with the maximum value
searchFor :: [String] -> String -> [String]
dict `searchFor` word = allHighestValues
    where
        values = map (evaluateWord word) dict
        highestValue = maximum (map snd values)
        allHighestValues = map fst (filter (\(_, v) -> v == highestValue) values)

soundex :: String -> (String, String)
