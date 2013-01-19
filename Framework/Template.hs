module Template
( template
) where

import PageTypes
import Data.Maybe
import HelperFunctions

template :: String -> [(String, String)] -> IO String
template file options = do
    f <- readFile file
    return $ processFile options f

processFile :: [(String, String)] -> String -> String
processFile options = unlines . (map (processLine options)) . lines

processLine :: [(String, String)] -> String -> String
processLine = replaceVariables

replaceVariables :: [(String, String)] -> String -> String
replaceVariables options line =
    if foundStart
        then if foundEnd
            then start ++ (fromMaybe "undefined" $ searchDict name options) ++ continue
            else line
        else line
    where
        beginSearch = findInString "{{" line
        endSearch = findInString "}}" $ snd beginSearch
        start = fst beginSearch
        name = fst endSearch
        foundStart = fst beginSearch /= line
        foundEnd = fst endSearch /= snd beginSearch
        continue = replaceVariables options (snd endSearch)

--              search -> source -> (before, after)
findInString :: String -> String -> (String, String)
findInString _ [] = ([], [])
findInString [] str = ([], str)
findInString (x:xs) (y:ys) =
    if x == y
        then if fst res
            then ([], snd res)
            else continue
        else continue
    where
        continue = (y:(fst recurse), snd recurse)
        recurse = findInString (x:xs) ys
        res = keepSearching xs ys
        keepSearching [] [] = (True, [])
        keepSearching _ [] = (False, [])
        keepSearching [] str = (True, str)
        keepSearching (x:xs) (y:ys) =
            if x == y
                then keepSearching xs ys
                else (False, [])
