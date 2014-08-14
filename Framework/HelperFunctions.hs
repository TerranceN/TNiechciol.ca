module HelperFunctions
( searchDict
, breakDrop
, breakDropAll
, slugify
) where

import Data.Char
import Data.Maybe

searchDict :: String -> [(String, a)] -> Maybe a
searchDict _ [] = Nothing
searchDict key (x:xs)
    | key == fst x = Just $ snd x
    | otherwise = searchDict key xs


breakDrop :: (a -> Bool) -> [a] -> ([a], [a])
breakDrop _ [] = ([], [])
breakDrop p (x:xs) =
    if p x
        then ([], xs)
        else (x:(fst res), (snd res))
    where
        res = breakDrop p xs

breakDropAll :: (a -> Bool) -> [a] -> [[a]]
breakDropAll _ [] = []
breakDropAll p lst =
    if not . null $ snd broken
        then (fst broken):(breakDropAll p (snd broken))
        else [fst broken]
    where
        broken = breakDrop p lst

slugify str = catMaybes $ map slugifyChar str
  where
    slugifyChar c
        | c == ' ' = Just '-'
        | c `elem` allowedChars = Just (toLower c)
        | otherwise = Nothing
    allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"
