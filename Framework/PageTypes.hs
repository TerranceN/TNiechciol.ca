module PageTypes
( Page
, Html
, Handler
, Option
, tag
, text
, uText
, noHtml
, validHtmlChar
) where

import Text.ParserCombinators.Parsec
import Data.DList hiding (foldr)
import Data.Char
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy

type Html = StateT (DList Char) IO ()
type Page = [Option] -> [Option] -> Html
type Option = (String, String)
type Handler = (Parser [Option], Page)

validHtmlChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.,!@#$%^() "

validHtmlChar :: Parser Char
validHtmlChar = oneOf validHtmlChars

uText :: String -> Html
uText str = modify (\x -> x `append` (fromList str))

text :: String -> Html
text = uText . encode

noHtml :: Html
noHtml = return ()

encode :: String -> String
encode = foldr (\x y -> (encodeChar x) ++ y) ""
  where
    encodeChar :: Char -> String
    encodeChar c =
        if c `elem` validHtmlChars
            then [c]
            else "&#" ++ (show (ord c)) ++ ";"

tag :: String -> [Option] -> Html -> Html
tag name options contents = do
    uText "<" >> uText name >> (mapM renderOption options) >> uText ">"
    contents
    uText "</" >> uText name >> uText ">"
  where
    renderOption (key, value) = do
        uText " "
        uText key
        uText "=\""
        uText value
        uText "\""
