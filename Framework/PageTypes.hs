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
, Request(..)
, Response(..)
, httpResponse
) where

import Text.ParserCombinators.Parsec
import Data.DList hiding (foldr)
import Data.Char
import qualified Data.HashMap.Lazy as Map
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy

data Request = Request { requestUrl :: String
                       , requestMethod :: String
                       , requestQueryParams :: Map.HashMap String String
                       }

data Response = Response { responseStatus :: Int
                         , responseContentType :: String
                         , responseBody :: String
                         }

type Option = (String, String)

type Html = StateT (DList Char) IO ()
type Page = Request -> IO Response
type Handler = (Parser (Map.HashMap String String), Endpoint)
type Endpoint = Map.HashMap String String -> Page

httpResponse :: Int -> Html -> IO Response
httpResponse statusCode html = do
    content <- execStateT html (fromList "")
    return Response { responseStatus = statusCode
                    , responseContentType = "text/html"
                    , responseBody = toList $ (fromList "<!DOCTYPE html>\n") `append` content
                    }

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
