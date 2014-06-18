module RequestHandler
( handleRequest
) where

import System.Environment as Env
import Data.Maybe
import Data.Char
import qualified Data.HashMap.Lazy as Map
import Text.ParserCombinators.Parsec
import Data.DList hiding (foldr, map)
import Control.Monad.Trans.State.Lazy

import HelperFunctions
import PageTypes

parseOptions :: String -> [(String, String)]
parseOptions = map (breakDrop (== '=')) . breakDropAll (== '&') 

servePage :: Request -> [Handler] -> IO ()
servePage request handlers =
    servePageHelper (requestUrl request) handlers
    where
        servePageHelper :: String -> [Handler] -> IO ()
        servePageHelper url [] =
            if url == "/404/"
                then putStrLn "Content-Type: text/plain\n\nUnhandled 404"
                else servePageHelper "/404/" handlers
        servePageHelper url (h:hs) =
            case parse (fst h) "url" url of
                Right urlOptions -> (putStr . buildResponseString) =<< endpoint urlOptions request
                Left _ -> servePageHelper url hs
            where
                endpoint = (snd h)

buildResponseString :: Response -> String
buildResponseString response =
    "HTTP/1.1 " ++ (show (responseStatus response)) ++ "\n" ++
    "Content-Type: " ++ (responseContentType response) ++ "\n" ++
    "\n" ++
    (responseBody response)

buildRequest :: IO Request
buildRequest = do
    env <- Env.getEnvironment
    let url = map toLower $ takeWhile (/= '?') $ fromMaybe "/404/" $ searchDict "REQUEST_URI" env
    let method = fromMaybe "" $ searchDict "REQUEST_METHOD" env
    let query = fromMaybe "" $ searchDict "QUERY_STRING" env
    let options = parseOptions query
    return Request { requestUrl = url
                   , requestMethod = method
                   , requestQueryParams = Map.fromList options
                   }

handleRequest :: [Handler] -> IO ()
handleRequest handlers = do
    request <- buildRequest
    servePage request handlers
