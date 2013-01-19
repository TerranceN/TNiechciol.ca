module RequestHandler
( handleRequest
) where

import System.Environment as Env
import Data.Maybe
import Data.Char
import Text.ParserCombinators.Parsec
import Data.DList hiding (foldr, map)
import Control.Monad.Trans.State.Lazy

import HelperFunctions
import PageTypes

parseOptions :: String -> [(String, String)]
parseOptions = map (breakDrop (== '=')) . breakDropAll (== '&') 

servePage :: String -> [Option] -> [Handler] -> IO ()
servePage url getOptions handlers =
    servePageHelper url handlers
    where
        servePageHelper :: String -> [Handler] -> IO ()
        servePageHelper url [] =
            if url == "/404/"
                then putStrLn "Content-Type: text/plain\n\nUnhandled 404"
                else servePageHelper "/404/" handlers
        servePageHelper url (h:hs) =
            case parse (fst h) "url" url of
                Right urlOptions -> (putStr . toList) =<< execStateT (page urlOptions getOptions) (fromList "")
                Left _ -> servePageHelper url hs
            where
                page = (snd h)

handleRequest :: [Handler] -> IO ()
handleRequest handlers = do
    env <- Env.getEnvironment
    query <- return $ fromMaybe "" $ searchDict "QUERY_STRING" env
    options <- return $ parseOptions query
    url <- return $ map toLower $ takeWhile (/= '?') $ fromMaybe "/404/" $ searchDict "REQUEST_URI" env
    servePage url options handlers
