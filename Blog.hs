module Blog where

import System.Info
import System.Process
import System.Locale
import Data.Time
import Control.Monad
import Control.Monad.Trans
import Data.DList hiding (map)
import Control.Monad.Trans.State.Lazy

import Text.ParserCombinators.Parsec

import PageTypes
import PageStructure
import HelperFunctions

runBlog :: Html -> IO ()
runBlog blog = putStr . toList =<< execStateT blog (fromList "")

slash =
    if os == "mingw32"
        then '\\'
        else '/'

toPath str = map convert str
  where
    convert '/' = slash
    convert '\\' = slash
    convert c = c

blogDir :: String -> String
blogDir blogName = (toPath $ "Blog/" ++ (slugify blogName) ++ "/")

loadBlogContent :: String -> IO String
loadBlogContent blogName = do
    (exit, stdout, stderr) <- readProcessWithExitCode ((blogDir blogName) ++ "Main") [] ""
    return stdout

data BlogInformation = BlogName String

parseBlogName :: Parser BlogInformation
parseBlogName = do
    string "Name:"
    skipMany space
    name <- many1 (noneOf "\n")
    return $ BlogName name

parseBlogInformation :: Parser [BlogInformation]
parseBlogInformation = do
    spaces
    result <- mapM (\x -> x >>= \info -> spaces >> return info) [parseBlogName]
    eof
    return result

compileBlogInformation :: [BlogInformation] -> String
compileBlogInformation [] = ""
compileBlogInformation (b:bs) =
    case b of
        BlogName n -> n
        _ -> compileBlogInformation bs

loadBlogInformation :: String -> IO String
loadBlogInformation blogName = do
    fileString <- readFile ((blogDir blogName) ++ "BlogInfo.txt")
    case parse parseBlogInformation "blogInfo files" fileString of
        Left err -> return ""
        Right info -> return $ compileBlogInformation info

renderBlog blogName urlOptions request = mainLayout head body
  where
    head = tag "title" [] $ text "Blog"
    body = do
        content <- lift $ loadBlogContent blogName
        realName <- lift (loadBlogInformation blogName)
        tag "h1" [] $ text realName
        uText content
        time <- lift $ do
            time <- getZonedTime
            return $ formatTime defaultTimeLocale "%m/%d/%Y %I:%M %p" time
        tag "p" [] $ text ("Last modified: " ++ time)
