module Blog where

import System.Info
import System.Process
import Data.Maybe
import Data.Char
import Control.Monad
import Control.Monad.Trans
import Data.DList hiding (map)
import Control.Monad.Trans.State.Lazy

import PageTypes
import PageStructure

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

loadBlogContent :: String -> IO (Int, Int, String)
loadBlogContent blogName = do
    (exit, stdout, stderr) <- readProcessWithExitCode (toPath $ "Blog/" ++ (slugify blogName) ++ "/Main") [] ""
    return (0, 0, stdout)

renderBlog :: String -> [Option] -> [Option] -> Html
renderBlog blogName urlOptions queryOptions = mainLayout head body
  where
    head = tag "title" [] $ text "Blog"
    body = do
        (dateWritten, dateModified, content) <- lift $ loadBlogContent blogName
        tag "h1" [] $ text blogName
        uText content
        tag "p" [] $ do
            text (show dateWritten)
        if (dateWritten == dateModified)
            then return ()
            else tag "p" [] $ text (show dateModified)

slugify str = catMaybes $ map slugifyChar str
  where
    slugifyChar c
        | c == ' ' = Just '-'
        | c `elem` allowedChars = Just (toLower c)
        | otherwise = Nothing
    allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"
