module Blog where

import Data.Maybe
import Data.Char
import Control.Monad

import PageTypes
import PageStructure

blogIndex :: [Option] -> Html
blogIndex options =
    mainLayout head content
  where
    head = tag "title" [] $ text "Blog"
    content :: Html
    content = do
        tag "h1" [] $ text "Blog Index"
        tag "p" [] $ text $ "Under construction."
        mapM_ (\(x, y) -> tag "p" [] $ link ("/blog/" ++ slugify x) x) $ take 5 blogs

blogLayout head content = mainLayout head content

renderBlog blogName content options =
    blogLayout head (content options)
  where
    head = tag "title" [] $ text "Blog"

firstBlog options = text "asdfg"

slugify str = catMaybes $ map slugifyChar str
  where
    slugifyChar c
        | c == ' ' = Just '-'
        | c `elem` allowedChars = Just (toLower c)
        | otherwise = Nothing
    allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"

blogs =
    [("First Blog", firstBlog)
    ]

handlers =
    [(exactly "/blog/", blogIndex)] ++ (map (\(x, y) -> (exactly ("/blog/" ++ (slugify x) ++ "/"), renderBlog x y)) blogs)
