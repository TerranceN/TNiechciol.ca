module Main where

import PageStructure
import Blog

main = runBlog blog

blog = do
    tag "p" [] $ text "Hello, world!"
