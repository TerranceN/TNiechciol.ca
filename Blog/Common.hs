module Blog.Common
( blogLayout
, btext
, itext
, monospan
, monotext
, monopara
) where

import PageTypes
import PageStructure

blogLayout :: Html -> Html -> String -> IO Response
blogLayout extraHead blogContent titleText = mainLayout head body [("section", "blog")]
  where
    head = do
      title titleText
      stylesheet "/styles/blog.css"
      script "/blog_assets/ExpandCodeBlock.js"
      extraHead
    body = do
      tag "h1" [] $ text titleText
      blogContent


btext str = do
  tag "b" [] $ text str

itext str = do
  tag "i" [] $ text str

monospan contents = do
  tag "code" [("class", "monospace")] contents

monotext str = do
  monospan $ text str

monopara str = do
  tag "p" [("class", "monospace")] $ monotext str
