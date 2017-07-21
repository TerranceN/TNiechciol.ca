module Blog.CanvasDemo
( canvasDemoWithClass
, canvasDemo
, canvasDemoInteractive
, canvasDemoCanvasNoLink
, canvasDemoCanvas
) where

import PageTypes
import PageStructure

canvasDemoWithClass cl description contents = do
  tag "div" [("class", "canvas_demo " ++ cl)] $ do
    tag "div" [("class", "demo_contents")] $ do
      tag "div" [("class", "demo_contents_cell")] $ do
        contents
    tag "p" [] $ do
      tag "i" [] $ do
        description

canvasDemo description contents =
  canvasDemoWithClass "" description contents

canvasDemoInteractive description contents =
  canvasDemoWithClass "interactive" description contents

canvasDemoCanvasNoLink src =
  tag "div" [] $ do
    tag "canvas" [("src", src)
                 ,("width", "200")
                 ,("height", "200")
                 ] noHtml

canvasDemoCanvas src =
  tag "div" [] $ do
    tag "canvas" [("src", src)
                 ,("width", "200")
                 ,("height", "200")
                 ] noHtml
    tag "a" [("href", src), ("target", "_blank")] $ text "View source"
