module PageStructure
( link
, navBar
, stylesheet
, mainLayout
, exactly
, screenshot
, youtube_video
, projectLayout
, projectSection
, title
, module PageTypes
, module Text.ParserCombinators.Parsec
) where

import Data.Char
import Text.ParserCombinators.Parsec

import PageTypes

title :: String -> Html
title str = tag "title" [] $ text (str ++ " | TNiechciol.ca")

link :: String -> String -> Html
link url "" = link url url
link url string = do
    tag "a" [("href", url)] $ text string

stylesheet :: String -> Html
stylesheet url = do
    tag "link" [("rel", "stylesheet")
               ,("type", "text/css")
               ,("href", url)] noHtml

navBar :: Html
navBar = do
    tag "div" [("id", "navBar")] $ do
        tag "ul" [] $ do
            tag "li" [] $ do
                link "/" "Home"
            tag "li" [] $ do
                link "/Projects/" "Projects"
                tag "ul" [] $ do
                    tag "li" [] $ link "/Projects/GeoWarsClone/" "Geometry Wars Clone"
                    tag "li" [] $ link "/Projects/LightingDemo/" "2D Lighting Demo"
                    tag "li" [] $ link "/Projects/ParadoxTower/" "Paradox Tower"
                    tag "li" [] $ link "/Projects/SpringPhysics/" "Spring Physics Demo"
            tag "li" [] $ do
                link "/Resume/" "Resume"
            tag "li" [] $ do
                link "/Contact/" "Contact"

mainLayout :: Html -> Html -> Html
mainLayout heads strs = do
    uText "Content-Type: text/html\n"
    uText "\n"
    uText "<!DOCTYPE html>"
    tag "html" [] $ do
        tag "head" [] $ do
            stylesheet "/styles/main.css"
            stylesheet "http://fonts.googleapis.com/css?family=Source+Sans+Pro"
            heads
        tag "body" [("background", "/images/low_contrast_linen.png")] $ do
            tag "div" [("id", "wraper")] $ do
                tag "div" [("id", "navSpacing")] noHtml
                tag "div" [("id", "pageContent"), ("class", "textsection")] strs
                tag "div" [("id", "footer")] $ do
                    tag "div" [("id", "footseperator")] noHtml
                    tag "div" [("class", "textsection")] $ do
                        tag "p" [] $ do
                            text "© Terrance Niechciol "
                            text "| Background obtained from "
                            link "http://subtlepatterns.com/" "SubtlePatterns.com"
            navBar

projectLayout head body =
    mainLayout (customHead >> head) body
  where
    customHead = stylesheet "/styles/project.css"

projectSection name body =
    tag "div" [("class", "section " ++ (map toLower name))] $ do
        tag "h2" [] $ text name
        tag "div" [("class", "section_body")] $ body

youtube_video id width height =
    tag "iframe" [("src", "http://www.youtube.com/embed/" ++ id)
                 ,("width", show width)
                 ,("height", show height)
                 ,("frameborder", "0")
                 ,("allowfullscreen", "true")] noHtml

screenshot url alt =
    tag "img" [("src", url)
              ,("alt", alt)
              ,("title", alt)] noHtml

exactly :: String -> Parser [Option]
exactly str = (mapM char str) >>= (\x -> eof >> return [("url", x)])
