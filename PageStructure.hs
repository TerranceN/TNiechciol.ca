module PageStructure
( link
, linkNewTab
, ulist
, button
, navBar
, script
, stylesheet
, mainPage
, mainLayout
, exactly
, image
, screenshot
, youtube_video
, projectSection
, title
, module PageTypes
, module Text.ParserCombinators.Parsec
) where

import Data.Char
import qualified Data.HashMap.Lazy as Map
import Text.ParserCombinators.Parsec

import PageTypes

title :: String -> Html
title str = tag "title" [] $ text (str ++ " | eat.sleep.build")

link :: String -> String -> Html
link url "" = link url url
link url string = do
    tag "a" [("href", url)] $ text string

linkNewTab :: String -> String -> Html
linkNewTab url "" = link url url
linkNewTab url string = do
    tag "a" [("href", url), ("target", "_blank")] $ text string

ulist items = do
    tag "ul" [] $ mapM_ (\x -> tag "li" [] x) items

button :: String -> String -> Html
button url "" = button url url
button url string = do
    tag "a" [("href", url), ("class", "button")] $ text string

script :: String -> Html
script url = do
    tag "script" [("rel", "stylesheet")
                 ,("type", "text/javascript")
                 ,("src", url)] noHtml

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
                tag "div" [] $ link "/" "Home"
            tag "li" [] $ do
                tag "div" [] $ link "/Projects/" "Projects"
                tag "ul" [] $ do
                    tag "li" [] $ link "/Projects/GeoWarsClone/" "Geometry Wars Clone"
                    tag "li" [] $ link "/Projects/ATAHackathonGame/" "ATA Hackathon Game"
                    tag "li" [] $ link "/Projects/DeferedRenderer/" "Defered Renderer"
                    tag "li" [] $ link "/Projects/GeoWarsCloneOld/" "Geometry Wars Clone (old)"
                    tag "li" [] $ link "/Projects/LightingDemo/" "2D Lighting Demo"
                    tag "li" [] $ link "/Projects/ParadoxTower/" "Paradox Tower"
                    tag "li" [] $ link "/Projects/SpringPhysics/" "Spring Physics Demo"
            tag "li" [] $ do
                tag "div" [] $ link "/Resume/" "Resume"

meta :: Html
meta = do
    tag "meta" [ ("name", "keywords")
               , ("content", "Terrance Terry Niechciol")] noHtml
    tag "meta" [ ("name", "description")
               , ("content", "My personal website where you can find my Resume and some small projects I've made for fun.")] noHtml

mainPage :: Html -> Html -> Html
mainPage head content = do
    uText "<!--  \"Terrance is a state of mind, he isn't a person.\" -Awn  -->"
    tag "html" [] $ do
        tag "head" [] $ do
            meta
            stylesheet "/styles/main.css"
            stylesheet "http://fonts.googleapis.com/css?family=Source+Sans+Pro"
            head
        tag "body" [("background", "/images/confectionary.jpg")] $ do
            tag "div" [("id", "wrapper")] $ do
                tag "div" [("id", "navSpacing")] noHtml
                tag "div" [("id", "pageContent"), ("class", "textsection")] content
                tag "div" [("id", "footer")] $ do
                    tag "div" [("id", "footseperator")] noHtml
                    tag "div" [("class", "textsection")] $ do
                        tag "p" [] $ do
                            text "© Terrance Niechciol "
                            text "| Background obtained from "
                            link "http://subtlepatterns.com/" "SubtlePatterns.com"
            navBar

mainLayout :: Html -> Html -> IO Response
mainLayout head content = do
    httpResponse 200 $ mainPage head content

projectSection name body =
    tag "div" [("class", "section " ++ (map toLower name))] $ do
        tag "h2" [] $ text name
        tag "div" [("class", "section_body")] $ body

youtube_video id width height =
    tag "div" [("class", "youtube_video")] $ do
        tag "div" [("class", "youtube_video_border")] $ do
            tag "iframe" [("src", "http://www.youtube.com/embed/" ++ id)
                         ,("width", show width)
                         ,("height", show height)
                         ,("frameborder", "0")
                         ,("allowfullscreen", "true")] noHtml

image url alt = 
    tag "img" [("src", url)
              ,("alt", alt)
              ,("title", alt)] noHtml

screenshot url alt =
    tag "div" [("class", "screenshot_container")] $ do
      tag "a" [("href", "javascript:void(0)"), ("class", "screenshot")] $ image url alt

exactly :: String -> Parser (Map.HashMap String String)
exactly str = (mapM char str) >>= (\x -> eof >> return (Map.fromList [("url", x)]))
