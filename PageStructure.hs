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
import Control.Monad.Trans

import PageTypes
import GitHash

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
    hashedUrl <- lift $ hashifyUrl url ".js"
    tag "script" [("rel", "stylesheet")
                 ,("type", "text/javascript")
                 ,("src", hashedUrl)] noHtml

stylesheet :: String -> Html
stylesheet url = do
    hashedUrl <- lift $ hashifyUrl url ".css"
    tag "link" [("rel", "stylesheet")
               ,("type", "text/css")
               ,("href", hashedUrl)] noHtml

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

clickyTrackingCode = " \n\
  \<script type=\"text/javascript\"> \n\
  \var clicky_site_ids = clicky_site_ids || []; \n\
  \clicky_site_ids.push(100919464); \n\
  \(function() { \n\
  \  var s = document.createElement('script'); \n\
  \  s.type = 'text/javascript'; \n\
  \  s.async = true; \n\
  \  s.src = '//static.getclicky.com/js'; \n\
  \  ( document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0] ).appendChild( s ); \n\
  \})(); \n\
  \</script> \n\
  \<noscript><p><img alt=\"Clicky\" width=\"1\" height=\"1\" src=\"//in.getclicky.com/100919464ns.gif\" /></p></noscript> \n\
  \"

quotes = [("Terrance is a state of mind, he isn't a person.", "Awn")
         ,("I can't be a productive member of society until I get laid.", "Luz")
         ,("I was just talking to a guy over there who would be ideal for beating.", "Golla")
         ,("I'll never look like anything.", "Christina")
         ,("Rice just kills everything bad.", "Bogdan")
         ,("In this day and age no one uses idioms anymore.", "Liz")
         ,("Movies don't lie, Terrance, they're a real life depiction of real life.", "Flan")
         ,("Never have I ever... Wait, I don't know how to play this game.", "Vic")
         ,("I'd rather rest in a state of mediocrity.", "Kianna")
         ,("Damn, you could break a chair with that ass!", "Ally")
         ,("Sometimes I have a hard time telling the difference between popcorn and carpet.", "Lydia")
         ,("Dude, suitcases are so much easier than women.", "Patrick")
         ,("We had to get the social worker involved, because kids were covered in lube.", "Lauren")
         ,("I love that you think you're normal.", "Olga")
         ,("At this point I feel more like a cougar.", "Melissa")
         ,("He was intellectually masturbating the entire interview, but he just couldn't cum.", "Louie")
         ,("Brendan is a master as massacring girls with his balls.", "Kenshi")
         ]

renderQuote (quote, author) = uText ("\"" ++ quote ++ "\"" ++ " --" ++ author ++ "\n")

mainPage :: Html -> Html -> Html
mainPage head content = do
    uText "<!\n\nHere's some funny quotes I've gathered over the years (in no particular order):\n"
    mapM_ renderQuote quotes
    uText "\n-->"
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
            uText clickyTrackingCode

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
    tag "div" [("class", "image_container loading")] $ do
        tag "img" [("src", url)
                  ,("alt", alt)
                  ,("title", alt)
                  ,("onload", "this.parentNode.classList.remove('loading')")] noHtml

screenshot url alt =
    tag "div" [("class", "screenshot_container")] $ do
      tag "a" [("href", "javascript:void(0)"), ("class", "screenshot")] $ image url alt

exactly :: String -> Parser (Map.HashMap String String)
exactly str = (mapM char str) >>= (\x -> eof >> return (Map.fromList [("url", x)]))
