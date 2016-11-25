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
import Data.Maybe
import qualified Data.HashMap.Lazy as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Trans

import HelperFunctions
import PageTypes
import GitHash
import Clicky

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

inlineStylesheet :: String -> Html
inlineStylesheet file = do
    contents <- lift $ readFile file
    tag "style" [] $ uText contents

navBar :: String -> Html
navBar selected = do
    tag "div" [("id", "navBar")] $ do
        tag "ul" [] $ mapM_ selectSelected pages
  where
    selectSelected (name, html) =
      if name == selected
        then html [("class", "selected")]
        else html []
    home attrs = 
      tag "li" attrs $ do
          tag "div" [] $ link "/" "Home"
    projects attrs = 
      tag "li" attrs $ do
          tag "div" [] $ link "/Projects/" "Projects"
          tag "ul" [] $ do
              tag "li" [] $ link "/Projects/GeoWarsClone/" "Geometry Wars Clone"
              tag "li" [] $ link "/Projects/ATAHackathonGame/" "ATA Hackathon Game"
              tag "li" [] $ link "/Projects/DeferedRenderer/" "Defered Renderer"
              tag "li" [] $ link "/Projects/GeoWarsCloneOld/" "Geometry Wars Clone (old)"
              tag "li" [] $ link "/Projects/LightingDemo/" "2D Lighting Demo"
              tag "li" [] $ link "/Projects/ParadoxTower/" "Paradox Tower"
              tag "li" [] $ link "/Projects/SpringPhysics/" "Spring Physics Demo"
    resume attrs = 
      tag "li" attrs $ do
          tag "div" [] $ link "/Resume/" "Resume"
    pages = [("home", home)
            ,("projects", projects)
            ,("resume", resume)
            ]

meta :: Html
meta = do
    tag "meta" [ ("name", "keywords")
               , ("content", "Terrance Terry Niechciol")] noHtml
    tag "meta" [ ("name", "description")
               , ("content", "My personal website where you can find my Resume and some small projects I've made for fun.")] noHtml

quotes = [("Terrance is a state of mind, he isn't a person.", "Awn")

         -- Quotes from SF crew 2015
         ,("When I realized becoming an actress wouldn't pan out, I decided my life would be like a movie.", "Luz")
         ,("I'm not here for babies, I'm here for art!", "Philippe")
         ,("Damn, you could break a chair with that ass!", "Ally")
         ,("I really don't think anyone thought I was a lesbian in high school.", "Chelsea")
         ,("Do the trains go on a schedule?", "Sam")
         ,("Sometimes I have a hard time telling the difference between popcorn and carpet.", "Lydia")
         ,("Dude, suitcases are so much easier than women.", "Patrick")

        -- Quotes from SF crew 2016
        ,("Heterosexual sex is the gayest thing in the world.", "Joe")
        ,("Puppy killing is universal.", "Alexis")

         -- Quotes from Vancouver crew
         ,("In this day and age no one uses idioms anymore.", "Liz")
         ,("I'm not rude, I tell it like it is.", "Melissa")
         ,("Movies don't lie, Terrance, they're a real life depiction of real life.", "Flan")

         -- Quotes from Flan's friends
         ,("I'm profoundly dumbfounded.", "Malecki")
         ,("Never have I ever... Wait, I don't know how to play this game.", "Vic")
         ,("Rice just kills everything bad.", "Bogdan")
         ,("I was just talking to a guy over there who would be ideal for beating.", "Golla")
         ,("Spitters are quitters. And momma doesn't raise no quitters.", "Esther")

        -- Quotes from OCC people
         ,("I don't do effort.", "Kristin")
         ,("Go suck face somewhere else.", "Salahub")
         ,("I'm irrationally terrified of fireman poles.", "Tori")
         ,("Coincidentally you know her, and I want to put my penis in her.", "Brendo")
         ,("You were playing with his hair, I was playing with your butt.", "Akhi")

         -- Quotes from other people
         ,("I'd rather rest in a state of mediocrity.", "Kianna")
         ,("I'll never look like anything.", "Christina")
         ,("I'm so tempted to show you my bag of condiments.", "Valerie")

         -- Quotes from profs
         ,("I kinda ran out of gas here... I ran out of stuff. You gotta go home now.", "Computational Audio Prof.")

         -- Quotes from coworkers
         ,("We had to get the social worker involved, because kids were covered in lube.", "Lauren")
         ,("I love that you think you're normal.", "Olga")
         ,("Through physics I just ended up in his crotch.", "Dave")
         ,("He was intellectually masturbating the entire interview, but he just couldn't cum.", "Louie")
         ,("Brendan is a master at massacring girls with his balls.", "Kenshi")
         ,("You suck, and everything you touch sucks. You're like the anti-King Midas.", "Warsaba")
         ]

renderQuote (quote, author) = uText ("\"" ++ quote ++ "\"" ++ " --" ++ author ++ "\n")

mainPage :: Html -> Html -> [(String, String)] -> Html
mainPage head content options = do
    uText "<!\n\nHere's some funny quotes I've gathered over the years (in no particular order):\n"
    mapM_ renderQuote quotes
    uText "\n-->"
    tag "html" [] $ do
        tag "head" [] $ do
            meta
            inlineStylesheet "./styles/main.css"
            stylesheet "http://fonts.googleapis.com/css?family=Raleway|Roboto Condensed"
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
                            text "| Background from "
                            link "http://subtlepatterns.com/" "SubtlePatterns.com"
            navBar $ fromMaybe "" $ searchDict "section" options
            uText clickyTrackingCode

mainLayout :: Html -> Html -> [(String, String)] -> IO Response
mainLayout head content options = do
    httpResponse 200 $ mainPage head content options

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
