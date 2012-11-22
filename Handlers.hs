module Handlers
( handlers
) where

import Data.Maybe
import System.Environment as Env

import PageTypes
import Template
import HelperFunctions

navBar :: [IO String]
navBar = 
    [tag "div" [("id", "navBar")] [
        tag "ul" [] [
            tag "li" [] [
                tag "a" [("href", "/~tniechci/")] [text "Home"]
            ],
            tag "li" [] [
                tag "a" [("href", "/~tniechci/Projects/")] [text "Projects"],
                tag "ul" [] [
                    tag "li" [] [tag "a" [("href", "/~tniechci/Projects/GeoWarsClone/")] [text "Geometry Wars Clone"]],
                    tag "li" [] [tag "a" [("href", "/~tniechci/Projects/LightingDemo/")] [text "2D Lighting Demo"]],
                    tag "li" [] [tag "a" [("href", "/~tniechci/Projects/ParadoxTower/")] [text "Paradox Tower"]],
                    tag "li" [] [tag "a" [("href", "/~tniechci/Projects/SpringPhysics/")] [text "Spring Physics Demo"]]
                ]
            ],
            tag "li" [] [
                tag "a" [("href", "/~tniechci/Contact/")] [text "Contact"]
            ]
        ]
    ]]

mainLayout :: [IO String] -> [IO String] -> [IO String]
mainLayout heads strs =
    [text "Content-Type: text/html\n"
    ,text "<!DOCTYPE html>"
    ,tag "html" [] [
        tag "head" [] [
            tag "link" [("rel", "stylesheet")
                                ,("type", "text/css")
                                ,("href", "/~tniechci/styles/main.css")] [],
            mergeIOStrings heads
        ],
        tag "body" [("background", "/~tniechci/images/low_contrast_linen.png")] [
            tag "div" [("id", "wraper")] [
                tag "div" [("id", "navSpacing")] [],
                tag "div" [("id", "pageContent"), ("class", "textsection")] strs,
                tag "div" [("id", "footer")] [
                    tag "div" [("id", "footseperator")] [],
                    tag "div" [("class", "textsection")] [
                        tag "p" [] [
                            text "&copy; Terrance Niechciol ",
                            text "| Background obtained from ",
                            tag "a" [("href", "http://subtlepatterns.com/")] [
                                text "SubtlePatterns.com"
                            ]
                        ]
                    ]
                ]
            ],
            mergeIOStrings navBar
        ]
    ]]

projectLayout head body =
    mainLayout (customHead ++ head) body
    where
        customHead = [tag "link" [("rel", "stylesheet")
                                 ,("type", "text/css")
                                 ,("href", "/~tniechci/styles/project.css")] []]

index :: Page
index options =
    mainLayout head content
    where
        head =
            [tag "title" [] [text "Terrance Niechciol's Website"]]
        content =
            [tag "p" [] [
                text "Website under construction while I create a ",
                tag "a" [("href", "https://github.com/TerranceN/Haskell-Web-Framework")]
                                 [text "Web Framework in Haskell."]
            ]]

contactPage options = 
    mainLayout head content
    where
        head =
            [tag "title" [] [text "Contact Information"]
            ,tag "link" [("rel", "stylesheet")
                                 ,("type", "text/css")
                                 ,("href", "/~tniechci/styles/contact.css")] []]
        content =
            [template "../templates/contact.template" []]

geometryWarsPage options =
    projectLayout head content
    where
        head =
            [tag "title" [] [text "Geometry Wars Clone"]]
        content =
            [template "../templates/geowarsclone.template" []]

springPhysicsPage options =
    projectLayout head content
    where
        head =
            [tag "title" [] [text "Spring Physics Demo"]]
        content =
            [template "../templates/springphysics.template" []]

lightingDemoPage options =
    projectLayout head content
    where
        head =
            [tag "title" [] [text "2D Lighting Demo"]]
        content =
            [template "../templates/lightingdemo.template" []]

notFoundPage :: Page
notFoundPage options =
    mainLayout [] [tag "p" [] [
        tag "h1" [] [text "404"],
        text "Sorry, the page you requested cannot be found."
    ]]

testPost :: IO String
testPost = do
    env <- Env.getEnvironment
    contentLength <- return $ read $ fromMaybe "0" $ searchDict "CONTENT_LENGTH" env
    allContent <- getContents
    content <- return $ take contentLength allContent
    return content
    
postTest :: Page
postTest options =
    mainLayout [] [tag "p" [] [
        testPost
    ]]

formTest :: Page
formTest options =
    mainLayout [] [tag "p" [] [
        tag "form" [("action", "../postTest/"), ("method", "post")] [
            tag "input" [("type", "textbox"), ("name", "str")] [],
            tag "input" [("type", "submit")] []
        ]
    ]]

handlers :: [Handler]
handlers =
    [("/~tniechci/", index)
    ,("/~tniechci/contact/", contactPage)
    ,("/~tniechci/projects/geowarsclone/", geometryWarsPage)
    ,("/~tniechci/projects/springphysics/", springPhysicsPage)
    ,("/~tniechci/projects/lightingdemo/", lightingDemoPage)
    ,("/~tniechci/formtest/", formTest)
    ,("/~tniechci/posttest/", postTest)
    ,("/~tniechci/404/", notFoundPage)
    ]
