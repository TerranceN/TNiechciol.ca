module Handlers
( handlers
) where

import Data.Maybe
import Data.Char
import Data.List
import System.Environment as Env
import Control.Monad
import Control.Monad.Trans

import Text.ParserCombinators.Parsec

import Template
import HelperFunctions

import PageStructure
import qualified Blog

index :: [Option] -> [Option] -> Html
index urlOptions queryOptions =
    mainLayout head content
  where
    head =
        tag "title" [] $ text "Terrance Niechciol's Website"
    content =
        tag "p" [] $ do
            text "Website under construction while I create a "
            link "https://github.com/TerranceN/Haskell-Web-Framework"
                 "Web Framework in Haskell."

contactPage urlOptions queryOptions = 
    mainLayout head content
  where
    head = do
        title "Contact Information"
        stylesheet "/styles/contact.css"
    content = do
        tag "h1" [] $ text "Contact"
        tag "p" [] $ do
            text "Email: "
            link "mailto:TNiechciol@gmail.com" "TNiechciol@gmail.com"
        tag "p" [] $ text "Phone Number: 1-519-721-1435"

projectsPage urlOptions queryOptions =
    mainLayout head content
  where
    head = do
        title "Projects"
    content = do
        tag "ul" [] $ do
            tag "li" [] $ link "GeoWarsClone" "Geometry Wars Clone"
            tag "li" [] $ link "LightingDemo" "2D Lighting Demo"
            tag "li" [] $ link "ParadoxTower" "Paradox Tower"
            tag "li" [] $ link "SpringPhysics" "Spring Physics Demo"

geometryWarsPage urlOptions queryOptions =
    projectLayout head content
  where
    head =
        title "Geometry Wars Clone"
    content = do
        tag "h1" [] $ text "Geometry Wars Clone"
        projectSection "Description" $ do
            tag "p" [] $ do
                text "A clone of the Xbox Live Arcade game "
                link "http://en.wikipedia.org/wiki/Geometry_Wars" "Geometry Wars"
                text ", written in Java with "
                link "http://www.lwjgl.org/" "LWJGL"
                text ". Created during my final semester of high school (Feb. 2011 - June 2011)."
        projectSection "Video" $ do
            tag "div" [("class", "youtube_video")] $ do
                youtube_video "2-HFsanORGw" 560 315
        projectSection "Screenshots" $ do
            screenshot "/images/GWClone_grid.png" "Grid Effects"
            screenshot "/images/GWClone_dragon.png" "Dragons!"
        projectSection "Downloads" $ do
            tag "ul" [] $ do
                tag "li" [] $ link "/files/Geometry_Wars_Clone.zip" "JAR"
                tag "li" [] $ link "/files/GWCloneSrc.zip" "Source"

springPhysicsPage urlOptions queryOptions =
    projectLayout head content
  where
    head =
        title "Spring Physics Demo"
    content = do
        tag "h1" [] $ text "Spring Physics Demo"
        projectSection "Description" $ do
            tag "p" [] $ do
                text "A physics simulation of a blob of springs that can control its own springiness. Created in C++ using "
                link "http://www.sfml-dev.org/" "SFML"
                text " in a week in April 2012."
        projectSection "Video" $ do
            tag "div" [("class", "youtube_video")] $ do
                youtube_video "OS00DNj0GgI" 560 315
        projectSection "Downloads" $ do
            tag "h3" [] $ text "Binaries"
            tag "ul" [] $ do
                tag "li" [] $ link "/files/SpringPhysicsW32.zip" "Windows"
            tag "h3" [] $ text "Source"
            tag "ul" [] $ do
                tag "li" [] $ link "/files/SpringPhysicsSrc.zip" "Source"

lightingDemoPage urlOptions queryOptions = 
    projectLayout head content
  where
    head =
        title "2D Lighting Demo"
    content = do
        tag "h1" [] $ text "2D Lighting Demo"
        projectSection "Description" $ do
            tag "p" [] $ do
                text "This demo creates and displays shadow geometry based on arbitrary shapes in the scene. Coded in Java with "
                link "http://www.lwjgl.org/" "LWJGL"
                text ", and was worked on from Oct. 2011 to Jan. 2012."
        projectSection "Screenshots" $ do
            screenshot "/images/LightingDemo1_web.png" "alt"
            screenshot "/images/LightingDemo2_web.png" "alt"

paradoxTowerPage urlOptions queryOptions =
    projectLayout head content
  where
    head =
        title "Paradox Tower"
    content = do
        tag "h1" [] $ text "Paradox Tower"
        tag "p" [] $ do
            text "This project is hosted on the Global Jam Jam site "
            link "http://archive.globalgamejam.org/2012/paradox-tower" "here"
            text "."

notFoundPage :: [Option] -> [Option] -> Html
notFoundPage urlOptions queryOptions =
    mainLayout noHtml $ tag "p" [] $ do
        tag "h1" [] $ text "404"
        tag "p" [] $ text "Sorry, the page you requested cannot be found."

urlDecoder :: Parser String
urlDecoder = many (encoded <|> anyChar)
  where
    hexChars = ['0'..'9'] ++ ['A'..'Z']
    decodeHex c = case elemIndex (toUpper c) hexChars of
        Just x -> x
        Nothing -> 0
    encoded :: Parser Char
    encoded = do
        char '%'
        x <- oneOf hexChars
        y <- oneOf hexChars
        return (chr (16 * (decodeHex x) + (decodeHex y)))

decodeUrl str =
    case parse urlDecoder "url_decode" str of
        Right x -> x

testPost :: Html
testPost = do
    env <- lift Env.getEnvironment
    let contentLength = read $ fromMaybe "0" $ searchDict "CONTENT_LENGTH" env
    allContent <- lift getContents
    let content = decodeUrl (take contentLength allContent)
    text content

postTest :: [Option] -> [Option] -> Html
postTest urlOptions queryOptions = do
    mainLayout noHtml $ tag "p" [] $ testPost

formTest :: [Option] -> [Option] -> Html
formTest urlOptions queryOptions =
    mainLayout noHtml $ tag "p" [] $ do
        tag "form" [("action", "/postTest/"), ("method", "post")] $ do
            tag "input" [("type", "textbox"), ("name", "str")] noHtml
            tag "input" [("type", "submit")] noHtml

blogRouter :: [Option] -> [Option] -> Html
blogRouter urlOptions getOptions = do
    let blogOption = searchDict "blogName" urlOptions
    case blogOption of
        Nothing -> notFoundPage urlOptions getOptions
        Just blogName -> Blog.renderBlog blogName urlOptions getOptions

blogUrl :: Parser [Option]
blogUrl = do
    blogName <- many validHtmlChar
    char '/'
    return [("blogName", blogName)]

handlers :: [Handler]
handlers =
    [(exactly "/", index)
    ,(exactly "/contact/", contactPage)
    ,(exactly "/projects/", projectsPage)
    ,(exactly "/projects/geowarsclone/", geometryWarsPage)
    ,(exactly "/projects/springphysics/", springPhysicsPage)
    ,(exactly "/projects/lightingdemo/", lightingDemoPage)
    ,(exactly "/projects/paradoxtower/", paradoxTowerPage)
    ,(exactly "/formtest/", formTest)
    ,(exactly "/posttest/", postTest)
    ,(exactly "/404/", notFoundPage)
    ,(string "/blog/" >> blogUrl, blogRouter)
    ]
