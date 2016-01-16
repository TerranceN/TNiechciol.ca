-- Needed for Parsec
{-# LANGUAGE GADTs #-}

module Handlers
( handlers
) where

import Data.Maybe
import Data.Char
import Data.List
import qualified Data.HashMap.Lazy as Map
import System.Environment as Env
import Control.Monad
import Control.Monad.Trans

import Text.ParserCombinators.Parsec

import Template
import HelperFunctions


import PageStructure
import qualified Blog
import qualified Resume
import qualified Projects

index urlOptions request =
    mainLayout head content
  where
    head =
        tag "title" [] $ text "Terrance Niechciol's Website"
    content = do
        tag "h1" [] $ text "Terrance Niechciol's Personal Website"
        tag "p" [] $ do
            text "Welcome to my personal website where you can find my "
            link "/Resume/" "resume"
            text ", and some small "
            link "/Projects/" "projects"
            text " that I've made for fun. If you need to get in touch with me, you check out my "
            link "/Contact/" "contact information"
            text ". I also have a "
            link "https://github.com/TerranceN" "github account"
            text " you can check out too."
        uText "<br />"
        tag "p" [] $ do
            text "You can also find this "
            link "https://github.com/TerranceN/TNiechciol.ca""website on github"
            text "."

contactPage urlOptions request = 
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
        tag "p" [] $ text "Phone Number: 1-614-721-5649"

resumePage urlOptions request =
    mainLayout head content
  where
    head = do
        title "Resume"
        Resume.oswaldFont
        stylesheet "/styles/resume.css"
        stylesheet "/styles/base_resume.css"
    content = do
        Resume.resume

notFoundPage urlOptions request = do
    httpResponse 404 $ mainPage noHtml $ tag "p" [] $ do
        tag "h1" [] $ text "404"
        tag "p" [] $ text "Sorry, the page you requested cannot be found."

urlDecoder :: Parser String
urlDecoder = many (encoded <|> space <|> anyChar)
  where
    hexChars = ['0'..'9'] ++ ['A'..'Z']
    decodeHex c = case elemIndex (toUpper c) hexChars of
        Just x -> x
        Nothing -> 0
    space :: Parser Char
    space = do
        char '+'
        return ' '
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

postTest urlOptions request = do
    mainLayout noHtml $ tag "p" [] $ testPost

formTest urlOptions request =
    mainLayout noHtml $ tag "p" [] $ do
        tag "form" [("action", "/postTest/"), ("method", "post")] $ do
            tag "input" [("type", "textbox"), ("name", "str")] noHtml
            tag "input" [("type", "submit")] noHtml

blogRouter urlOptions request = do
    let blogOption = Map.lookup "blogName" urlOptions
    case blogOption of
        Nothing -> notFoundPage urlOptions request
        Just blogName -> Blog.renderBlog blogName urlOptions request

blogUrl :: Parser (Map.HashMap String String)
blogUrl = do
    blogName <- many validHtmlChar
    char '/'
    return $ Map.fromList [("blogName", blogName)]

subHandler :: String -> [Handler] -> [Handler]
subHandler prefix handlers =
    map addPrefix handlers
  where
    addPrefix (url, responseFcn) = (string prefix >> url, responseFcn)

handlers :: [Handler]
handlers =
    [(exactly "/", index)
    ,(exactly "/contact/", contactPage)
    ,(exactly "/resume/", resumePage)
    ,(exactly "/formtest/", formTest)
    ,(exactly "/posttest/", postTest)
    ,(exactly "/404/", notFoundPage)
    ,(string "/blog/" >> blogUrl, blogRouter)
    ,(exactly "/base_resume/", Resume.resumePage)
    ] ++ (subHandler "/projects" Projects.handlers)
