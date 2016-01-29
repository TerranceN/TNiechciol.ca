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
    head = do
        tag "title" [] $ text "Terrance Niechciol's Website"
        stylesheet "/styles/index.css"
    content = do
        image "/images/profile_pic.jpg" "A picture of me!"
        tag "p" [("class", "first")] $ do
            text "Hi, I'm Terrance Niechciol, and this is my personal website where you can find my "
            link "/Resume/" "resume"
            text ", and some small "
            link "/Projects/" "projects"
            text " that I've made for fun. I also have a "
            link "https://github.com/TerranceN" "github account"
            text " you can check out too."
        tag "p" [] $ do
            text "I'm a Computer Science student at the University of Waterloo in my 4th year. I'm taking an extra year though so I can do an additional co-op and take some harder CS courses (Computer Graphics, Computational Audio, and Real-time Programming). In my spare time I play badminton, and I've dabbled in learning the violin and learning french. I also have a love of coffee (pour-overs > all), artsy video games (like Fez, Gone Home, and Dark Souls), and meeting new people (through events, parties, and Lyft lines)."
        tag "p" [("class", "noindent")] $ do
            text "Here's my "
            link "/todo/" "todo list"
            text "."
        tag "p" [("class", "noindent")] $ do
            text "You can also find "
            link "https://github.com/TerranceN/TNiechciol.ca""this website on github"
            text "."
        tag "p" [("class", "noindent")] $ do
            text "Feel free to send me an email: "
            link "mailto:TNiechciol@gmail.com" "TNiechciol@gmail.com"

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

todoPage urlOptions request =
    mainLayout head content
  where
    head = do
      title "TODO"
      stylesheet "/styles/todo.css"
    content = do
      tag "h1" [] $ text "// TODO:"
      tag "hr" [] noHtml
      tag "p" [] $ do
        text "A big list of things I want to accomplish by the time I'm 30. I've always kept a list of things like this in the back of my head, but I've been inspired to flesh it out and put it online after looking at "
        link "http://www.evykassirer.com/todo" "Evy Kassier's todo page" >> text "."
      tag "h2" [] $ text "Tech"
      ulist
        [text "Work at a video game development company that targets consoles or PC (i.e. 'real' game development)"
        ,text "Create and release a video game that makes me at least $1 (basically the " >> link "http://ludumdare.com/compo/2014/09/29/october-challenge-2014/" "ludum dare October 2014 challenge" >> text ")"
        ]
      tag "h2" [] $ text "Health/Food"
      ulist 
        [text "Learn to cook my own meals by cooking at least 5/7 days a week " >> link "http://knowyourmeme.com/memes/5-7" "(thats a perfect score, right?)"
        ,text "Reduce my body fat percentage to 10-12% in order to have visible abs, and maintain that for at least a year"
        ,text "Learn to roast my own coffee beans and experiment with adding flavours"
        ]
      tag "h2" [] $ text "Travel"
      ulist 
        [text "Go to Oktoberfest in Munich"
        ]
      tag "h2" [] $ text "Music"
      ulist
        [text "Become competent at violin"
        ,text "Make music for a video game (even if it's my own game)"
        ]
      tag "h2" [] $ text "Languages"
      ulist 
        [text "Learn French"
        ,text "Learn German"
        ,text "Learn Mandarin"
        ]

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
    ,(exactly "/resume/", resumePage)
    ,(exactly "/todo/", todoPage)
    ,(exactly "/formtest/", formTest)
    ,(exactly "/posttest/", postTest)
    ,(exactly "/404/", notFoundPage)
    ,(string "/blog/" >> blogUrl, blogRouter)
    ,(exactly "/base_resume/", Resume.resumePage)
    ] ++ (subHandler "/projects" Projects.handlers)
