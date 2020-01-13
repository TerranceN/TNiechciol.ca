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

import HelperFunctions


import PageStructure
import qualified Resume
import qualified Projects

index urlOptions request =
    mainLayout head content [("section", "home")]
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
            text " that I've made for fun (at least some of them anyway). I also have a "
            link "https://github.com/TerranceN" "github account"
            text " you can check out too."
        tag "p" [] $ do
            text "I'm a Software Developer living in San Francisco, trying not to fall into the stereotypical SF techie life. I'm currently " >> link "https://www.remind.com/about/" "helping teachers for a living" >> text ", and working on a bunch of cool things in my spare time, including but not limited to:"
            tag "div" [] $ ulist
              [text "Building interesting modes of transportation (like a bamboo bicycle or a onewheel)"
              ,text "Fermenting things (like cucumbers to make pickles, or honey to make mead)"
              ,text "Playing artsy video games (like Fez, Gone Home, and Dark Souls)"
              ,text "Lifting weights"
              ,text "Learning swing dancing"
              ]
        tag "p" [("class", "noindent")] $ do
            text "You can also find "
            link "https://github.com/TerranceN/TNiechciol.ca" "this website on github"
            text " (the way I built it is kinda cool)."
        tag "p" [("class", "noindent")] $ do
            text "Here's my "
            link "/todo/" "life todo list"
            text "."
        tag "p" [("class", "noindent")] $ do
            text "Feel free to send me an email: "
            link "mailto:TNiechciol@gmail.com" "TNiechciol@gmail.com"

resumePage urlOptions request =
    mainLayout head content [("section", "resume")]
  where
    head = do
        title "Resume"
        Resume.fonts
        stylesheet "/styles/resume.css"
        stylesheet "/styles/base_resume.css"
    content = do
        tag "div" [("class", "links")] $ do
          tag "div" [] $ link "/MuseumOfResumes" "Museum of Resumes"
          tag "div" [] $ link "/files/Resume.pdf" "Printable PDF"
        Resume.resume

todoPage urlOptions request =
    mainLayout head content []
  where
    head = do
      title "TODO"
      stylesheet "/styles/todo.css"
    content = do
      tag "h1" [] $ text "// TODO:"
      tag "hr" [] noHtml
      tag "p" [] $ do
        text "A big list of things I want to accomplish " >> tag "strike" [] (text "by the time I'm 30") >> text ". I've always kept a list of things like this in the back of my head, but I've been inspired to flesh it out and put it online after looking at "
        link "http://www.evykassirer.com/todo" "Evy Kassier's todo page" >> text "."
      tag "h2" [] $ text "Tech/Engineering"
      ulist
        [text "Work at a video game development company that makes traditional video games"
        ,text "Create and release a video game that makes me at least $1 (basically the " >> link "http://ludumdare.com/compo/2014/09/29/october-challenge-2014/" "ludum dare October 2014 challenge" >> text ")"
        ,tag "strike" [] (text "Learn how to use a 3D printer") >> text " (GF and I put together a " >> link "https://www.prusa3d.com/original-prusa-i3-mk3/" "Prusa I3 MK3s kit" >> text ")"
        ,tag "strike" [] $ text "Make my garage door openable over WiFi"
        ,text "Build my own smart door system"
        ,tag "strike" [] (text "Learn how to use a laser cutter") >> text " (I bought a " >> link "https://www.google.com/search?q=k40+laser" "K40 Laser" >> text ")"
        ,do
          tag "strike" [] (text "Build a CNC Machine") >> text " (I built one of " >> link "https://www.v1engineering.com/specifications/" "these" >> text ")"
          tag "div" [] $ ulist
            [text "Learn to reliably machine aluminum"
            ,text "Learn to do multi-sided machining"
            ]
        ,text "Build my own " >> link "https://onewheel.com/" "OneWheel"
        ]
      tag "h2" [] $ text "Health/Food"
      ulist 
        [tag "strike" [] $ text "Make lacto-fermented pickles"
        ,tag "strike" [] $ text "Make mead (honey wine)"
        ,do
          tag "p" [] $ tag "strike" [] $ text "Learn to cook my own meals by cooking at least 5/7 days a week " >> link "http://knowyourmeme.com/memes/5-7" "(thats a perfect score, right?)"
          tag "p" [] $ text "Now that I can survive on my own cooking abilities, I want to learn to cook specific things:"
          tag "div" [] $ ulist
            [tag "strike" [] $ text "Lasagna"
            ,tag "strike" [] $ text "Scrambled Eggs"
            ,tag "strike" [] $ text "Chicken Stir-Fry"
            ,tag "strike" [] $ text "Butter Chicken"
            ,text "Shakshuka"
            ,text "Chicken/Turkey in the oven"
            ,text "Cheesecake"
            ]
        ,text "Reduce my body fat percentage to 10-12% in order to have visible abs, and maintain that for at least a year"
        ,tag "strike" [] $ text "Squat 225lbs"
        ,text "Deadlift 225lbs"
        ,text "Bench 135lbs"
        ,tag "strike" [] $ text "Brew my own coffee regularly"
        ,tag "strike" [] $ text "Learn to roast my own coffee beans and experiment with adding flavours"
        ]
      tag "h2" [] $ text "Travel"
      ulist 
        [text "Go to a beer festival in Germany"
        ,text "Visit the Rocky Mountains in Alberta"
        ,tag "strike" [] $ text "Hike " >> link "http://www.yosemitehikes.com/tioga-road/clouds-rest/clouds-rest.htm" "Cloud's Rest" >> text " (my favourite hike so far!)"
        ,tag "strike" [] $ text "Hike " >> link "http://www.yosemitehikes.com/yosemite-valley/half-dome/half-dome.htm" "Half Dome"
        ]
      tag "h2" [] $ text "Music"
      ulist
        [text "Become good enough at violin to play " >> link "https://www.youtube.com/watch?v=8Ilq8MmasjA&t=16s" "Bach Partita No. 3 Prelude"
        ,tag "strike" [] $ text "Make music for a video game (even if it's my own game)"
        ]
      tag "h2" [] $ text "Languages"
      ulist 
        [do
          text "Learn Polish"
          tag "div" [] $ ulist
            [tag "strike" [] $ text "Maintain a duolingo streak of over half a year"
            ,text "Get 1:1 video tutoring"
            ]
        ,text "Learn French"
        ]

museumOfResumes urlOptions request = do
    mainLayout head body []
  where
    head = do
      title "Museum of Resumes"
      stylesheet "/styles/museum_of_resumes.css"
    body = do
      tag "h1" [] $ text "Museum of Resumes"
      tag "hr" [] noHtml
      tag "p" [] $ text "Here you'll find a collection of snapshots of my old resumes, my thoughts on them, and why I changed certain things. I sometimes try weird stuff on them, but I've found employers to be very lenient about it, and at least they stand out. Let's laugh at how bad they are together."
      tag "div" [("class", "resume_section")] $ do
        tag "h2" [] $ text "August 2012 - Waterloo 2A"
        link "/files/old_resumes/2012-08.pdf" "PDF file"
        tag "ul" [] $ do
          tag "li" [] $ text "Made with html/css and Chrome's print to pdf function."
          tag "li" [] $ text "My first resume ever."
          tag "li" [] $ text "Led to my first ever job working at Willet!"
          tag "li" [] $ text "I thought making my resume look like code would make it stand out. I guess it worked?"
          tag "li" [] $ text "I like that right from the get-go I thought that having a way for employers to find out more about your projects was important. I should have linked to them directly though."
      tag "div" [("class", "resume_section")] $ do
        tag "h2" [] $ text "April 2013 - Waterloo 2B"
        link "/files/old_resumes/2013-04.pdf" "PDF file"
        tag "ul" [] $ do
          tag "li" [] $ text "Made with LaTeX."
          tag "li" [] $ text "Led to my job at A Thinking Ape!"
          tag "li" [] $ text "After making my previous resume I really hated having it go onto two pages so I really cut my resume down to just the important stuff."
          tag "li" [] $ text "That revealed that I actually have a lack of important content to put on it (which is why there's so much empty space)."
          tag "li" [] $ text "Projects are now linked to directly."
          tag "li" [] $ text "Links lack a distinct style to visually identify them, which is a no-no."
      tag "div" [("class", "resume_section")] $ do
        tag "h2" [] $ text "December 2014 - Waterloo 3B"
        link "/files/old_resumes/2014-12.pdf" "PDF file"
        tag "ul" [] $ do
          tag "li" [] $ text "Made with html/css and Chrome's print to pdf function."
          tag "li" [] $ text "Led to my job at Remind!"
          tag "li" [] $ text "I got the idea in my head that I should minimize time from opening my resume to seeing my projects in action, so I added inline images."
          tag "li" [] $ text "The inline images are a bit overboard though. If it's ever printed in black and white it would look like garbage."
          tag "li" [] $ text "Links have a distinct style, but if it's ever printed you wouldn't know what they link to."
          tag "li" [] $ text "Experimented with using different fonts, but I only used one font and it doesn't work well for the smaller text."
      tag "div" [("class", "resume_section")] $ do
        tag "h2" [] $ text "December 2015 - Waterloo 4A"
        link "/files/old_resumes/2015-12.pdf" "PDF file"
        tag "ul" [] $ do
          tag "li" [] $ text "Made with html/css and Chrome's print to pdf function."
          tag "li" [] $ text "The biggest change here is that I updated it to include Remind, and reduced the file size from 3.2MB to 157KB"
      tag "div" [("class", "resume_section")] $ do
        tag "h2" [] $ text "April 2016 - Waterloo 4B"
        tag "div" [] $ link "/files/old_resumes/2016-04.pdf" "PDF file"
        tag "ul" [] $ do
          tag "li" [] $ text "Made with html/css and " >> link "http://wkhtmltopdf.org/" "wkhtmltopdf" >> text "."
          tag "li" [] $ text "I was (correctly) talked out of having inline images in a resume during a resume critique."
          tag "li" [] $ text "Realized some people might print this out so links should be written in full."
          tag "li" [] $ text "Included youtube links because seeing a game in motion is more convincing than seeing a screenshot."
          tag "li" [] $ text "Experimented with removing the skills section entirely and just listing the skills I used beside each job and project."
          tag "li" [] $ text "Experimented with bubble letters to make my resume feel lighter, but it allowed the smaller text to overpower the titles too much."
          tag "li" [] $ text "I experimented with different ways of seperating sections. But I feel like having a full border stands out too much."
      tag "div" [("class", "resume_section")] $ do
        tag "h2" [] $ text "December 2016 - Waterloo 5A"
        link "/files/old_resumes/2016-12.pdf" "PDF file"
        tag "ul" [] $ do
          tag "li" [] $ text "Made with html/css and " >> link "http://wkhtmltopdf.org/" "wkhtmltopdf" >> text "."
          tag "li" [] $ text "Switched back to solid lettering"
          tag "li" [] $ text "Used a second font for the smaller text, which looks soooo much better now."
          tag "li" [] $ text "Added more room between the two columns and left-aligned the right column. They don't look like they run into each other anymore."
          tag "li" [] $ text "Added svg icons for the info lines on the right, which makes a very solid visual line separating the columns."
          tag "li" [] $ text "Experimented with separating sections with horizontal red lines and subsections with grey lines."
      tag "div" [("class", "resume_section")] $ do
        tag "h2" [] $ text "April 2017 - Waterloo 5B"
        link "/files/old_resumes/2017-04.pdf" "PDF file"
        tag "ul" [] $ do
          tag "li" [] $ text "Made with html/css and " >> link "http://wkhtmltopdf.org/" "wkhtmltopdf" >> text "."
          tag "li" [] $ text "Updated with my new graduation date."
      tag "div" [("class", "resume_section")] $ do
        tag "h2" [] $ text "May 2017 - Waterloo 6A"
        link "/files/old_resumes/2017-05.pdf" "PDF file"
        tag "ul" [] $ do
          tag "li" [] $ text "Made with html/css and " >> link "https://github.com/fraserxu/electron-pdf" "electron-pdf" >> text "."
          tag "li" [] $ text "Using electron-pdf and pdftk, the pdf is actually letter sized."
          tag "li" [] $ text "Moved extra info column to fit under job titles."
          tag "li" [] $ text "Replaced job titles with the company logo."
          tag "li" [] $ text "Removed my phone number."
      tag "div" [("class", "resume_section")] $ do
        tag "h2" [] $ text "Current"
        link "/Resume" "Direct link"
        link "/files/Resume.pdf" "PDF file"
        tag "ul" [] $ do
          tag "li" [] $ text "Made with html/css and " >> link "https://github.com/fraserxu/electron-pdf" "electron-pdf" >> text "."

notFoundPage urlOptions request = do
    httpResponse 404 $ mainPage noHtml body []
  where
    body =
      tag "p" [] $ do
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
    mainLayout noHtml body []
  where
    body = tag "p" [] $ testPost

formTest urlOptions request =
    mainLayout noHtml body []
  where
    body =
      tag "p" [] $ do
        tag "form" [("action", "/postTest/"), ("method", "post")] $ do
            tag "input" [("type", "textbox"), ("name", "str")] noHtml
            tag "input" [("type", "submit")] noHtml

subHandler :: String -> [Handler] -> [Handler]
subHandler prefix handlers =
    map addPrefix handlers
  where
    addPrefix (url, responseFcn) = (string prefix >> url, responseFcn)

handlers :: [Handler]
handlers =
    [(exactly "/", index)
    ,(exactly "/resume/", resumePage)
    ,(exactly "/museumofresumes/", museumOfResumes)
    ,(exactly "/todo/", todoPage)
    ,(exactly "/formtest/", formTest)
    ,(exactly "/posttest/", postTest)
    ,(exactly "/404/", notFoundPage)
    ,(exactly "/base_resume/", Resume.resumePage)
    ] ++ (subHandler "/projects" Projects.handlers)
