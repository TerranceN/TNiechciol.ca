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
        tag "p" [] $ text "Phone Number: 1-519-721-1435"

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

projectsPage urlOptions request =
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

geometryWarsPage urlOptions request =
    projectLayout head content
  where
    head =
        title "Geometry Wars Clone"
    content = do
        tag "h1" [] $ text "Geometry Wars Clone"
        tag "hr" [] noHtml
        projectSection "Description" $ do
            tag "p" [] $ do
                text "Complete rewrite of the old version, which you can find "
                link "/Projects/GeoWarsCloneOld/" "here"
            tag "p" [] $ do
                text "A clone of the game "
                link "http://en.wikipedia.org/wiki/Geometry_Wars" "Geometry Wars"
                text " including a bloom effect, a huge number of particles, and a deformable grid (also implemented as particles). "
                text "Uses framebuffers to store particle position and velocity, allowing all particle simulation to be done on the GPU using shaders."
            tag "p" [] $ do
                text "Created using Scala and "
                link "http://www.lwjgl.org/" "LWJGL"
                text " during Nov. 2013 - Dec. 2013."
        projectSection "Screenshots" $ do
            screenshot "/images/geometry_wars_clone_v2.png" "Particles everywhere!"
        projectSection "Downloads" $ do
            tag "ul" [] $ do
                tag "li" [] $ do
                    link "https://github.com/TerranceN/Geometry-Wars-Clone-V2" "Source on Github"

geometryWarsOldPage urlOptions request =
    projectLayout head content
  where
    head =
        title "Geometry Wars Clone (old version)"
    content = do
        tag "h1" [] $ text "Geometry Wars Clone (old version)"
        tag "hr" [] noHtml
        projectSection "Description" $ do
            tag "p" [] $ do
                text "See the new, updated version "
                link "/Projects/GeoWarsClone/" "here"
            tag "p" [] $ do
                text "A clone of the Xbox Live Arcade game "
                link "http://en.wikipedia.org/wiki/Geometry_Wars" "Geometry Wars."
            tag "p" [] $ do
                text "Created using Java and "
                link "http://www.lwjgl.org/" "LWJGL"
                text " during my final semester of high school (Feb. 2011 - June 2011)."
        projectSection "Video" $ do
            tag "div" [("class", "youtube_video")] $ do
                youtube_video "2-HFsanORGw" 560 315
        projectSection "Screenshots" $ do
            screenshot "/images/GWClone_grid.png" "Grid Effects"
            screenshot "/images/GWClone_dragon.png" "Dragons!"
        projectSection "Downloads" $ do
            tag "ul" [] $ do
                tag "li" [] $ link "/files/Geometry_Wars_Clone.zip" "JAR"
                tag "li" [] $ do
                    link "/files/GWCloneSrc.zip" "Source"
                    text ", and on GitHub "
                    link "https://github.com/TerranceN/GeometryWarsClone" "here"
                    text "."

ataHackathonPage urlOptions request =
    projectLayout head content
  where
    head =
        title "ATA Hackathon Game"
    content = do
        tag "h1" [] $ text "ATA Hackathon Game"
        tag "hr" [] noHtml
        projectSection "Description" $ do
            tag "p" [] $ do
                text "Created for a 48-hour hackathon while working for my previous employer "
                link "http://www.athinkingape.com/" "A Thinking Ape"
                text " with a team of 2 other engineers on co-op there, as well as two full-time artists."
            tag "p" [] $ do
                text "Created using Java and "
                link "http://libgdx.badlogicgames.com/" "LibGDX"
                text " during Apr. 8-10, 2014."
        projectSection "Screenshots" $ do
            screenshot "/images/ata_hackathon_game_merged.png" "Game"
        projectSection "Downloads" $ do
            tag "ul" [] $ do
                tag "li" [] $ do
                    link "https://github.com/TerranceN/ATAHackathonW14" "Source on Github"

deferedRendererPage urlOptions request =
    projectLayout head content
  where
    head =
        title "Defered Renderer"
    content = do
        tag "h1" [] $ text "Defered Renderer"
        tag "hr" [] noHtml
        projectSection "Description" $ do
            tag "p" [] $ do
                text "3D Renderer that stores all albedo, depth, normal, and specular information into a series of textures,"
                text " then combines those textures and lighting information to make the final image."
                text " This allows lighting calculations to be done once per pixel, instead of once per fragnent."
            tag "p" [] $ do
                text "The renderer also computes ambient occlusion information using the scene information in the G-Buffer."
                text " Ambient occlusion is computed in a style similar to Crysis, but uses normal-oriented hemispheres instead of full spheres."
                text " This allows the normal map to be used, allowing for fine details in the computed occlusion map."
            tag "p" [] $ do
                text "Created using Scala and "
                link "http://www.lwjgl.org/" "LWJGL"
                text " during Sep. 2013 - Oct. 2013."
        projectSection "Screenshots" $ do
            screenshot "/images/defered_renderer.png" "Game"
        projectSection "Downloads" $ do
            tag "ul" [] $ do
                tag "li" [] $ do
                    link "https://github.com/TerranceN/Deferred-Renderer" "Source on Github"

springPhysicsPage urlOptions request =
    projectLayout head content
  where
    head =
        title "Spring Physics Demo"
    content = do
        tag "h1" [] $ text "Spring Physics Demo"
        tag "hr" [] noHtml
        projectSection "Description" $ do
            tag "p" [] $ do
                text "A physics simulation of a blob of springs that can control its own springiness."
            tag "p" [] $ do
                text "Created using C++ and "
                link "http://www.sfml-dev.org/" "SFML."
        projectSection "Video" $ do
            tag "div" [("class", "youtube_video")] $ do
                youtube_video "OS00DNj0GgI" 560 315
        projectSection "Downloads" $ do
            tag "h3" [] $ text "Binaries"
            tag "ul" [] $ do
                tag "li" [] $ link "/files/SpringPhysicsW32.zip" "Windows"
                tag "li" [] $ link "/files/SpringPhysicsLinux.zip" "Linux"
            tag "h3" [] $ text "Source"
            tag "ul" [] $ do
                tag "li" [] $ do
                    link "/files/SpringPhysicsSrc.zip" "Source"
                    text ", and on GitHub "
                    link "https://github.com/TerranceN/SpringPhysicsDemo" "here"
                    text "."

lightingDemoPage urlOptions request = 
    projectLayout head content
  where
    head =
        title "2D Lighting Demo"
    content = do
        tag "h1" [] $ text "2D Lighting Demo"
        tag "hr" [] noHtml
        projectSection "Description" $ do
            tag "p" [] $ do
                text "This demo creates and displays shadow geometry based on arbitrary shapes in the scene. Coded in Java with "
                link "http://www.lwjgl.org/" "LWJGL"
                text ", and was worked on from Oct. 2011 to Jan. 2012."
        projectSection "Screenshots" $ do
            screenshot "/images/LightingDemo1_web.png" ""
            screenshot "/images/LightingDemo2_web.png" ""
        projectSection "Downloads" $ do
            tag "ul" [] $ do
                tag "li" [] $ link "/files/2DShadows.zip" "JAR"
                tag "li" [] $ do
                    link "/files/2DShadowsSrc.zip" "Source"
                    text ", and on GitHub "
                    link "https://github.com/TerranceN/2DLightingDemo" "here"
                    text "."

paradoxTowerPage urlOptions request =
    projectLayout head content
  where
    head =
        title "Paradox Tower"
    content = do
        tag "h1" [] $ text "Paradox Tower"
        tag "hr" [] noHtml
        tag "p" [] $ do
            text "This project is hosted on the Global Jam Jam site "
            link "http://archive.globalgamejam.org/2012/paradox-tower" "here"
            text "."

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

handlers :: [Handler]
handlers =
    [(exactly "/", index)
    ,(exactly "/contact/", contactPage)
    ,(exactly "/resume/", resumePage)
    ,(exactly "/projects/", projectsPage)
    ,(exactly "/projects/geowarsclone/", geometryWarsPage)
    ,(exactly "/projects/geowarscloneold/", geometryWarsOldPage)
    ,(exactly "/projects/atahackathongame/", ataHackathonPage)
    ,(exactly "/projects/deferedrenderer/", deferedRendererPage)
    ,(exactly "/projects/springphysics/", springPhysicsPage)
    ,(exactly "/projects/lightingdemo/", lightingDemoPage)
    ,(exactly "/projects/paradoxtower/", paradoxTowerPage)
    ,(exactly "/formtest/", formTest)
    ,(exactly "/posttest/", postTest)
    ,(exactly "/404/", notFoundPage)
    ,(string "/blog/" >> blogUrl, blogRouter)
    ,(exactly "/base_resume/", Resume.resumePage)
    ]
