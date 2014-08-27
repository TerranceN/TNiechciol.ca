module Resume where

import Control.Monad

import PageTypes
import PageStructure
import HelperFunctions

header = do
    tag "div" [("id", "header")] $ do
        tag "h1" [] $ text "Terrance Niechciol"
        tag "div" [] $ do
            tag "div" [] $ link "mailto:TNiechciol@gmail.com" "TNiechciol@gmail.com"
            tag "div" [] $ link "http://github.com/TerranceN/" "github.com/TerranceN"
            tag "div" [] $ link "tel:+15197211435" "+1-519-721-1435"
            tag "div" [] $ link "http://TNiechciol.ca" "TNiechciol.ca"

section name content = do
    tag "div" [("class", "section_wrapper"), ("id", (slugify name) ++ "_section_wrapper")] $ do
        tag "h2" [] $ tag "b" [] $ text name
        tag "img" [("class", "seperator"), ("src", "/images/resume_gradient.png")] noHtml
        tag "div" [] content

subsection name content = subsectionBase name Nothing Nothing content

subsectionWithDate name date content = subsectionBase name (Just date) Nothing content

subsectionWithDateAndUrl name date url content = subsectionBase name (Just date) (Just url) content

subsectionBase name date link content = do
    tag "div" [("class", "subsection_wrapper"), ("id", (slugify name) ++ "_subsection_wrapper")] $ do
        tag "h3" [] $ tag "b" [] $ do
            case link of
                Nothing -> text name
                Just url -> tag "a" [("href", url)] $ text name
        case date of
            Nothing -> noHtml
            Just date -> dateDiv date
        tag "div" [] content
  where
    dateDiv date = do
        tag "div" [("class", "right_info")] $ do
            date

ulist items = do
    tag "ul" [] $ mapM_ (\x -> tag "li" [] x) items

placeholder = "http://www.zwaldtransport.com/images/placeholders/placeholder1.jpg"

resume urlOptions request = do
    httpResponse 200 $ do
        tag "html" [] $ do
            tag "head" [] $ do
                tag "link" [("href", "http://fonts.googleapis.com/css?family=Oswald:400,700")
                           ,("rel", "stylesheet")
                           ,("type", "text/css")
                           ] noHtml
                stylesheet "/styles/base_resume.css"
            tag "body" [] $ do
                tag "div" [("id", "wrapper")] $ do
                tag "div" [("id", "header_relative")] $ do
                    header
                    tag "div" [("id", "content")] $ do
                        section "Skills" $ do
                            ulist [text "Proficient in both imperative (Python, Java, Javascript) and functional languages (Haskell, Scala)"
                                  ,text "Able to work independently within a small, focused team"
                                  ,text "Experience with Unix environments, bash scripting, etc."
                                  ,text "Experience with OpenGL, GLSL and GLES 2.0 (including both WebGL and Android)"
                                  ]
                        section "Work Experience" $ do
                            subsectionWithDateAndUrl "A Thinking Ape" (text "May 2013 - Aug. 2013, Jan. 2014 - Aug. 2014") "http://www.athinkingape.com/" $ do
                                ulist [text "Worked a total of three semesters, each with a different focus"
                                      ,text "Developed the iOS frontend of a prototype poker app"
                                      ,text "Worked on caching for the backend metrics team"
                                      ,text "Developed frontend features for a real-time 3d Android racing game, including an interactive map, and the movement/drifting simulation"
                                      ]
                            subsectionWithDateAndUrl "Willet" (text "Sept. 2012 - Dec. 2012") "http://www.secondfunnel.com/" $ do
                                ulist [text "Thrived in a fast-paced startup environment"
                                      ,text "Helped write a web app for brands to create an infitely scrolling page of related products"
                                      ,text "Web app was written in Python using the Django framework, with just normal html/css/javascript with jQuery on the frontend"
                                      ]
                        section "Personal Projects" $ do
                            tag "div" [("class", "project_descriptions")] $ do
                                subsection "Defered Renderer with SSAO" $ do
                                    ulist [text "Written in Scala using LWJGL"
                                          ,text "Albedo, depth, and surface normals are stored in two textures, then combined for the lighting pass, which only has to be run per-pixel instead of per-fragment"
                                          ,text "SSAO is implemented Crysis-style by sampling the depth information in the G-Buffer to approximate the scene geometry"
                                          ]
                                subsection "Geometry Wars Clone" $ do
                                    ulist [text "2D, top down, space shooter with deformable grid, particle effects, and a neon glow on everything"
                                          ,text "Written in Scala using LWJGL"
                                          ,text "Particle simulation (including grid) runs on the GPU by using FBOs to store position/velocity, with shaders to update the simulation"
                                          ]
                                subsection "ATA Co-op Hackathon Game" $ do
                                    ulist [text "2D multiplayer platformer deathmatch game, where players have the ability to create spheres of influence that remove collision with the level"
                                          ,text "Written in Java and libGDX"
                                          ,text "Created for a 48-hour co-op student hackathon at A Thinking Ape with two other engineering co-ops and two full-time artists"
                                          ]
                                subsection "TNiechciol.ca" $ do
                                    ulist [text "Haskell webapp running on top of lighttpd"
                                          ]
                            tag "div" [("class", "project_images")] $ do
                                tag "div" [] $ tag "img" [("src", placeholder)] noHtml
                                tag "div" [] $ tag "img" [("src", placeholder)] noHtml
                                tag "div" [] $ tag "img" [("src", placeholder)] noHtml
  where
    head = do
        noHtml
    body = do
        noHtml

