module Resume
( resume
, resumePage
, oswaldFont
) where

import Control.Monad

import PageTypes
import PageStructure
import HelperFunctions

header = do
    tag "div" [("id", "header")] $ do
        tag "h1" [] $ text "Terrance Niechciol"
        tag "div" [] $ do
            tag "div" [] $ linkNewTab "mailto:TNiechciol@gmail.com" "TNiechciol@gmail.com"
            tag "div" [] $ linkNewTab "http://github.com/TerranceN/" "github.com/TerranceN"
            tag "div" [] $ linkNewTab "tel:+15197211435" "+1-519-721-1435"
            tag "div" [] $ linkNewTab "http://TNiechciol.ca" "TNiechciol.ca"

section name content = do
    tag "div" [("class", "section_wrapper"), ("id", (slugify name) ++ "_section_wrapper")] $ do
        tag "h2" [] $ tag "b" [] $ text name
        tag "img" [("class", "seperator"), ("src", "/images/resume_gradient.png")] noHtml
        tag "div" [] content

subsection name content = subsectionBase name Nothing Nothing content

subsectionWithUrl name url content = subsectionBase name Nothing (Just url) content

subsectionWithDate name date content = subsectionBase name (Just date) Nothing content

subsectionWithDateAndUrl name date url content = subsectionBase name (Just date) (Just url) content

subsectionBase name date link content = do
    tag "div" [("class", "subsection_wrapper"), ("id", (slugify name) ++ "_subsection_wrapper")] $ do
        tag "div" [("class", "vertical_seperator_table")] $ do
            tag "img" [("class", "vertical_seperator"), ("src", "/images/resume_gradient_vertical.png")] noHtml
            tag "div" [("class", "subsection_seperated_content")] $ do
                tag "h3" [] $ tag "b" [] $ do
                    case link of
                        Nothing -> text name
                        Just url -> linkNewTab url name
                case date of
                    Nothing -> noHtml
                    Just date -> dateDiv date
                tag "div" [("class", "inner_content")] content
  where
    dateDiv date = do
        tag "div" [("class", "right_info")] $ do
            date

ulist items = do
    tag "ul" [] $ mapM_ (\x -> tag "li" [] x) items

projectImage url = do
    tag "div" [("class", "project_image")] $ do
        tag "img" [("src", url)] noHtml

placeholder = "/images/placeholder.jpg"

resume = do
    tag "div" [("class", "resume_content")] $ do
        tag "div" [("id", "resume_wrapper")] $ do
        tag "div" [("id", "header_relative")] $ do
            header
            tag "div" [("id", "resume_body")] $ do
                section "Skills" $ do
                    subsection "Languages" $ do
                        tag "div" [("class", "skills_sublist")] $ do
                            ulist [text "C/C++"
                                  ,text "Java"
                                  ,text "Python"
                                  ,text "Scala"
                                  ]
                        tag "div" [("class", "skills_sublist")] $ do
                            ulist [text "Javascript"
                                  ,text "Haskell"
                                  ,text "Lisp"
                                  ]
                    subsection "Platforms" $ do
                        ulist [text "Web"
                              ,text "Desktop"
                              ,text "IOS"
                              ,text "Android"
                              ]
                    subsection "Technologies/Libraries" $ do
                        ulist [text "OpenGL/GLSL"
                              ,text "GLES 2.0 (both Android and WebGL)"
                              ,text "LWJGL"
                              ,text "LibGDX"
                              ,text "SFML"
                              ]
                section "Work Experience" $ do
                    subsectionWithDateAndUrl "A Thinking Ape" (text "May 2013 - Aug. 2013, Jan. 2014 - Aug. 2014") "http://www.athinkingape.com/" $ do
                        ulist [text "Developed the iOS frontend of a prototype poker app focusing on home games. Eventually became " >> linkNewTab "https://itunes.apple.com/us/app/pineapple-poker/id906193660?mt=8" "Pineapple Poker"
                              ,text "Created and improved analytics tools on the metrics team"
                              ,text "Developed frontend features for a 3d racing game on Android, including an interactive map, and the movement/drifting animation for the cars using GLES 2.0"
                              ]
                    subsectionWithDateAndUrl "Willet" (text "Sept. 2012 - Dec. 2012") "http://www.secondfunnel.com/" $ do
                        ulist [text "Created front end for an infintely scrolling webpage that predicts user's product preferences"
                              ,text "Server code written in Python with Django, and fontend in Sass/Javascript with jQuery"
                              ,text "As the 5th member of the company, able to keep up in a fast-paced, small startup"
                              ]
                section "Personal Projects" $ do
                    tag "div" [("class", "project_descriptions")] $ do
                        subsectionWithUrl "Geometry Wars Clone" "/Projects/GeoWarsClone/" $ do
                            ulist [text "2D, top down space shooter with deformable grid, particle effects, and a neon glow effect"
                                  ,text "Particle simulation (including grid) runs on the GPU by using framebuffers to store position/velocity, with shaders to update the simulation"
                                  ,text "Written in Scala using LWJGL"
                                  ]
                        subsectionWithUrl "ATA Co-op Hackathon Game" "/Projects/ATAHackathonGame/" $ do
                            ulist [text "2D multiplayer platformer deathmatch game, where players have the ability to create spheres of influence that remove collision with the level"
                                  ,text "Created for a 48-hour co-op student hackathon at A Thinking Ape with two other engineering co-ops and two full-time artists"
                                  ,text "Written in Java and libGDX"
                                  ]
                        subsectionWithUrl "Defered Renderer with SSAO" "/Projects/DeferedRenderer/" $ do
                            ulist [text "Albedo, depth, and surface normals are stored in two textures, then combined for the lighting pass, which only has to be run per-pixel instead of per-fragment"
                                  ,text "SSAO is implemented Crysis-style by sampling the depth information in the " >> (tag "div" [] $ text "G-Buffer to approximate the scene geometry")
                                  ,text "Written in Scala using LWJGL"
                                  ]
                        subsectionWithUrl "TNiechciol.ca" "http://TNiechciol.ca" $ do
                            ulist [text "Haskell webapp running on top of lighttpd"
                                  ]
                    tag "div" [("class", "project_images")] $ do
                        projectImage "/images/geometry_wars_clone_v2_small.png"
                        projectImage "/images/ata_hackathon_game_merged_small.png"
                        projectImage "/images/defered_renderer_small.png"
                section "School" $ do
                    text "Persuing a Bachelor's in Computer Science from the University of Waterloo"

oswaldFont = do
    tag "link" [("href", "http://fonts.googleapis.com/css?family=Oswald:400,700")
               ,("rel", "stylesheet")
               ,("type", "text/css")
               ] noHtml

resumePage urlOptions request = do
    httpResponse 200 $ do
        tag "html" [] $ do
            tag "head" [] $ do
                oswaldFont
                stylesheet "/styles/base_resume.css"
                stylesheet "/styles/resume_page.css"
            tag "body" [] $ do
                resume
  where
    head = do
        noHtml
    body = do
        noHtml

