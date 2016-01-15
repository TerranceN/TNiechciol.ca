module Resume
( resume
, resumePage
, oswaldFont
) where

import Control.Monad
import Data.Maybe

import PageTypes
import PageStructure
import HelperFunctions

header = do
    tag "div" [("id", "header")] $ do
        tag "h1" [] $ text "Terrance Niechciol"
        tag "div" [("class", "contact_info")] $ do
            tag "div" [] $ linkNewTab "mailto:TNiechciol@gmail.com" "TNiechciol@gmail.com"
            tag "div" [] $ linkNewTab "http://github.com/TerranceN/" "github.com/TerranceN"
            tag "div" [] $ linkNewTab "tel:+15197211435" "+1-519-721-1435"
            tag "div" [] $ linkNewTab "http://eat.sleep.build" "eat.sleep.build"

withVertialSeperator content = do
  tag "div" [("class", "vertical_seperator_table")] $ do
      tag "div" [("class", "vertical_seperator_wrapper")] $ do
        tag "img" [("class", "vertical_seperator"), ("src", "/images/resume_gradient_vertical.png")] noHtml
      tag "div" [("class", "vertical_seperator_content")] $ do
        content
  

section name content = do
    tag "div" [("class", "section"), ("id", (slugify name) ++ "_section")] $ do
        tag "h2" [] $ text name
        tag "img" [("class", "seperator"), ("src", "/images/resume_gradient.png")] noHtml
        tag "div" [("class", "section_content")] content

subsection name sideInfo content = do
    tag "div" [("class", "subsection"), ("id", (slugify name) ++ "_subsection")] $ do
        tag "div" [] $ do
            tag "h3" [] $ text name
            tag "div" [("class", "inner_content")] content
            tag "div" [("class", "right_info")] $ do
                wrapDivs sideInfo
  where
    wrapDivs = mapM_ wrapDiv
    wrapDiv thing = tag "div" [] thing

ulist items = do
    tag "ul" [] $ mapM_ (\x -> tag "li" [] x) items

placeholder = "/images/placeholder.jpg"

resume = do
    tag "div" [("class", "resume_content")] $ do
        tag "div" [("id", "resume_wrapper")] $ do
        tag "div" [("id", "header_relative")] $ do
            header
            tag "div" [("id", "resume_body")] $ do
                section "Work Experience" $ do
                    subsection "Remind"
                      [(link "https://www.remind.com/about" "remind.com/about")
                      ,(text "Jan. 2015 - Aug. 2015")
                      ,(text "Ruby, Go, Javascript")
                      ,(text "Rails, React.js")
                      ] $ do
                        ulist [text "Replaced a prototype chat backend with a separate chat service named hermes, that stores messages using dynamoDB"
                              ,text "Updated SMS and Email handling to support chat"
                              ,text "Rewrote major parts of the web dashboard using React to bring it to feature parity with the mobile clients"
                              ]
                    tag "hr" [("class", "subsection_separator")] noHtml
                    subsection "A Thinking Ape"
                      [(link "http://www.athinkingape.com/about" "athinkingape.com/about")
                      ,(text "May 2013 - Aug. 2013, Jan. 2014 - Aug. 2014")
                      ,(text "Objective C, Python, Javascript, Java")
                      ,(text "iOS, Django, Android, GLES 2.0")
                      ] $ do
                        ulist [text "Developed the iOS frontend of a prototype poker app focusing on home games. Eventually became " >> linkNewTab "https://itunes.apple.com/us/app/pineapple-poker/id906193660?mt=8" "Pineapple Poker"
                              ,text "Created and improved analytics tools on the metrics team"
                              ,text "Developed frontend features for a 3d racing game on Android, including an interactive map, and the movement/drifting animation for the cars using GLES 2.0"
                              ]
                section "Personal Projects" $ do
                    tag "div" [("class", "project_descriptions")] $ do
                        subsection "Geometry Wars Clone"
                          [(link "/Projects/GeoWarsClone/" "eat.sleep.build/Projects/GeoWarsClone")
                          ,(text "Sept. 2013 - Dec. 2013")
                          ,(text "Scala")
                          ,(text "LWJGL, OpenGL, GPGPU")
                          ] $ do
                            ulist [text "2D, top down space shooter with deformable grid, particle effects, and a neon glow effect"
                                  ,text "Particle simulation (including grid) runs on the GPU by using framebuffers to store position/velocity, with shaders to update the simulation"
                                  ,text "Written in Scala using LWJGL"
                                  ]
                        tag "hr" [("class", "subsection_separator")] noHtml
                        subsection "ATA Co-op Hackathon Game"
                          [(link "/Projects/ATAHackathonGame/" "eat.sleep.build/Projects/ATAHackathonGame")
                          ,(text "Two days during April 2014")
                          ,(text "Java")
                          ,(text "libGDX, OpenGL")
                          ] $ do
                            ulist [text "2D multiplayer platformer deathmatch game, where players have the ability to create spheres of influence that remove collision with the level"
                                  ,text "Created for a 48-hour co-op student hackathon at A Thinking Ape with two other engineering co-ops and two full-time artists"
                                  ,text "Written in Java and libGDX"
                                  ]
                        tag "hr" [("class", "subsection_separator")] noHtml
                        subsection "Defered Renderer with SSAO"
                          [(link "/Projects/DeferedRenderer/" "eat.sleep.build/Projects/DeferedRenderer")
                          ,(text "Sept. 2014 - Dec. 2014")
                          ,(text "Scala")
                          ,(text "LWJGL, OpenGL")
                          ] $ do
                            ulist [text "Albedo, depth, and surface normals are stored in two textures, then combined for the lighting pass, which only has to be run per-pixel instead of per-fragment"
                                  ,text "SSAO is implemented Crysis-style by sampling the depth information in the G-Buffer to approximate the scene geometry"
                                  ,text "Written in Scala using LWJGL"
                                  ]
                        tag "hr" [("class", "subsection_separator")] noHtml
                        subsection "eat.sleep.build"
                          [(link "http://eat.sleep.build" "eat.sleep.build")
                          ,(text "Nov. 2012 - present")
                          ,(text "Haskell")
                          ,(text "lighttpd")
                          ] $ do
                            ulist [text "Haskell webapp running on top of lighttpd"
                                  ,text "Created an Html Monad to programmatically compose html"
                                  ]
                section "School" $ do
                  tag "p" [] $ do
                    text "Pursuing a Bachelor's in Computer Science from the University of Waterloo"

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

