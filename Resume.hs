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
                      [link "https://www.remind.com/about" "remind.com/about"
                      ,text "Jan. 2015 - Aug. 2015"
                      ,text "Ruby, Go, Javascript"
                      ,text "Rails, DynamoDB, React.js"
                      ] $ do
                        ulist [text "Replaced a prototype chat backend with a separate chat micro-service, in order to keep chat separate from core services in case chat experiences heavy load"
                              ,text "Updated the SMS and Email processing to support new chat features"
                              ,text "Rewrote major parts of the web dashboard using React to bring it to feature parity with the mobile clients"
                              ]
                    subsection "A Thinking Ape"
                      [link "http://www.athinkingape.com/about" "athinkingape.com/about"
                      ,text "May 2013 - Aug. 2013, Jan. 2014 - Aug. 2014"
                      ,text "Objective C, Python, Javascript, Java"
                      ,text "iOS, Django, Android, GLES 2.0"
                      ] $ do
                        ulist [text "Developed the iOS frontend of a prototype poker app focusing on home games. Eventually became " >> linkNewTab "https://itunes.apple.com/us/app/pineapple-poker/id906193660?mt=8" "Pineapple Poker"
                              ,text "Created and improved analytics tools on the metrics team in order for them to filter information faster and be able to see a user age breakdown for specific days"
                              ,text "Developed frontend features for a 3d racing game on Android, including an interactive map, and the movement/drifting animation for the cars using GLES 2.0"
                              ]
                section "Personal Projects" $ do
                    tag "div" [("class", "project_descriptions")] $ do
                        subsection "Geometry Wars Clone"
                          [link "/Projects/GeoWarsClone/" "eat.sleep.build/Projects/GeoWarsClone"
                          ,text "Sept. 2013 - Dec. 2013"
                          ,text "Scala"
                          ,text "LWJGL, OpenGL, GPGPU"
                          ] $ do
                            ulist [text "Created a clone of the Xbox Live Arcade game Geometry Wars to learn how effects like the deformable grid, and bloom are implemented"
                                  ,text "Coded the particle simulation to run on the GPU in order to have hundreds of thousands of particles without slowdown"
                                  ]
                        subsection "ATA Co-op Hackathon Game"
                          [link "/Projects/ATAHackathonGame/" "eat.sleep.build/Projects/ATAHackathonGame"
                          ,text "Two days during April 2014"
                          ,text "Java"
                          ,text "libGDX, OpenGL"
                          ] $ do
                            ulist [text "Created a 2D multiplayer deathmatch platformer for fun, for a 48-hour hackathon at A Thinking Ape with two other programmers, and two artists"
                                  ,text "Added the ability for players to phase through walls in order to add variety to the combat"
                                  ]
                        subsection "Defered Renderer with SSAO"
                          [link "/Projects/DeferedRenderer/" "eat.sleep.build/Projects/DeferedRenderer"
                          ,text "Sept. 2014 - Dec. 2014"
                          ,text "Scala"
                          ,text "LWJGL, OpenGL"
                          ] $ do
                            ulist [text "Created a tech demo to learn how to implement some modern graphics techniques"
                                  ,text "Implemented normal mapping and specular mapping to make walls look more detailed"
                                  ,text "Implented defered rendering in order to efficiently render many lights"
                                  ,text "Used depth information from the defered rendering process in order to create a screen space approximation of ambient occlusion, in order to have light falloff more realistically around corners"
                                  ]
                        subsection "eat.sleep.build"
                          [link "http://eat.sleep.build" "eat.sleep.build"
                          ,text "Nov. 2012 - present"
                          ,text "Haskell"
                          ,text "lighttpd"
                          ] $ do
                            ulist [text "Created a Haskell webapp running on top of lighttpd"
                                  ,text "Created an Html DSL using monads to programmatically compose html"
                                  ,text "Built from a \"Hello World\" Haskell application into a full website in order to better understand other frameworks like Django and Rails"
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

