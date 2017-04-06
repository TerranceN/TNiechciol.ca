module Resume
( resume
, resumePage
, fonts
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
            tag "div" [] $ mailIcon >> link "mailto:TNiechciol@gmail.com" "TNiechciol@gmail.com"
            tag "div" [] $ githubIcon >> link "https://github.com/TerranceN/" "github.com/TerranceN"
            tag "div" [] $ phoneIcon >> link "tel:+16147215649" "+1-614-721-5649"
            tag "div" [] $ linkIcon >> link "https://eat.sleep.build" "eat.sleep.build"

section name content = do
    tag "div" [("class", "section"), ("id", (slugify name) ++ "_section")] $ do
        tag "h2" [] $ text name
        tag "div" [("class", "section_content")] content

subsection name sideInfo headerInfo content = do
    tag "div" [("class", "subsection"), ("id", (slugify name) ++ "_subsection")] $ do
        tag "div" [] $ do
            tag "div" [("class", "subsection_header")] $ do
              tag "h3" [] $ text name
              maybe noHtml (\headerInfo -> tag "div" [("class", "header_info")] $ headerInfo) headerInfo
            tag "div" [("class", "inner_content")] content
            tag "div" [("class", "right_info")] $ do
                wrapDivs sideInfo
  where
    wrapDivs = mapM_ wrapDiv
    wrapDiv thing = tag "div" [] thing
linkIcon = image "/images/link_icon.svg" "link"
calendarIcon = image "/images/calendar_icon.svg" "date"
videoIcon = image "/images/video_icon.svg" "video"
langIcon = image "/images/lang_icon.svg" "language(s)"
toolIcon = image "/images/gear_icon.svg" "APIs/frameworks used"
githubIcon = image "/images/github_icon.svg" "github"
mailIcon = image "/images/email_icon.svg" "email"
phoneIcon = image "/images/phone_icon.svg" "phone number"
locationIcon = image "/images/location_icon.svg" "location"

resume = do
    tag "div" [("class", "resume_content")] $ do
        tag "div" [("id", "resume_wrapper")] $ do
        tag "div" [("id", "header_relative")] $ do
            header
            tag "div" [("id", "resume_body")] $ do
                section "Work Experience" $ do
                    subsection "Remind"
                      [linkIcon >> link "https://www.remind.com/about" "remind.com/about"
                      ,locationIcon >> text "San Francisco"
                      ,calendarIcon >> text "Jan. 2015 - Aug. 2015, May 2016 - Aug. 2016"
                      ,langIcon >> text "Ruby, Go, Javascript, Java"
                      ,toolIcon >> text "Rails, DynamoDB, React.js, Android"
                      ] (Just (text "(Co-op evaluations: Outstanding/Excellent)")) $ do
                        ulist [tag "span" [] $ text "Replaced a prototype chat backend with a chat micro-service, to separate chat performance concerns from the main API"
                              ,tag "span" [] $ text "Rewrote the web dashboard using React for feature parity with the mobile clients"
                              ,tag "span" [] $ text "Updated SMS copy to be more user-friendly and clear, resulting in the largest increase in SMS users installing the app in years"
                              ,tag "span" [] $ text "Consolidated old cross-platform prompt systems into a flexible, easy-to-extend system that's been adopted by other teams successfully"
                              ]
                    subsection "A Thinking Ape"
                      [linkIcon >> link "http://www.athinkingape.com/about" "athinkingape.com/about"
                      ,locationIcon >> text "Vancouver"
                      ,calendarIcon >> text "May 2013 - Aug. 2013, Jan. 2014 - Aug. 2014"
                      ,langIcon >> text "Objective C, Python, Javascript, Java"
                      ,toolIcon >> text "iOS, Django, Android, GLES 2.0"
                      ] (Just (text "(Co-op evaluations: Excellent/Excellent)")) $ do
                        ulist [tag "span" [] $ text "Developed the iOS frontend of a prototype poker app focusing on playing with friends, which eventually became " >> link "https://itunes.apple.com/us/app/pineapple-poker/id906193660?mt=8" "Pineapple Poker"
                              ,tag "span" [] $ text "Created and improved analytics tools on the metrics team in order for them to filter information faster and be able to see a user age breakdown for specific days"
                              ,tag "span" [] $ text "Developed frontend features for a 3D racing game on Android, including an interactive map, and the movement/drifting animation for the cars using GLES 2.0"
                              ]
                section "Personal Projects" $ do
                    tag "div" [("class", "project_descriptions")] $ do
                        subsection "Geometry Wars Clone"
                          [linkIcon >> link "https://eat.sleep.build/Projects/GeoWarsClone/" "eat.sleep.build/Projects/GeoWarsClone"
                          ,videoIcon >> link "https://youtu.be/Xv-3VLCFOQM" "youtu.be/Xv-3VLCFOQM"
                          ,calendarIcon >> text "Sept. 2013 - Dec. 2013"
                          ,langIcon >> text "Scala"
                          ,toolIcon >> text "LWJGL, OpenGL, GPGPU"
                          ] Nothing $ do
                            ulist [tag "span" [] $ text "Created a clone of the Xbox Live Arcade game Geometry Wars to learn how to implement effects like the deformable grid, and bloom"
                                  ,tag "span" [] $ text "Particles are simulated on the GPU to have hundreds of thousands without slowdown"
                                  ,tag "span" [] $ text "The deformable grid is also simulated on the GPU but is affected by ships and bullets"
                                  ,tag "span" [] $ text "The dynamic music system plays more intense music when there's more enemies"
                                  ]
                        subsection "ATA Co-op Hackathon Game"
                          [linkIcon >> link "https://eat.sleep.build/Projects/ATAHackathonGame/" "eat.sleep.build/Projects/ATAHackathonGame"
                          ,videoIcon >> link "https://youtu.be/y7BLvpp1HlY" "youtu.be/y7BLvpp1HlY"
                          ,calendarIcon >> text "A weekend in April 2014"
                          ,langIcon >> text "Java"
                          ,toolIcon >> text "libGDX, OpenGL"
                          ] Nothing $ do
                            ulist [tag "span" [] $ text "Created a 2D multiplayer deathmatch platformer for a 48-hour hackathon at A Thinking Ape with two other programmers, and two artists"
                                  ,tag "span" [] $ text "Added the ability for players to phase through walls to add variety to the combat"
                                  ,tag "span" [] $ text "Responsible for movement and level collision, and graphical effects like bullet trails and the desaturation effect when phasing through walls"
                                  ]
                        subsection "Deferred Renderer with SSAO"
                          [linkIcon >> link "https://eat.sleep.build/Projects/DeferredRenderer/" "eat.sleep.build/Projects/DeferredRenderer"
                          ,videoIcon >> link "https://youtu.be/eJY72rMtFx4" "youtu.be/eJY72rMtFx4"
                          ,calendarIcon >> text "Sept. 2014 - Dec. 2014"
                          ,langIcon >> text "Scala"
                          ,toolIcon >> text "LWJGL, OpenGL"
                          ] Nothing $ do
                            ulist [tag "span" [] $ text "Created a tech demo to learn how to implement some modern graphics techniques"
                                  ,tag "span" [] $ text "Implemented normal mapping and specular mapping to make walls look more detailed"
                                  ,tag "span" [] $ text "Implemented deferred rendering to efficiently render many lights"
                                  ,tag "span" [] $ text "Used depth information from the deferred rendering process to create a screen space approximation of ambient occlusion, to have light falloff more realistically in corners"
                                  ]
                        subsection "eat.sleep.build"
                          [linkIcon >> link "https://eat.sleep.build" "eat.sleep.build"
                          ,calendarIcon >> text "Nov. 2012 - present"
                          ,langIcon >> text "Haskell"
                          ,toolIcon >> text "lighttpd"
                          ] Nothing $ do
                            ulist [tag "span" [] $ text "Created a Haskell webapp running on top of lighttpd"
                                  ,tag "span" [] $ text "Created an Html DSL using monads to programmatically compose html"
                                  ,tag "span" [] $ text "Built from a \"Hello World\" Haskell application into a full website to better understand how other frameworks like Django and Rails work"
                                  ]
                section "School" $ do
                  tag "p" [] $ do
                    uText "Computer Science student at the University of Waterloo &mdash; Expected graduation: August 2017"

fonts = do
    tag "link" [("href", "https://fonts.googleapis.com/css?family=Oswald:400,700|Roboto Condensed")
               ,("rel", "stylesheet")
               ,("type", "text/css")
               ] noHtml

resumePage urlOptions request = do
    httpResponse 200 $ do
        tag "html" [] $ do
            tag "head" [] $ do
                fonts
                stylesheet "/styles/base_resume.css"
                stylesheet "/styles/resume_page.css"
            tag "body" [] $ do
                resume
  where
    head = do
        noHtml
    body = do
        noHtml

