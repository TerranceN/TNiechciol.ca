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
            tag "div" [] $ linkIcon >> link "https://eat.sleep.build" "eat.sleep.build"

section name content = do
    tag "div" [("class", "section"), ("id", (slugify name) ++ "_section")] $ do
        tag "h2" [] $ text name
        tag "div" [("class", "section_content")] content

subsection header sideInfo headerInfo content = do
    tag "div" [("class", "subsection")] $ do
        tag "div" [] $ do
            tag "div" [("class", "inner_content")] content
            tag "div" [("class", "left_info")] $ do
              tag "div" [("class", "subsection_header")] $ do
                header
                maybe noHtml (\headerInfo -> tag "div" [("class", "header_info")] $ headerInfo) headerInfo
              tag "div" [("class", "info")] $ do
                wrapDivs sideInfo
  where
    wrapDivs = mapM_ wrapDiv
    wrapDiv thing = tag "div" [] thing

linkIcon = inlineSvg "./images/link_icon.svg"
calendarIcon = inlineSvg "./images/calendar_icon.svg"
videoIcon = inlineSvg "./images/video_icon.svg"
langIcon = inlineSvg "./images/lang_icon.svg"
toolIcon = inlineSvg "./images/gear_icon.svg"
githubIcon = inlineSvg "./images/github_icon.svg"
mailIcon = inlineSvg "./images/email_icon.svg"
phoneIcon = inlineSvg "./images/phone_icon.svg"
locationIcon = inlineSvg "./images/location_icon.svg"

remindLogo = tag "div" [("id", "remindLogo")] $ image "/images/remind_logo.svg" "Remind"
ataLogo = tag "div" [("id", "ataLogo")] $ image "/images/ata_logo.svg" "A Thinking Ape"
waterlooLogo = tag "div" [("id", "waterlooLogo")] $ image "/images/uwaterloo_logo.svg" "A Thinking Ape"

resume = do
  tag "div" [("class", "resume_content")] $ do
    tag "div" [("id", "resume_wrapper")] $ do
      header
      tag "div" [("id", "resume_body")] $ do
        subsection remindLogo
          [linkIcon >> link "https://www.remind.com/about" "remind.com/about"
          ,locationIcon >> text "San Francisco"
          --,calendarIcon >> text "Jan. 2015 - Aug. 2015, May 2016 - Aug. 2016, Sept. 2017 - now"
          ,calendarIcon >> do
            tag "div" [] $ do
              tag "p" [] $ text "Jan. 2015 - Aug. 2015,"
              tag "p" [] $ text "May 2016 - Aug. 2016,"
              tag "p" [] $ text "Sept. 2017 - now"
          ,langIcon >> text "Ruby, Go, Typescript, Javascript"
          ,toolIcon >> text "Rails, React, Postgres, DynamoDB, Redis"
          ] Nothing $ do
                  ulist
                    [tag "div" [] $ bold (text "Backend: ") >> text "Lead or co-led the creation of a half dozen new micro-services, including services for our chat, third party integrations, and our authentication systems."
                    ,tag "div" [] $ bold (text "Frontend: ") >> text "Heavily involved in web frontend development, from being on the team that re-wrote it in React in 2015, to regularly implementing features for it to this day. Parts of the code I wrote in 2015 is still in use today."
                    ,tag "div" [] $ bold (text "Feature Ownership: ") >> text "Lead the successful development of dozens of product features, personally implementing large parts of the backend and web frontend of those."
                    ,tag "div" [] $ bold (text "Enterprise Readiness: ") >> text "Regularly involved in incident response and resolution, including projects to resolve multiple core, time-sensitive issues."
                    ]
        subsection ataLogo
          [linkIcon >> link "http://www.athinkingape.com/about" "athinkingape.com/about"
          ,locationIcon >> text "Vancouver"
          ,calendarIcon >> do
            tag "div" [] $ do
              tag "p" [] $ text "May 2013 - Aug. 2013"
              tag "p" [] $ text "Jan. 2014 - Aug. 2014"
          ,langIcon >> text "Objective C, Python, Javascript, Java"
          ,toolIcon >> text "iOS, Django, Android, GLES 2.0"
          ] Nothing $ do
            ulist [tag "div" [] $ bold (text "Frontend: ") >> text "Developed the iOS frontend of a prototype poker app focusing on playing with friends, which eventually became an app called Pineapple Poker."
                  ,tag "div" [] $ bold (text "Frontend: ") >> text "Developed frontend features for a 3D racing game on Android, including an interactive map, and the movement/drifting animation for the cars using GLES 2.0."
                  ,tag "div" [] $ bold (text "Backend: ") >> text "Built and improved analytics tools on the metrics team in order for them to filter information faster and be able to see a user age breakdown for specific days."
                  ]
        subsection (tag "h2" [] (text "eat.sleep.build"))
          [linkIcon >> link "https://eat.sleep.build" "eat.sleep.build"
          ,linkIcon >> link "https://github.com/TerranceN/TNiechciol.ca" "github.com/TerranceN/TNiechciol.ca"
          ,calendarIcon >> text "Nov. 2012 - present"
          ,langIcon >> text "Haskell"
          ] Nothing $ do
            tag "div" [] $ text "My website: a Haskell webapp running on top of lighttpd"
            ulist [tag "div" [] $ bold (text "Type Systems: ") >> text "Created a DSL using monads to programmatically compose HTML."
                  ,tag "div" [] $ bold (text "Web Fundamentals: ") >> text "Built without any web frameworks to better understand how other frameworks like Django and Rails work under the hood."
                  ]
        subsection (tag "h2" [] (text "asdf2"))
          [linkIcon >> link "https://github.com/TerranceN/asdf2" "github.com/TerranceN/asdf2"
          ,calendarIcon >> text "April 2016"
          ,langIcon >> text "C"
          ,toolIcon >> text "ncurses"
          ] Nothing $ do
            tag "div" [] $ text "A command line to quickly search through shell history."
            ulist [tag "div" [] $ bold (text "Iterative Design: ") >> text "Organically developed from a tiny " >> tag "span" [] (text "`history | grep $@`") >> text " script into a tool I use hundreds of times a day."
                  ,tag "div" [] $ bold (text "Always Improving: ") >> text "One of multiple tools I designed from the ground up to increase my productivity."
                  ]
        subsection (tag "h2" [] (text "Geometry Wars Clone"))
          [linkIcon >> link "https://eat.sleep.build/Projects/GeoWarsClone/" "eat.sleep.build/Projects/GeoWarsClone"
          ,videoIcon >> link "https://youtu.be/Xv-3VLCFOQM" "youtu.be/Xv-3VLCFOQM"
          ,calendarIcon >> text "Sept. 2013 - Dec. 2013"
          ,langIcon >> text "Scala"
          ,toolIcon >> text "LWJGL, OpenGL, GPGPU"
          ] Nothing $ do
            tag "div" [] $ text "A clone of the Xbox Live Arcade game Geometry Wars"
            ulist [tag "div" [] $ bold (text "Detail Oriented: ") >> text "Hundreds of thousands of particles/grid points are simulated on the GPU without slowdown. Basically a homemade GPGPU/Cuda system."
                  ,tag "div" [] $ bold (text "Juicy Design: ") >> text "Meticulously hand-tweaked music and animations to make the game feel satisfying to play."
                  ]
        subsection (tag "h2" [] (text "Other Projects"))
          [] Nothing $ do
            ulist [bold (text "Bamboo Bicycle: ") >> text "hand-built a bicycle frame out of bamboo with my girlfriend"
                  ,bold (text "CNC Machine: ") >> text "built a CNC Machine from 3d printed parts using designs on the web"
                  ,bold (text "Fermentation: ") >> text "experimenting with fermenting cucumbers (pickles) and honey (mead)"
                  ,bold (text "DIY WiFi Garage opener: ") >> text "ESP8266 controller with a relay and a laser-cut box"
                  ,bold (text "DIY Smart Lights: ") >> text "MITM attacked smart outlets + Kindle Fires as smart buttons"
                  ]
        --subsection (tag "h2" [] (text "ATA Co-op Hackathon Game"))
        --  [linkIcon >> link "https://eat.sleep.build/Projects/ATAHackathonGame/" "eat.sleep.build/Projects/ATAHackathonGame"
        --  ,videoIcon >> link "https://youtu.be/y7BLvpp1HlY" "youtu.be/y7BLvpp1HlY"
        --  ,calendarIcon >> text "A weekend in April 2014"
        --  ,langIcon >> text "Java"
        --  ,toolIcon >> text "libGDX, OpenGL"
        --  ] Nothing $ do
        --    ulist [tag "div" [] $ text "Built a 2D multiplayer deathmatch platformer for a 48-hour hackathon at A Thinking Ape with two other programmers, and two artists"
        --          ,tag "div" [] $ text "Responsible for movement and level collision, and graphical effects like bullet trails and the desaturation effect when phasing through walls"
        --          ,tag "div" [] $ text "Worked hyper-efficiently in a small focused team."
        --          ]
        -- subsection (tag "h2" [] (text "Deferred Renderer with SSAO"))
        --   [linkIcon >> link "https://eat.sleep.build/Projects/DeferredRenderer/" "eat.sleep.build/Projects/DeferredRenderer"
        --   ,videoIcon >> link "https://youtu.be/eJY72rMtFx4" "youtu.be/eJY72rMtFx4"
        --   ,calendarIcon >> text "Sept. 2014 - Dec. 2014"
        --   ,langIcon >> text "Scala"
        --   ,toolIcon >> text "LWJGL, OpenGL"
        --   ] Nothing $ do
        --     ulist [tag "div" [] $ text "Created a tech demo to learn how to implement some modern graphics techniques"
        --           ,tag "div" [] $ text "Implemented normal mapping and specular mapping to make walls look more detailed"
        --           ,tag "div" [] $ text "Implemented deferred rendering to efficiently render many lights"
        --           ,tag "div" [] $ text "Used depth information from the deferred rendering process to create a screen space approximation of ambient occlusion, to have light falloff more realistically in corners"
        --           ]
        subsection waterlooLogo
          [] Nothing $ do
            ulist [tag "div" [] $ text "Bachelor of Computer Science"
                  ,tag "div" [] $ text "Graduated December 2017"
                  ]
        tag "div" [("class", "clear")] $ noHtml

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
                script "/scripts/pdf_read_when_loaded.js"
                stylesheet "/styles/base_resume.css"
                stylesheet "/styles/resume_page.css"
            tag "body" [] $ do
                resume
  where
    head = do
        noHtml
    body = do
        noHtml

