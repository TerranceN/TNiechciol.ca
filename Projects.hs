module Projects
( handlers
) where

import PageTypes
import PageStructure

projectPreview name link image description = do
    tag "div" [("class", "preview")] $ do
        tag "div" [("class", "image")] $ do
            screenshot image "preview image"
        tag "div" [("class", "content")] $ do
            tag "h2" [] $ do
                text name
            tag "hr" [] noHtml
            tag "div" [("class", "description")] $ do
                description
                button ("/Projects" ++ link) "Go to project page"

projectLayout head body =
    mainLayout (customHead >> head) (body >> popup)
  where
    customHead = do
        script "/scripts/jquery-2.2.0.min.js"
        script "/scripts/screenshots.js"
        stylesheet "/styles/project.css"
    popup = do
        tag "div" [("class", "overlay")] noHtml
        tag "div" [("class", "popup_container")] $ do
            tag "div" [("class", "popup_content")] noHtml

projectsPage urlOptions request =
    projectLayout head content
  where
    head = do
        title "Projects"
    content = do
        projectPreview "Geometry Wars Clone" "/GeoWarsClone" "/images/geometry_wars_clone_v2_small.png" $ do
            tag "p" [] $ text "A clone of the Xbox Live Arcade game Geometry Wars. A 2D top down shooter on a deformable grid. The grid and particle effects are computed on the GPU using an old-school style of GPGPU using OpenGL textures and shaders."
        tag "hr" [("class", "preview_separator")] noHtml
        projectPreview "ATA Hackathon Game" "/ATAHackathonGame" "/images/ata_hackathon_game_merged_small.png" $ do
            tag "p" [] $ text "A 2D arena shooter platformer made during a 48-hour hackathon while working at A Thinking Ape. It features wall-jumping, powerups, and the ability to temporarily remove walls in a circle around you for getting the drop on an enemy, or to shoot through walls."
        tag "hr" [("class", "preview_separator")] noHtml
        projectPreview "Defered Renderer" "/DeferedRenderer" "/images/defered_renderer_small.png" $ do
            tag "p" [] $ text "A 3D rendering engine that implements real-time bump mapping, defered rendering, and ambient occlusion. Using the depth information generated during the defered rendering process, occlusion information for each pixel is calculated in screen-space using a randomly oriented sampling of nearby points."
        tag "hr" [("class", "preview_separator")] noHtml
        projectPreview "Geometry Wars Clone (old)" "/GeoWarsCloneOld" "/images/GWClone_dragon.png" $ do
            tag "p" [] $ text "An old attempt at making a Geometry Wars clone. Particles and grid effects are implemented on the CPU and are rendered with OpenGL 1.0's glBegin and glEnd. The outdated technologies used is what motivated creating a new version."
        tag "hr" [("class", "preview_separator")] noHtml
        projectPreview "2D Lighting Demo" "/LightingDemo" "/images/LightingDemo1_web.png" $ do
            tag "p" [] $ text "A lighting demo to test the creation of 2D shadows. Shadows are generated by calculating the geometry of the shadows and then using it to create a mask for each light."
        tag "hr" [("class", "preview_separator")] noHtml
        projectPreview "Paradox Tower" "/ParadoxTower" "/images/paradox_tower.png" $ do
            tag "p" [] $ text "A top-down 2D adventure game created during the Global Game Jam, a 48-hour game jam."
        tag "hr" [("class", "preview_separator")] noHtml
        projectPreview "Spring Physics Demo" "/SpringPhysics" "/images/spring_physics.png" $ do
            tag "p" [] $ text "An attempt to create a game about a living ball of slime that can change the surface tension of its skin in order to jump. This uses a collection of springs to simulate the ball of slime. Unfortunately Euler integration was used, which lead to some instability in the slime ball."

geometryWarsPage urlOptions request =
    projectLayout head content
  where
    head =
        title "Geometry Wars Clone"
    content = do
        tag "h1" [] $ text "Geometry Wars Clone"
        tag "hr" [] noHtml
        youtube_video "Xv-3VLCFOQM" 560 315
        projectSection "Description" $ do
            tag "p" [] $ do
                text "A clone of the game "
                link "http://en.wikipedia.org/wiki/Geometry_Wars" "Geometry Wars"
                text " including a bloom effect, a huge number of particles, and a deformable grid (also implemented as particles). "
                text "Uses framebuffers to store particle position and velocity, allowing all particle simulation to be done on the GPU using shaders."
            tag "h3" [] $ text "Particle Simulation"
            tag "p" [] $ text "Particles and the deformable grid are simulated using an old-school style GPGPU technique. Particles' positions and velocities are stored as textures on the GPU where the index of each pixel represents each particle, and the red and green values represent the x and y components of each vector. To find the acceleration for each particle is simply the direction towards its initial position, but the process for the grid is a bit more complex."
            tag "p" [] $ text "For the grid, each intersection point is simulated as a particle. In order to have multiple pull and push effects on the grid, an acceleration texture is generated in screen space, where each pixel represents the acceleration that a particle at that position would experience."
            tag "p" [] $ text "Using the previous positions and velocities of the particles, and the generated acceleration texture, an update shader takes the old position, uses it to look up the acceleration at that position, and then calculates a new position and velocity and stores it in the appropriate textures. Because each update operation only looks at a single pixel, the same textures are used for both input and output."
        projectSection "Screenshots" $ do
            screenshot "/images/geometry_wars_clone_v2_small.png" "Particles everywhere!"
        projectSection "Downloads" $ do
            button "https://github.com/TerranceN/Geometry-Wars-Clone-V2" "Source"

geometryWarsOldPage urlOptions request =
    projectLayout head content
  where
    head =
        title "Geometry Wars Clone (old version)"
    content = do
        tag "h1" [] $ text "Geometry Wars Clone (old version)"
        tag "hr" [] noHtml
        youtube_video "2-HFsanORGw" 560 315
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
        projectSection "Screenshots" $ do
            screenshot "/images/GWClone_grid.png" "Grid Effects"
            screenshot "/images/GWClone_dragon.png" "Dragons!"
        projectSection "Downloads" $ do
            tag "p" [] $ text "Doesn't currently work on Macs, sorry!"
            button "/files/Geometry_Wars_Clone.zip" "JAR"
            button "https://github.com/TerranceN/GeometryWarsClone" "Source"

ataHackathonPage urlOptions request =
    projectLayout head content
  where
    head =
        title "ATA Hackathon Game"
    content = do
        tag "h1" [] $ text "ATA Hackathon Game"
        tag "hr" [] noHtml
        youtube_video "y7BLvpp1HlY" 560 315
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
            screenshot "/images/ata_hackathon_game_merged_small.png" "Game"
        projectSection "Downloads" $ do
            button "https://github.com/TerranceN/ATAHackathonW14" "Source"

deferedRendererPage urlOptions request =
    projectLayout head content
  where
    head =
        title "Defered Renderer"
    content = do
        tag "h1" [] $ text "Defered Renderer"
        tag "hr" [] noHtml
        youtube_video "eJY72rMtFx4" 560 315
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
            screenshot "/images/defered_renderer_small.png" "Game"
        projectSection "Downloads" $ do
            button "https://github.com/TerranceN/Deferred-Renderer" "Source"

springPhysicsPage urlOptions request =
    projectLayout head content
  where
    head =
        title "Spring Physics Demo"
    content = do
        tag "h1" [] $ text "Spring Physics Demo"
        tag "hr" [] noHtml
        youtube_video "OS00DNj0GgI" 560 315
        projectSection "Description" $ do
            tag "p" [] $ do
                text "A physics simulation of a blob of springs that can control its own springiness."
            tag "p" [] $ do
                text "Created using C++ and "
                link "http://www.sfml-dev.org/" "SFML."
        projectSection "Downloads" $ do
            button "/files/SpringPhysicsW32.zip" "Windows"
            button "/files/SpringPhysicsLinux.zip" "Linux"
            button "https://github.com/TerranceN/SpringPhysicsDemo" "Source"

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
            tag "p" [] $ text "Doesn't currently work on Macs, sorry!"
            button "/files/2DShadows.zip" "JAR"
            button "https://github.com/TerranceN/2DLightingDemo" "Source"

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

handlers :: [Handler]
handlers =
    [(exactly "/", projectsPage)
    ,(exactly "/geowarsclone/", geometryWarsPage)
    ,(exactly "/geowarscloneold/", geometryWarsOldPage)
    ,(exactly "/atahackathongame/", ataHackathonPage)
    ,(exactly "/deferedrenderer/", deferedRendererPage)
    ,(exactly "/springphysics/", springPhysicsPage)
    ,(exactly "/lightingdemo/", lightingDemoPage)
    ,(exactly "/paradoxtower/", paradoxTowerPage)
    ]
