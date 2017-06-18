module Blog
( handlers
) where

import Data.Ord
import Data.List
import Data.Time
import PageTypes
import PageStructure

blogLayout :: Html -> Html -> String -> IO Response
blogLayout extraHead blogContent titleText = mainLayout head body [("section", "blog")]
  where
    head = do
      title titleText
      stylesheet "/styles/blog.css"
      extraHead
    body = do
      tag "h1" [] $ text titleText
      blogContent


blogSummaryPage urlOptions request = blogLayout noHtml body "Blog"
  where
    sortedBlogPosts = sortBy (comparing blogDate) blogPosts
    last5Blogs = (take 5) . reverse $ sortedBlogPosts
    makeBlogLink blogPost = do
      tag "div" [] $ do
        link ("/Blog/" ++ blogUrl blogPost) (blogTitle blogPost)
    body = do
      mapM_ makeBlogLink last5Blogs

loremIpsum = do
  tag "p" [] $ sentences
    [text "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    ,text "Vivamus semper purus non neque imperdiet laoreet."
    ,text "Donec in nisl in elit lacinia tempor."
    ,text "Fusce efficitur diam nec ligula interdum eleifend."
    ,text "Pellentesque tincidunt viverra elit, et tincidunt neque volutpat a."
    ,text "Vestibulum feugiat ultricies pretium."
    ,text "Pellentesque enim tortor, feugiat vel dignissim in, dictum non tortor."
    ,text "Vestibulum vehicula leo vitae nunc pellentesque, vitae pretium leo eleifend."
    ,text "Praesent suscipit ipsum eu elit vestibulum viverra."
    ]
  canvasDemo "Some description of the demo." $ do
    image "/blog_assets/graphics_for_the_practical_man/rotate.png" "Rotation"
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/star_at_rot.js"
  tag "p" [] $ sentences
    [text "Ut hendrerit consequat purus, nec tincidunt ligula auctor eget."
    ,text "Nullam iaculis pharetra magna, at egestas purus luctus vitae."
    ,text "Sed sit amet mattis felis."
    ,text "Pellentesque nunc nisl, volutpat sed lorem a, condimentum dictum velit."
    ,text "Ut auctor dignissim mollis."
    ,text "Aliquam sodales arcu rutrum mi tempus commodo."
    ,text "Mauris tristique dui urna, eu commodo nibh mollis vitae."
    ,text "Sed tristique lectus in eros dignissim aliquam."
    ,text "Fusce ornare, ipsum ut efficitur suscipit, nulla elit mollis ligula, vel sagittis quam risus ut ipsum."
    ]
  canvasDemo "Some description of the demo." $ do
    image "/blog_assets/graphics_for_the_practical_man/rotate.png" "Rotation"
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/star_at_rot.js"
  tag "p" [] $ sentences
    [text "Vestibulum sodales semper nunc sed eleifend."
    ,text "Donec sit amet mauris sit amet ligula convallis sodales."
    ,text "Etiam rhoncus venenatis lectus nec rhoncus."
    ,text "Cras non libero vel ligula hendrerit faucibus."
    ,text "Integer non enim erat."
    ,text "Aenean ultrices ipsum feugiat, faucibus libero eu, interdum tellus."
    ,text "Sed et pulvinar velit, ac hendrerit elit."
    ,text "Praesent fringilla neque vitae sem molestie, vitae tempor libero dictum."
    ,text "Donec ac tristique libero."
    ,text "Fusce nec diam vel ex mattis aliquam quis sit amet nisi."
    ,text "In sit amet velit a augue posuere interdum in ut purus."
    ]
  canvasDemo "Some description of the demo." $ do
    image "/blog_assets/graphics_for_the_practical_man/rotate.png" "Rotation"
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/star_at_rot.js"
  tag "p" [] $ sentences
    [text "Nullam id auctor felis."
    ,text "Duis eleifend interdum dolor, eget porttitor risus."
    ,text "Sed malesuada mi eget augue laoreet pretium."
    ,text "Pellentesque et risus ac elit gravida sollicitudin."
    ,text "Maecenas in erat egestas, pulvinar purus vel, hendrerit ex."
    ,text "Praesent at enim porta, posuere lacus nec, semper lectus."
    ,text "Vestibulum finibus sem nibh, eget tincidunt lorem cursus varius."
    ,text "Mauris tristique vestibulum sapien at tristique."
    ,text "Quisque egestas non magna sed finibus."
    ,text "Sed convallis lorem eu ligula dignissim malesuada vitae quis tellus."
    ,text "Praesent tellus eros, vehicula eget nibh in, porttitor vestibulum lacus."
    ]
  canvasDemo "Some description of the demo." $ do
    image "/blog_assets/graphics_for_the_practical_man/rotate.png" "Rotation"
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/star_at_rot.js"
  tag "p" [] $ sentences
    [text "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    ,text "Donec volutpat ligula interdum varius luctus."
    ,text "Sed turpis est, fringilla eu vulputate nec, egestas quis ipsum."
    ,text "Etiam fermentum tempor vehicula."
    ,text "Duis consectetur dapibus diam, ac efficitur odio ullamcorper in."
    ,text "Nam sed congue magna."
    ,text "Nunc tristique tincidunt convallis."
    ,text "Cras suscipit accumsan quam, sed auctor sapien efficitur sit amet."
    ,text "Phasellus ut varius erat."
    ,text "Nunc tempus libero sed justo dignissim, sed cursus dolor aliquet."
    ,text "Praesent egestas lectus est."
    ,text "Integer lobortis quam a augue vestibulum, sit amet maximus tellus ultrices."
    ,text "Mauris laoreet congue mauris, bibendum imperdiet est rutrum sit amet."
    ,text "Proin sem purus, rutrum non urna efficitur, porta mollis lectus."
    ]
  canvasDemo "Some description of the demo." $ do
    image "/blog_assets/graphics_for_the_practical_man/rotate.png" "Rotation"
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/star_at_rot.js"

canvasDemo description contents = do
  tag "div" [("class", "canvas_demo")] $ do
    tag "div" [("class", "demo_contents")] $ do
      tag "div" [("class", "demo_contents_cell")] $ do
        contents
    tag "p" [] $ do
      tag "i" [] $ do
        text description

canvasDemoCanvas src = 
  tag "div" [] $ do
    tag "canvas" [("src", src)
                 ,("width", "200")
                 ,("height", "200")
                 ] noHtml
    tag "a" [("href", src), ("target", "_blank")] $ text "View source"


exampleBlogEntry = blogLayout head $ do
  tag "p" [] $ do
    sentences
      [text "This is a test post, please ignore it."
      ,text "I'm making it one sentence at a time."
      ]
  loremIpsum
  where
    head = do
      script "/scripts/jquery-2.2.0.min.js"
      script "/scripts/jquery.inview.min.js"
      script "/blog_assets/CanvasDemo.js"
      stylesheet "/blog_assets/CanvasDemo.css"

exampleBlogEntry2 = blogLayout noHtml $ do
  tag "p" [] $ text "This is different."
  loremIpsum


data BlogPost = BlogPost
  { blogTitle :: String
  , blogUrl :: String
  , blogDate :: Day
  , blogPage :: String -> IO Response
  }

blogPosts :: [BlogPost]
blogPosts =
    [BlogPost
      "Test Blog, Please Ignore"
      "example_blog"
      (fromGregorian 2017 01 17)
      exampleBlogEntry
    ,BlogPost
      "Test Blog2, Please Ignore"
      "example_blog2"
      (fromGregorian 2017 01 20)
      exampleBlogEntry2
    ]

blogLinks = map makeBlogLink blogPosts
  where
    makeBlogLink blogPost = (exactly ("/" ++ (blogUrl blogPost) ++ "/"), \_ _ -> (blogPage blogPost) (blogTitle blogPost))

handlers :: [Handler]
handlers =
    [(exactly "/", blogSummaryPage)
    ] ++ blogLinks
