module Blog.GraphicsForThePracticalMan
( blogEntry
) where

import PageTypes
import PageStructure
import Blog.Common
import Blog.CanvasDemo
import Blog.CodeDemo
import Blog.GraphicsForThePracticalMan.CodeSamples

blogEntry = blogLayout head $ do
  tag "p" [] $ do
    sentences
      [text "The goal of this article is to give you a broad understanding of how 3D graphics work, "
        >> text "regardless of previous familiarity of the subject, and without getting into implementation details "
        >> text "that come with using OpenGL/DirectX/Vulcan etc."
      ,text "The only given is that we have a canvas to draw on that has pixels, and the rest will be developed in this article."
      ]
  tag "p" [] $ do
    text "From a simple " >> monotext "drawPixel" >> text " operation we'll develop a method of drawing shapes, "
      >> text "translating/rotating/scaling the shapes, and perspective projection, to finally end up a rotating cube:"
  canvasDemo (text "This article will teach you how to draw this cube from scratch.") $ do
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/finished.js"
  tag "h2" [] $ text "Rendering From The Ground Up"
  tag "p" [("class", "noindent")] $ do
    text "TODO"
  tag "h3" [] $ text "The Canvas"
  tag "h3" [] $ text "Triangles"
  tag "h3" [] $ text "Transformations"
  tag "h2" [] $ text "Matrices and Building an Intuitive Understanding of Them"
  tag "p" [] $ do
    sentences
      [text ""
      ,text "Luckily for us there's a simpler, composable way to represent transformations, the matrix."
      ,text "Matrices are based on the idea of finding new axes (think grid axes: X and Y) that represent the transformation."
      ,text "These axes are called " >> btext "basis vectors" >> text "."
      ,text "The " >> btext "standard basis vectors" >> text " you're probably used to seeing are "
        >> monotext "(1, 0)" >> text ", " >> monotext "(0, 1)" >> text ", and look like this:"
      ]
  canvasDemo (text "The standard basis vectors are just one unit in each direction.") $ do
    image "/blog_assets/graphics_for_the_practical_man/standard_basis.png" "The standard basis"
  tag "p" [] $ do
    sentences
      [text ""
      ,text "In case you're not familliar with the word, a " >> btext "vector"
        >> text " is just a pair of numbers (like a point) that represents an offset from the origin, "
        >> monotext "(0, 0)" >> text "."
      ,text "That's why it's drawn as an arrow from the origin above."
      ,text "You can add vectors component-by-component just like a pair of points (e.g. "
        >> monotext "(1, 2) + (3, 4) = (4, 6)" >> text "), and you can multiply a vector by a number "
        >> text "(scaling the vector) by multiplying its individual components by the number (e.g. "
        >> monotext "5 * (1, 2) = (5, 10)" >> text ")."
      ,text "You can also think of points as vectors (since their position is relative to "
        >> monotext "(0, 0)" >> text "), and so from this point on the two terms will be used mostly interchangably."
      ]
  tag "p" [] $ do
    tag "i" [] $ do
      sentences
        [text "Note: In this case we're using 2-dimensional vectors, but you can have vectors of any dimension (as we'll see later)."
        ,text "In general, an n-dimensional vector has n components."
        ]
  tag "h3" [] $ text "Change of Basis"
  tag "p" [] $ do
    sentences
      [text "So how can different basis vectors be used to transform a shape? To answer this we can think about what it means to draw something at some point, say " >> monotext "(5, 3)" >> text ", in terms of the standard basis vectors."
      ,text "The x-value, 5, of that point represents how far along the x-axis, which is defined by the standard x-axis basis vector "
        >> monotext "(1, 0)" >> text ", and similarly the y-value, 3, represents how far along the y-axis, "
        >> text "defined by the standard y-axis basis vector " >> monotext "(0, 1)" >> text "."
      ,text "So to figure out where to draw that point we can do:"
      ]
  tag "div" [] $ do
    monopara "  5 * (1, 0) + 3 * (0, 1)"
    monopara "= (5, 0) + (3, 0)"
    monopara "= (5, 3)"
  canvasDemoInteractive (do
      text "We add " >> monospan (tag "b" [("class", "xtext")] (text "5")) >> text " of the x basis vector, "
        >> monotext "(1, 0)" >> text", and " >> monospan (tag "b" [("class", "ytext")] (text "3")) >> text " of the y basis vector "
        >> monotext "(0, 1)" >> text ", to get " >> monospan (tag "b" [("class", "finished_text")] (text "(5, 3)")) >> text "."
    )(do
      canvasDemoCanvasNoLink "/blog_assets/graphics_for_the_practical_man/grid_five_three.js"
    )
  tag "p" [] $ do
    sentences 
      [text "This process answers the question \"What is " >> monotext "(5, 3)" >> text " in the basis of "
        >> monotext "basis-x" >> text ", " >> monotext "basis-y" >> text ", when converted to the standard basis?\"."
      ,text "Since we're using the standard basis vectors for " >> monotext "basis-x" >> text " and " >> monotext "basis-y"
        >> text ", the basis we're going from and to are the same, so we end up with the same point."
      ,text "This process becomes more clear when we change what basis vectors we use."
      ,text ""
      ]
  tag "div" [] $ do
    tag "p" [("class", "nospacing")] $ do
      text "For example, if we use the basis vectors "
        >> monotext "(1, 0)" >> text ", " >> monotext "(0, 2)" >> text ", we end up with"
    tag "div" [] $ do
      monopara "  5 * (1, 0) + 3 * (0, 2)"
      monopara "= (5, 0) + (0, 6)"
      monopara "= (5, 6)"
    tag "p" [("class", "nospacing")] $ do
      text "doubling the y-value."
  canvasDemoInteractive (do
      sentences
        [text "If we change the basis vectors, we can transform where the point is drawn."
        ,text "We add " >> monospan (tag "b" [("class", "xtext")] (text "5")) >> text " of the x basis vector, "
          >> monotext "(1, 0)" >> text", and " >> monospan (tag "b" [("class", "ytext")] (text "3")) >> text " of the y basis vector "
          >> monotext "(0, 2)" >> text ", to get " >> monospan (tag "b" [("class", "finished_text")] (text "(5, 6)")) >> text "."
        ]
    )(do
      canvasDemoCanvasNoLink "/blog_assets/graphics_for_the_practical_man/grid_five_three_scaled.js"
    )
  tag "p" [] $ do
    sentences
      [text "This finds out what "
        >> monotext "(5, 3)" >> text " in this alternate basis"
        >> text " is, in terms of the standard basis vectors, which is " >> monotext "(5, 6)" >> text "."
      ,text "This makes sense because for every y-basis vector in the "
        >> monotext "(1, 0)" >> text ", " >> monotext "(0, 2)" >> text " basis, you need two y-basis vectors in the "
        >> monotext "(1, 0)" >> text ", " >> monotext "(0, 1)" >> text " basis."
      ,text "And so the resulting y-value is doubled."
      ,text "This process of converting a point defined in one basis to another is called a " >> btext "change of basis" >> text "."
      ]
  tag "p" [] $ do
    text "Now, if we draw the star, using the basis from before, " >> monotext "(1, 0)" >> text ", "
      >> monotext "(0, 2)" >> text ", when drawing each point, we end up scaling the star so that it's twice as high as it is wide:"
  canvasDemo (text "Using a basis with double the y-value scales the star to be twice as tall.") $ do
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/star_scaled_basis.js"
  codeDemoCollapsableJS transformBasisText
  tag "p" [("class", "noindent")] $ do
    text "We can find basis vectors to do many different transformations, such as..."
  tag "p" [("class", "noindent")] $ do
    text "Scaling:"
  canvasDemo (text "Cool! We can make things different sizes!") $ do
    canvasDemoCanvasNoLink "/blog_assets/graphics_for_the_practical_man/grid_scaling.js"
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/scaling.js"
  tag "p" [("class", "noindent")] $ do
    text "Rotating:"
  canvasDemo (text "Cool! We can spin things!") $ do
    canvasDemoCanvasNoLink "/blog_assets/graphics_for_the_practical_man/grid_rotating.js"
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/rotating.js"
  tag "p" [("class", "noindent")] $ do
    text "Shearing:"
  canvasDemo (text "Shear? Why would we ever need to do this?") $ do
    canvasDemoCanvasNoLink "/blog_assets/graphics_for_the_practical_man/grid_shearing.js"
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/shearing.js"
  tag "h3" [] $ text "Composing Multiple Bases"
  tag "p" [] $ do
    text "The great strength of working with bases is that you can compose them to come up with new bases that represent "
      >> itext "both" >> text " transformations."
  tag "p" [] $ do
    sentences
      [text "In the same way you can use basis vectors to transform a point, you can do the same thing to transform a vector."
      ,text "Combining transformation A with B, is applying A's transformation on each of B's basis vectors."
      ]
  tag "p" [("class", "noindent")] $ do
    text "For example, we can draw the star by combining a rotation and a scale:"
  canvasDemo (text "If we scale and then rotate, we get a rotating star that's twice as tall.") $ do
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/combined.js"
  codeDemoCollapsableJS combineBasisText
  tag "p" [] $ do
    sentences
      [text "One thing to note is that the order of composing matters."
      ,text "Formally, we would say combining transformations isn't "
        >> btext "commutative"
        >> text ", which means combining two transformations in one order is different than combining the same transformations in the opposite order."
      ]
  tag "p" [] $ do
    text "Here's the same transformations combined in the opposite order:"
  canvasDemo (text "If we switch the order, the size of the star changes as it rotates.") $ do
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/combined_reversed.js"
  tag "p" [] $ do
    sentences
      [text "Now, instead of having to compute our full transformation stack on each point when we draw it, "
        >> text "we can combine all our transformations into one set of bases that we can use to transform each point."
      ,text "This changes our runtime from " >> monotext "max_number_of_tranformations * number_of_points"
        >> text " to " >> monotext "max_number_of_transformations + number_of_points"
        >> text ", which for a complicated game with detailed assets is a " >> itext "massive" >> text " improvement."
      ,text "But you may have noticed there's one transformation missing."
      ]
  tag "h3" [] $ text "The Problem With Translations"
  tag "p" [] $
    sentences
      [text "Translations have an inherent problem: no matter how you change the basis vectors, the origin never changes."
      ,text "Mathematically, this is because a change of basis operation is always a "
        >> btext "linear operation" >> text "."
      ,text "This means that it doesn't matter if you add two vectors together, or scale a vector, before or after the change of basis."
      ]
  tag "div" [] $ do
    tag "p" [("class", "nospacing")] $ do
      text "In equations:"
    codeDemoJS changeOfBasisLinearText
  tag "p" [("class", "noindent")] $ do
    text "But transformations " >> btext "don't" >> text " satisfy either of these equations."
  tag "div" [] $ do
    tag "p" [("class", "nospacing")] $ do
      text "Let's say we have a function " >> monotext "moveRight" >> text " that translates a point to the right one unit on the x axis:"
    codeDemoJS moveRightAddingText
    tag "p" [("class", "nospacing")] $ do
      text "and"
    codeDemoJS moveRightScalingText
  tag "p" [] $ do
    sentences
      [text "So if any change of basis is a linear operation, and translations aren't linear operations, is there any way to store translations as a change of basis? Mathematically, no."
      ,text "But there's a common work-around to store translations..."
      ]
  tag "h3" [] $ text "Shear to the Rescue"
  tag "p" [("class", "noindent")] $ do
    text "The idea behind this work-around is in our earlier shear example:"
  canvasDemo (text "Our shear example from earlier.") $ do
    canvasDemoCanvasNoLink "/blog_assets/graphics_for_the_practical_man/grid_shearing.js"
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/shearing.js"
  tag "p" [] $ do
    text "The idea comes from noticing that when shearing the image in the x-direction, points along the the line "
      >> monotext "y = 0" >> text " (the x-axis) don't move, but points above the x-axis move, specficially points on the line "
      >> monotext "y = 1" >> text " (one unit above the x-axis) move exactly how much the image is sheared."
  tag "p" [] $ do
    sentences
      [text "If we were drawing something in one dimension (so just along the x-axis), "
        >> text "we could instead convert its points to be two dimensional points with a y-value of 1."
      ,text "If we then sheared the image in the x-direction, and then converted back to a one dimensional point "
        >> text "(by ignoring/throwing out the y-value), we would effectively be translating our object:"
      ]
  canvasDemoInteractive (text "Shearing along the x-axis using the mouse position makes points with a y-value of 1 directly follow the mouse. If we then remove the y-values, it's as if we're translating a line along the x-axis.") $ do
    canvasDemoCanvasNoLink "/blog_assets/graphics_for_the_practical_man/grid_translate_before.js"
    canvasDemoCanvasNoLink "/blog_assets/graphics_for_the_practical_man/grid_translate_after.js"
  tag "p" [] $ do
    sentences
      [text "If we take something we want to draw in one dimension, but represent it using two-dimensional vectors with the y-component always being 1, we can apply a two-dimensional shear transformation that only affects the x-direction (keeping the y-component as 1), then throw out the y-component to go back to a one-dimensional object."
      ,text "This will effectively translate the object, and since shearing is a linear operation, we can respresent the transformation using basis vectors."
      ]
  tag "p" [] $ do
    sentences
      [text "We can do the same thing for a two-dimensional object."
      ,text "This means using three-dimensional vectors where the z-component is always 1, and using three-dimensional basis vectors to store the transformations."
      ,text "Then translation is just applying a shear in the x and y directions based on the z-direction:"
      ]
  canvasDemoInteractive (text "Translation!") $ do
    canvasDemoCanvas "/blog_assets/graphics_for_the_practical_man/star_translate.js"
  codeDemoCollapsableJS translateUsingShearText
  tag "h3" [] $ text "Matrices"
  tag "h2" [] $ text "Drawing 3-Dimensional Objects"
  tag "h3" [] $ text "Perspective Projection"
  tag "h3" [] $ text "Depth Buffering"
  where
    head = do
      script "/scripts/jquery-2.2.0.min.js"
      script "/scripts/jquery.inview.min.js"
      script "/blog_assets/CanvasDemo.js"
      stylesheet "/blog_assets/CanvasDemo.css"
      script "/blog_assets/prism.js"
      stylesheet "/blog_assets/prism.css"

