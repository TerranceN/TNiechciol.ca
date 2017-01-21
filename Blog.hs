module Blog
( handlers
) where

import PageTypes
import PageStructure

blogLayout titleText blogContent = mainLayout head body [("section", "blog")]
  where
    head = do
      title titleText
      stylesheet "/styles/blog.css"
    body = do
      tag "h1" [] $ text titleText
      blogContent


blogSummaryPage urlOptions request = blogLayout "Test Blog, Please Ignore" body
  where
    body = do
      tag "p" [] $ text "This is a test post, please ignore it."
      tag "p" [] $ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus semper purus non neque imperdiet laoreet. Donec in nisl in elit lacinia tempor. Fusce efficitur diam nec ligula interdum eleifend. Pellentesque tincidunt viverra elit, et tincidunt neque volutpat a. Vestibulum feugiat ultricies pretium. Pellentesque enim tortor, feugiat vel dignissim in, dictum non tortor. Vestibulum vehicula leo vitae nunc pellentesque, vitae pretium leo eleifend. Praesent suscipit ipsum eu elit vestibulum viverra."
      tag "p" [] $ text "Ut hendrerit consequat purus, nec tincidunt ligula auctor eget. Nullam iaculis pharetra magna, at egestas purus luctus vitae. Sed sit amet mattis felis. Pellentesque nunc nisl, volutpat sed lorem a, condimentum dictum velit. Ut auctor dignissim mollis. Aliquam sodales arcu rutrum mi tempus commodo. Mauris tristique dui urna, eu commodo nibh mollis vitae. Sed tristique lectus in eros dignissim aliquam. Fusce ornare, ipsum ut efficitur suscipit, nulla elit mollis ligula, vel sagittis quam risus ut ipsum."
      tag "p" [] $ text "Vestibulum sodales semper nunc sed eleifend. Donec sit amet mauris sit amet ligula convallis sodales. Etiam rhoncus venenatis lectus nec rhoncus. Cras non libero vel ligula hendrerit faucibus. Integer non enim erat. Aenean ultrices ipsum feugiat, faucibus libero eu, interdum tellus. Sed et pulvinar velit, ac hendrerit elit. Praesent fringilla neque vitae sem molestie, vitae tempor libero dictum. Donec ac tristique libero. Fusce nec diam vel ex mattis aliquam quis sit amet nisi. In sit amet velit a augue posuere interdum in ut purus."
      tag "p" [] $ text "Nullam id auctor felis. Duis eleifend interdum dolor, eget porttitor risus. Sed malesuada mi eget augue laoreet pretium. Pellentesque et risus ac elit gravida sollicitudin. Maecenas in erat egestas, pulvinar purus vel, hendrerit ex. Praesent at enim porta, posuere lacus nec, semper lectus. Vestibulum finibus sem nibh, eget tincidunt lorem cursus varius. Mauris tristique vestibulum sapien at tristique. Quisque egestas non magna sed finibus. Sed convallis lorem eu ligula dignissim malesuada vitae quis tellus. Praesent tellus eros, vehicula eget nibh in, porttitor vestibulum lacus."
      tag "p" [] $ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec volutpat ligula interdum varius luctus. Sed turpis est, fringilla eu vulputate nec, egestas quis ipsum. Etiam fermentum tempor vehicula. Duis consectetur dapibus diam, ac efficitur odio ullamcorper in. Nam sed congue magna. Nunc tristique tincidunt convallis. Cras suscipit accumsan quam, sed auctor sapien efficitur sit amet. Phasellus ut varius erat. Nunc tempus libero sed justo dignissim, sed cursus dolor aliquet. Praesent egestas lectus est. Integer lobortis quam a augue vestibulum, sit amet maximus tellus ultrices. Mauris laoreet congue mauris, bibendum imperdiet est rutrum sit amet. Proin sem purus, rutrum non urna efficitur, porta mollis lectus."

handlers :: [Handler]
handlers =
    [(exactly "/", blogSummaryPage)
    ]
