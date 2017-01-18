Website
=======

This is my personal website, hosted at [eat.sleep.build](https://eat.sleep.build). It used to be at [TNiechciol.ca](http://TNiechciol.ca), but my last name is hard to spell, so I switched to an easier-to-spell domain name.

My website is built with [Haskell](http://www.haskell.org/haskellwiki/Haskell) and runs on top of [lighttpd](https://www.lighttpd.net/).

Html Generation
---------------

Using a State monad, I have been able to write html as Haskell code instead of injecting values into templates:

```haskell
-- I am using DLists to ensure that string appending is done right assiciatively.
--   (a ++ (b ++ c)) instead of ((a ++ b) ++ c)
-- The IO monad is added as well so that I can do things like cache busting based on the current commit hash
--   (see https://github.com/TerranceN/TNiechciol.ca/commit/77cfabdf9fa7e10a72ac7dc710f1196206d76ff4).
type Html = StateT (DList Char) IO ()

uText :: String -> Html
uText str = modify (\x -> x `append` (fromList str))

-- Note that encode encodes unsafe characters.
text :: String -> Html
text = uText . encode
```

Another nice benefit is that due to the laziness of Haskell, monadic values (like values of the Html type) can be passed around before they are evaluated, allowing you to inject Html values into functions, that can then place those values where appropriate. One common example of this is a `tag` function:

```haskell
tag :: String -> [Option] -> Html -> Html
tag name options contents = do
    uText "<" >> uText name >> (mapM renderOption options) >> uText ">"
    contents
    uText "</" >> uText name >> uText ">"
  where
    renderOption (key, value) = do
        uText " "
        uText key
        uText "=\""
        uText value
        uText "\""
```

You can then create a page like this:

```haskell
examplePage :: Html
examplePage = do
    uText "Content-Type: text/html\n"
    uText "\n"
    uText "<!DOCTYPE html>"
    tag "html" [] $ do
        tag "head" [] $ tag "title" [] $ text "Example Page"
        tag "body" [] $ do
            tag "h1" [] $ text "Example Page"
            tag "p" [] $ text "Hello, world!"
```

Notice how I never need to take care of closing tags, it is done automatically.

Once again, passing monadic values is very convenient, this time for making templates:

```haskell
noHtml :: Html
noHtml = return ()

stylesheet :: String -> Html
stylesheet url = do
    tag "link" [("rel", "stylesheet")
               ,("type", "text/css")
               ,("href", url)] noHtml

pageTemplate :: Html -> Html -> Html
pageTemplate head content = do
    uText "Content-Type: text/html\n"
    uText "\n"
    uText "<!DOCTYPE html>"
    tag "html" [] $ do
        tag "head" [] $ do
            stylesheet "my_main_styles.css"
            head
        tag "body" [] $ do
            tag "div" [("class", "container")] content

examplePage :: Html
examplePage =
    pageTemplate head content
  where
    head = tag "title" [] $ text "Example Page"
    content = do
        tag "h1" [] $ text "Example Page"
        tag "p" [] $ text "Hello, world!"
```
