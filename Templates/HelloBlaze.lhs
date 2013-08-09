
Using `blaze-html`
------------------

It is trivial to use [`blaze-html`](http://jaspervdj.be/blaze/) with
Happstack. Essentially you just use `toResponse` to convert a blaze
`Html` value into a `Response`. For more detailed information on using
`blaze-html`, see the [`blaze-html
website`](http://jaspervdj.be/blaze/). The following example should
get you started:

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Happstack.Server
> import           Text.Blaze ((!))
> import qualified Text.Blaze.Html4.Strict as H
> import qualified Text.Blaze.Html4.Strict.Attributes as A
>
> appTemplate :: String -> [H.Html] -> H.Html -> H.Html
> appTemplate title headers body =
>     H.html $ do
>       H.head $ do
>         H.title (H.toHtml title)
>         H.meta ! A.httpEquiv "Content-Type"
>                ! A.content "text/html;charset=utf-8"
>         sequence_ headers
>       H.body $ do
>         body
>
> helloBlaze :: ServerPart Response
> helloBlaze =
>    ok $ toResponse $
>     appTemplate "Hello, Blaze!"
>                 [H.meta ! A.name "keywords"
>                         ! A.content "happstack, blaze, html"
>                 ]
>                 (H.p $ do "Hello, "
>                           H.b "blaze-html!")
>
> main :: IO ()
> main = simpleHTTP nullConf $ helloBlaze

Source code for the app is [here](http://srclink/HelloBlaze.hs).

Now if we visit [http://localhost:8000/](http://localhost:8000/), we will get an HTML page which says:

`Hello,` **`blaze-html!`**

This example is pretty simple, but there are a few things to
note:

 * The `appTemplate` function is purely `blaze-html` code and is in no way Happstack specific.
 * The existence of the `appTemplate` is purely a stylistic choice.
 * I have found it useful to set the content-type meta tag.
 * Happstack will automatically set the HTTP header `Content-Type: text/html; charset=UTF-8`. (`blaze-html` only supports UTF-8)
