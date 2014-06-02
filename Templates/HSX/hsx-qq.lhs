`hsx` `QuasiQuoter`
-------------------

Instead of using the `hsx2hs` preprocessor, we can use the `[hsx| |]` `QuasiQuoter`. We can take the code from the previous section and make three simple changes.

First we remove the `-F -pgmFhsx` pragma and enable the `QuasiQuotes` `LANGUAGE` extension instead.

> {-# LANGUAGE FlexibleContexts, OverlappingInstances, QuasiQuotes #-}
> module Main where
>

Next we have some imports:

> import Control.Applicative        ((<$>))
> import Control.Monad.Identity     (Identity(runIdentity))
> import Data.String                (IsString(fromString))
> import Data.Text                  (Text)
> import HSP
> import HSP.Monad                  (HSPT(..))
> import Happstack.Server.HSP.HTML
> import Happstack.Server.XMLGenT
> import Happstack.Server           ( Request(rqMethod), ServerPartT
>                                   , askRq, nullConf, simpleHTTP
>                                   )

The second change is to import the `hsx` `QuasiQuoter`:

> import Language.Haskell.HSX.QQ    (hsx)
>

The third change is to wrap the XML markup inside of a `[hsx| |]`:

> hello :: ServerPartT IO XML
> hello = unHSPT $ unXMLGenT
>  [hsx|
>   <html>
>    <head>
>     <title>Hello, HSP!</title>
>    </head>
>    <body>
>     <h1>Hello HSP!</h1>
>     <p>We can insert Haskell expression such as this:
>         <% show $ sum [1 .. (10 :: Int)] %></p>
>     <p>We can use the ServerPartT monad too.
>        Your request method was: <% getMethod %></p>
>     <hr/>
>     <p>We don't have to escape & or >. Isn't that nice?</p>
>     <p>If we want <% "<" %> then we have to do something funny.</p>
>     <p>But we don't have to worry about
>        escaping <% "<p>a string like this</p>" %></p>
>     <p>We can also nest <% <span>like <% "this." %> </span> %></p>
>    </body>
>   </html>
>  |]
>       where
>       getMethod :: XMLGenT (HSPT XML (ServerPartT IO)) String
>       getMethod = show . rqMethod <$> askRq
>

The `main` function is unaltered.

> main :: IO ()
> main = simpleHTTP nullConf $ hello
>

As a quick aside -- in the `hello` example, the `getMethod` function gives the type checker enough information to infer the type of the XML generated by the `[hsx| |]` quasiquoter. If you comment out the call to `getMethod` you will get an ambiguous type error message. One way to fix this by adding an explicit type signature to the closing html tag like:

    </html>  :: XMLGenT (HSPT XML (ServerPartT IO)) XML

In practice, we often create a page template function, similar to `defaultTemplate` which already contains the `<html>`, `<head>`, and `<body>` tags and also provides the extra type information needed.

The `HSPT` monad itself is covered in a later section.

~~~~ {.source-goes-here}
~~~~
