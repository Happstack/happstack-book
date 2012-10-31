
`web-routes` and `HSP`
----------------------

You will need to install the optional `web-routes`, `web-routes-th`,
`web-routes-hsp` and `happstack-hsp` packages for this section.


> {-# LANGUAGE TemplateHaskell #-}
> {-# OPTIONS_GHC -F -pgmFtrhsx #-}
> module Main where
>
> import Control.Applicative ((<$>))
> import Happstack.Server
> import Happstack.Server.HSP.HTML
> import qualified HSX.XMLGenerator as HSX
> import Web.Routes
> import Web.Routes.TH
> import Web.Routes.XMLGenT
> import Web.Routes.Happstack


If you are using `web-routes` and `HSP` then inserting URLs is especially clean and easy. If we have the URL:

> data SiteURL = Monkeys Int deriving (Eq, Ord, Read, Show)
>
> $(derivePathInfo ''SiteURL)
>


Now we can define a template like this:

> monkeys :: Int -> RouteT SiteURL (ServerPartT IO) Response
> monkeys n =
>     do html <- defaultTemplate "monkeys" () $
>         <%>
>          You have <% show n %> monkeys.
>          Click <a href=(Monkeys (succ n))>here</a> for more.
>         </%>
>        ok $ (toResponse html)



Notice that in particular this bit:


~~~~ {.haskell}
<a href=(Monkeys (succ n))>here</a>
~~~~


We do not need `showURL`, we just use the URL type directly. That works because `Web.Routes.XMLGenT` provides an instance:



~~~~ {.haskell}
instance (Functor m, Monad m) => EmbedAsAttr (RouteT url m) (Attr String url)
~~~~


Here is the rest of the example:



> route :: SiteURL -> RouteT SiteURL (ServerPartT IO) Response
> route url =
>     case url of
>       (Monkeys n) -> monkeys n
>
> site :: Site SiteURL (ServerPartT IO Response)
> site = setDefault (Monkeys 0) $ mkSitePI (runRouteT route)
>
> main :: IO ()
> main = simpleHTTP nullConf $
>   msum [ dir "favicon.ico" $ notFound (toResponse ())
>        , implSite (pack "http://localhost:8000") empty site
>        ]


Source code for the app is [here](http://srclink/WebRoutesHSP.hs).

