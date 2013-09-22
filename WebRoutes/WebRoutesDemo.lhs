
`web-routes` Demo
-----------------

Let's start by looking at a simple example of using `web-routes`. In this example we will use blaze for the HTML templates.

In order to run this demo you will need to install `web-routes`, `web-routes-th` and `web-routes-happstack` from hackage.

> {-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
>     TemplateHaskell #-}
> module Main where
>
> import Prelude                 hiding (head)
>
> import Control.Monad           (msum)
> import Data.Data               (Data, Typeable)
> import Data.Monoid             (mconcat)
> import Data.Text               (pack)
> import Happstack.Server
>     ( Response, ServerPartT, ok, toResponse, simpleHTTP
>     , nullConf, seeOther, dir, notFound, seeOther)
> import Text.Blaze.Html4.Strict
>     ( (!), html, head, body, title, p, toHtml
>     , toValue, ol, li, a)
> import Text.Blaze.Html4.Strict.Attributes (href)
> import Web.Routes
>     ( PathInfo(..), RouteT, showURL
>     , runRouteT, Site(..), setDefault, mkSitePI)
> import Web.Routes.TH           (derivePathInfo)
> import Web.Routes.Happstack    (implSite)
>


First we need to define the type to represent our routes. In this site we will have a homepage and articles which can be retrieved by their id.

> newtype ArticleId = ArticleId { unArticleId :: Int }
>     deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)
>
> data Sitemap
>     = Home
>     | Article ArticleId
>     deriving (Eq, Ord, Read, Show, Data, Typeable)
>


Next we use `template-haskell` to derive an instance of `PathInfo` for the `Sitemap` type.

> $(derivePathInfo ''Sitemap)
>



The `PathInfo` class is defined in `Web.Routes` and looks like this:


~~~~ {.haskell}
class PathInfo a where
  toPathSegments :: a -> [Text]
  fromPathSegments :: URLParser a
~~~~


It is basically a class that describes how to turn a type into a
URL and back. This class is semi-optional. Some conversion methods
such as `web-routes-th` and `web-routes-regular` use
it, but others do not.

Since `ArticleId` is just a `newtype` we were
able to just do `deriving PathInfo` instead of having
to call `derivePathInfo`.

Next we need a function that maps a route to the handlers:



> route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
> route url =
>     case url of
>       Home                -> homePage
>       (Article articleId) -> articlePage articleId
>



As you can see, mapping a URL to a handler is just a straight-forward case statement. We do not need to do anything fancy here to extract the article id from the URL, becuse that has already been done when the URL was converted to a `Sitemap` value.

You may be tempted to write the `route` function like this instead of using the case statement:


~~~~ {.haskell}
route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route Home                = homePage
route (Article articleId) = articlePage articleId
~~~~


But, I don't recommend it. In a real application, the `route` function will likely take a number of extra arguments such as database handles. Every time you add a parameter, you have to update every pattern match to account for the extra argument, even for the handlers that don't use it. Using a `case` statement instead makes the code easier to maintain and more readable in my opinion.

The other thing you will notice is the `RouteT` monad transformer in the type signature. The `RouteT` monad transformer is another semi-optional feature of `web-routes`. `RouteT` is basically a `Reader` monad that holds the function which converts the URL type into a string. At first, this seems unnecessary -- why not just call `toPathInfo` directly and skip `RouteT` entirely? But it turns out there are few advantages that `RouteT` brings:


 1. `RouteT` is parametrized by the URL type -- in this case `Sitemap`. That will prevent us from accidentally trying to convert an `ArticleId` into a URL. An `ArticleId` is a valid component of some URLs, but it is not a valid URL by itself.

 2. The URL showing function inside `RouteT` can also contain additional information needed to form a valid URL, such as the hostname name, port, and path prefix

 3. `RouteT` is also used when we want to embed a library/sub-site into a larger site.

We will see examples of these benefits as we continue with the tutorial.

Next, we have the handler functions:


> homePage :: RouteT Sitemap (ServerPartT IO) Response
> homePage = do
>   articles <- mapM mkArticle [(ArticleId 1) .. (ArticleId 10)]
>   ok $ toResponse $
>     html $ do
>       head $ title $ (toHtml "Welcome Home!")
>       body $ do
>         ol $ mconcat articles
>   where
>     mkArticle articleId =
>         do url <- showURL (Article articleId)
>            return $ li $ a ! href (toValue url) $
>               toHtml $ "Article " ++ (show $ unArticleId articleId)
>


> articlePage :: ArticleId -> RouteT Sitemap (ServerPartT IO) Response
> articlePage (ArticleId articleId) = do
>   homeURL <- showURL Home
>   ok $ toResponse $
>      html $ do
>        head $ title $ (toHtml $ "Article " ++ show articleId)
>        body $ do
>         p $ do toHtml $ "You are now reading article "
>                toHtml $ show articleId
>         p $ do toHtml "Click "
>                a ! href (toValue homeURL) $ toHtml "here"
>                toHtml " to return home."
>



Even though we have the `RouteT` in the type signature -- these functions look normal `ServerPartT` functions -- we do not have to use `lift` or anything else. That is because `RouteT` is a instance of all the `Happstack` class and the related classes such as `ServerMonad,` `FilterMonad,` etc. Though you do need to make sure you have imported `Web.Routes.Happstack` to get those instances.

The only new thing here is the `showURL` function, which has the type:


~~~~ {.haskell}
showURL :: ShowURL m => URL m -> m Text
~~~~


`showURL` converts a url type, such as `Sitemap` into `Text` that we can use as an attribute value for an `href`, `src`, etc.

`URL m` is a type-function that calculates the type based on the monad we are currently in. For `RouteT url m a`, `URL m` is going to be whatever `url` is. In this example, `url` is `Sitemap`. If you are not familiar with type families and type functions, see section of `web-routes and type-families`.

Now we have:

 1. A url type, `Sitemap`

 2. functions to convert the type to a string and back via `PathInfo`

 3. a function to route the url to a handler, `route`


We need to tie these three pieces together. That is what the `Site` type does for us:


~~~~ {.haskell}
data Site url a = Site {
   -- | function which routes the url to a handler
   handleSite         :: (url -> [(Text, Text)] -> Text) -> url -> a
   -- | This function must be the inverse of 'parsePathSegments'.
 , formatPathSegments :: url -> ([Text], [(Text, Text)])
   -- | This function must be the inverse of 'formatPathSegments'.
 , parsePathSegments  :: [Text] -> Either String url
}
~~~~


Looking at the type for `Site`, we notice that it is very general -- it does not have any references to `Happstack`, `PathInfo`, `URLParser`, `RouteT`, etc. That is because those are all addons to the core of `web-routes`. We can convert our `route` to a `Site` using some simple helper functions like this:


> site :: Site Sitemap (ServerPartT IO Response)
> site =
>        setDefault Home $ mkSitePI (runRouteT route)
>



`runRouteT` removes the `RouteT` wrapper from our routing function:


~~~~ {.haskell}
runRouteT :: (url -> RouteT url m a)
          -> ((url -> [(Text, Text)] -> Text) -> url -> m a)
~~~~


So if we have our routing function like:


~~~~ {.haskell}
route :: Sitemap
      -> RouteT Sitemap (ServerPartT IO) Response
~~~~


`runRouteT` will convert it to a function that takes a url showing function:


~~~~ {.haskell}
(runRouteT route) :: (Sitemap -> [(Text, Text)] -> Text)
                  -> Sitemap
                  -> ServerPartT IO Response
~~~~


Since we created a `PathInfo` instance for
`Sitemap` we can use `mkSitePI` to convert the
new function to a `Site`. `mkSitePI` has the type:


~~~~ {.haskell}
mkSitePI :: (PathInfo url) =>
            ((url -> [(Text, Text)] -> Text) -> url -> a)
         -> Site url a
~~~~


so applying it to `runRouteT route` gives us:


~~~~ {.haskell}
(mkSitePI (runRouteT route)) :: Site Sitemap (ServerPartT IO Response)
~~~~


`setDefault` allows you to map `/` to any route you want. In this example we map `/` to `Home`.


~~~~ {.haskell}
setDefault :: url -> Site url a -> Site url a
~~~~


Next we use `implSite` to embed the `Site` into a normal Happstack route:


> main :: IO ()
> main = simpleHTTP nullConf $ msum
>   [ dir "favicon.ico" $ notFound (toResponse ())
>   , implSite (pack "http://localhost:8000") (pack "/route") site
>   , seeOther "/route" (toResponse ())
>   ]
>

The type for `implSite` is straight-forward:


~~~~ {.haskell}
implSite :: (Functor m, Monad m, MonadPlus m, ServerMonad m) =>
            Text           -- ^ "http://example.org"
         -> FilePath       -- ^ path to this handler, .e.g. "/route"
         -> Site url (m a) -- ^ the 'Site'
         -> m a
~~~~


The first argument is the domain/port/etc that you want to add to the beginning of any URLs you show. The first argument is not used during the decoding/routing process -- it is merely prepended to any generated url strings.

The second argument is the path to this handler. This path is automatically used when routing the incoming request and when showing the URL. This path can be used to ensure that all routes generated by `web-routes` are unique because they will be in a separate sub-directory (aka, a separate namespace). If you do not want to put the routes in a separate sub-directory you can set this field to `""`.

The third argument is the `Site` that does the routing.

If the URL decoding fails, then `implSite` will call `mzero`.

Sometimes you will want to know the exact parse error that caused the router to fail. You can get the error by using `implSite_` instead. Here is an alternative `main` that prints the route error to `stdout`.


~~~~ {.haskell}
main :: IO ()
main = simpleHTTP nullConf $ msum
 [ dir "favicon.ico" $ notFound (toResponse ())
 , do r <- implSite_ (pack "http://localhost:8000") (pack "/route") site
      case r of
        (Left e) -> liftIO (print e) >> mzero
        (Right m) -> return m
 , seeOther "/route" (toResponse ())
 ]
~~~~


Source code for the app is [here](http://srclink/WebRoutes/WebRoutesDemo.hs).

`web-routes` + Type Families
----------------------------

`showURL` has the type:


~~~~ {.haskell}
showURL :: ShowURL m => URL m -> m Text
~~~~


If you are not familiar with type families and type functions, the `URL m` in that type signature might look a bit funny. But it is really very simple.

The `showURL` function leverages the `ShowURL` class:


~~~~ {.haskell}
class ShowURL m where
   type URL m
   showURLParams :: (URL m) -> [(Text, Text)] -> m Text
~~~~


And here is the `RouteT` instance for `ShowURL`:


~~~~ {.haskell}
instance (Monad m) => ShowURL (RouteT url m) where
   type URL (RouteT url m) = url
   showURLParams url params =
       do showF <- askRouteT
          return (showF url params)
~~~~


Here `url` is a *type function* that is applied to a type and gives us another type. For example, writing `URL (RouteT Sitemap (ServerPartT IO))` gives us the type `Sitemap`. We can use the type function any place we would normally use a type.

In our example we had:


~~~~ {.haskell}
homeURL <- showURL Home
~~~~


So there, showURL is going to have the type:


~~~~ {.haskell}
showURL :: URL (RouteT Sitemap (ServerPartT IO))
        -> RouteT Sitemap (ServerPartT IO) Text
~~~~


which can be simplified to:


~~~~ {.haskell}
showURL :: Sitemap -> RouteT Sitemap (ServerPartT IO) Text
~~~~

So, we see that the URL type we pass to `showURL` is dictated by the monad we are currently in. This ensures that we only call `showURL` on values of the right type.

While `ShowURL` is generally used with the `RouteT` type -- it is not actually a requirement. You can implement `ShowURL` for any monad of your choosing.

