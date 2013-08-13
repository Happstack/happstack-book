
`web-routes-boomerang`
----------------------

In the previous example we used Template Haskell to automatically derive a mapping between the URL type and the URL string. This is very convenient early in the development process when the routes are changing a lot. But sometimes we want more precise control over the look of the URL. One solution is to write the mappings from the URL type to the URL string by hand.

One way to do that would be to write one function to show the URLs, and another function that uses `parsec` to parse the URLs. But having to say the same thing twice is really annoying and error prone. What we really want is a way to write the mapping once, and automatically exact a parser and printer from the specification.

Fortunately, Sjoerd Visscher and Martijn van Steenbergen figured out exactly how to do that and published a proof of concept library know as [`Zwaluw`](http://hackage.haskell.org/package/Zwaluw). With permission, I have refactored their original library into two separate libraries: [`boomerang`](http://hackage.haskell.org/package/boomerang) and [`web-routes-boomerang`](http://hackage.haskell.org/package/web-routes-boomerang).

The technique behind `Zwaluw` and `Boomerang` is very cool. But in this tutorial we will skip the theory and get right to the practice.

In order to run this demo you will need to install `web-routes`, `web-routes-boomerang` and `web-routes-happstack` from hackage.

We will modify the previous demo to use `boomerang` in order to demonstrate how easy it is to change methods midstream. We will also add a few new routes to demonstrate some features of using `boomerang`.



> {-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving
>   , TemplateHaskell, TypeOperators, OverloadedStrings #-}
> module Main where
>

The first thing to notice is that we hide `id` and `(.)` from the `Prelude` and import the versions from `Control.Category` instead.

> import Prelude                 hiding (head, id, (.))
> import Control.Category        (Category(id, (.)))
>
> import Control.Monad           (msum)
> import Data.Data               (Data, Typeable)
> import Data.Monoid             (mconcat)
> import Data.String             (fromString)
> import Data.Text               (Text)
> import Happstack.Server
>     ( Response, ServerPartT, ok, toResponse, simpleHTTP
>     , nullConf, seeOther, dir, notFound, seeOther)
> import Text.Blaze.Html4.Strict
>     ( (!), html, head, body, title, p, toHtml
>     , toValue, ol, li, a)
> import Text.Blaze.Html4.Strict.Attributes (href)
> import Text.Boomerang.TH       (derivePrinterParsers)
> import Web.Routes
>     ( PathInfo(..), RouteT, showURL
>     , runRouteT, Site(..), setDefault, mkSitePI)
> import Web.Routes.TH           (derivePathInfo)
> import Web.Routes.Happstack    (implSite)
> import Web.Routes.Boomerang
>

Next we have our `Sitemap` types again. `Sitemap` is similar to the previous example, except it also includes `UserOverview` and `UserDetail`.

> newtype ArticleId = ArticleId { unArticleId :: Int }
>     deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)
>
> data Sitemap
>     = Home
>     | Article ArticleId
>     | UserOverview
>     | UserDetail Int Text
>     deriving (Eq, Ord, Read, Show, Data, Typeable)
>

Next we call `derivePrinterParsers`:

> $(derivePrinterParsers ''Sitemap)
>


That will create new combinators corresponding to the constructors
for `Sitemap`. They will be named, `rHome`, `rArticle`, `rUserOverview`, and `rUserDetail`. These combinators are used to apply/remove the corresponding constructors, but they do not affect the appearance of the route at all. You could create these helper functions by hand, but they are dreadful boring and there is no advantage to doing so.

Now we can specify how the `Sitemap` type is mapped to a URL string and back:

> sitemap :: Router () (Sitemap :- ())
> sitemap =
>     (  rHome
>     <> rArticle . (lit "article" </> articleId)
>     <> lit "users" . users
>     )
>     where
>       users =  rUserOverview
>             <> rUserDetail </> int . lit "-" . anyText
>
> articleId :: Router () (ArticleId :- ())
> articleId =
>     xmaph ArticleId (Just . unArticleId) int

The mapping looks like this:

URL                   `<=>` type
---                   ----- ----
/                     `<=>` `Home`
/article/*int*        `<=>` `Article Int`
/users/               `<=>` `UserOverview`
/users/*int*-*string* `<=>` `UserDetail Int String`

The `sitemap` function looks like an ordinary parser. But, what makes it is exciting is that it also defines the pretty-printer at the same time.

By examining the mapping table and comparing it to the code, you should be able to get an intuitive feel for how `boomerang` works. The key boomerang features we see are:

`<>`
:    is the choice operator. It chooses between the various paths.
`.`
:    is used to combine elements together.
`</>`
:    matches on the / between path segments. (The combinators, such as `lit`, `int`, `anyText`, operate on a single path segment.)
`lit`
:    matches on a string literal. If you enabled `OverloadedStrings` then you do not need to explicitly use the `lit` function. For example, you could just write, `int . "-" . anyText`.
`int`
:    matches on an `Int`.
`anyText`
:    matches on any string. It keeps going until it reaches the end of the current path segment.
`xmaph`
:    is a bit like `fmap`, except instead of only needing `a -> b` it also needs the other direction, `b -> Maybe a`.

~~~~ {.haskell}
xmaph :: (a -> b)
      -> (b -> Maybe a)
      -> PrinterParser e tok i (a :- o)
      -> PrinterParser e tok i (b :- o)
~~~~

   In this example, we use `xmaph` to convert `int :: Router () (Int :- ())` into `articleId :: Router () (ArticleId :- ())`.

longest route
:    You will notice that the parser for /users comes before /users/*int*-*string*. Unlike `parsec`, the order of the parsers (usually) does not matter. We also do not have to use `try` to allow for backtracking. `boomerang` will find all valid parses and pick the best one. Here, that means the parser that consumed the most available input.

`Router` type is just a simple alias:


~~~~ {.haskell}
type Router a b = PrinterParser TextsError [Text] a b
~~~~


Looking at this line:


~~~~ {.haskell}
            <> rUserDetail </> int . lit "-" . anyText
~~~~


and comparing it to the constructor


~~~~ {.haskell}
    UserDetail Int Text
~~~~


we see that the constructor takes two arguments, but the mapping uses three combinators, `int`, `lit`, and `anyText`. It turns out that some combinators produce/consume values from the URL type, and some do not. We can find out which do and which don't by looking at the their types:


~~~~ {.haskell}
int     ::         PrinterParser TextsError [Text] r (Int :- r)
anyText ::         PrinterParser TextsError [Text] r (Text :- r)
lit     :: Text -> PrinterParser TextsError [Text] r r
~~~~


We see `int` takes `r` and produces `(Int :- r)` and `anyText` takes `r` and produces `(Text :- r)`. While `lit` takes `r` and returns `r`.

Looking at the type of the all three composed together we get:


~~~~ {.haskell}
int . lit "-" . anyText :: PrinterParser TextsError [Text] a (Int :- (Text :- a))
~~~~


So there we see the `Int` and `Text` that are arguments to `UserDetail`.

Looking at the type of `rUserDetail`, we will see that it has the type:


~~~~ {.haskell}
 rUserDetail :: PrinterParser e tok (Int :- (Text :- r)) (Sitemap :- r)
~~~~


So, it takes an `Int` and `Text` and produces a `Sitemap`. That mirrors what the `UserDetail` constructor itself does:


~~~~
ghci> :t UserDetail
UserDetail :: Int -> Text -> Sitemap
~~~~


Next we need a function that maps a route to the handlers. This is the same exact function we used in the previous example extended with the additional routes:



> route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
> route url =
>     case url of
>       Home                  -> homePage
>       (Article articleId)   -> articlePage articleId
>       UserOverview          -> userOverviewPage
>       (UserDetail uid name) -> userDetailPage uid name
>



Next, we have the handler functions. These are also exactly the same as the previous example, plus the new routes:



> homePage :: RouteT Sitemap (ServerPartT IO) Response
> homePage = do
>   articles     <- mapM mkArticle [(ArticleId 1) .. (ArticleId 10)]
>   userOverview <- showURL UserOverview
>   ok $ toResponse $
>     html $ do
>      head $ title $ "Welcome Home!"
>      body $ do
>       a ! href (toValue userOverview) $ "User Overview"
>       ol $ mconcat articles
>   where
>    mkArticle articleId = do
>     url <- showURL (Article articleId)
>     return $ li $ a ! href (toValue url) $
>       toHtml $ "Article " ++ (show $ unArticleId articleId)
>

> articlePage :: ArticleId
>             -> RouteT Sitemap (ServerPartT IO) Response
> articlePage (ArticleId articleId) = do
>   homeURL <- showURL Home
>   ok $ toResponse $
>     html $ do
>      head $ title $ (toHtml $ "Article " ++ show articleId)
>      body $ do
>       p $ toHtml $ "You are now reading article " ++ show articleId
>       p $ do "Click "
>              a ! href (toValue homeURL) $ "here"
>              " to return home."
>

> userOverviewPage :: RouteT Sitemap (ServerPartT IO) Response
> userOverviewPage = do
>   users <- mapM mkUser [1 .. 10]
>   ok $ toResponse $
>     html $ do
>       head $ title $ "Our Users"
>       body $ do
>         ol $ mconcat users
>   where
>     mkUser userId = do
>       url <- showURL (UserDetail userId
>                       (fromString $ "user " ++ show userId))
>       return $ li $ a ! href (toValue url) $
>         toHtml $ "User " ++ (show $ userId)
>


> userDetailPage :: Int
>                -> Text
>                -> RouteT Sitemap (ServerPartT IO) Response
> userDetailPage userId userName = do
>   homeURL <- showURL Home
>   ok $ toResponse $
>     html $ do
>       head $ title $ (toHtml $ "User " <> userName)
>       body $ do
>         p $ toHtml $ "You are now view user detail page for " <> userName
>         p $ do "Click "
>                a ! href (toValue homeURL) $ "here"
>                " to return home."
>

Creating the `Site` type is similar to the previous example. We still use `runRouteT` to unwrap the `RouteT` layer. But now we use `boomerangSite` to convert the `route` function into a `Site`:


> site :: Site Sitemap (ServerPartT IO Response)
> site =
>   setDefault Home $ boomerangSite (runRouteT route) sitemap
>


The route function is essentially the same in this example and the previous example -- it did not have to be changed to work with `boomerang` instead of `PathInfo`. It is the `formatPathSegments` and `parsePathSegments` functions bundled up in the `Site` that change. In the previous example, we used `mkSitePI`, which leveraged the `PathInfo` instances. Here we use `boomerangSite` which uses the `sitemap` mapping we defined above.

The practical result is that you can start by using `derivePathInfo` and avoid having to think about how the URLs will look. Later, once the routes have settled down, you can then easily switch to using `boomerang` to create your route mapping.

Next we use `implSite` to embed the `Site` into a normal Happstack route:

> main :: IO ()
> main = simpleHTTP nullConf $ msum
>   [ dir "favicon.ico" $ notFound (toResponse ())
>   , implSite "http://localhost:8000" "/route" site
>   , seeOther ("/route/" :: String) (toResponse ())
>   ]
>

Source code for the app is [here](http://srclink/WebRoutesBoomerang.hs).

In this example, we only used a few simple combinators. But `boomerang` provides a whole range of combinators such as `many`, `some`, `chain`, etc. For more information check out the [haddock documentation for `boomerang`](http://hackage.haskell.org/package/boomerang). Especially the `Text.Boomerang.Combinators` and `Text.Boomerang.Texts` modules.
