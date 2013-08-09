
Simple Cookie Demo
------------------

The cookie interface is pretty small. There are two parts to the interface: setting a cookie and looking up a cookie.

To create a `Cookie` value, we use the `mkCookie` function:


~~~~ {.haskell}
-- | create a 'Cookie'
mkCookie  :: String -- ^ cookie name
          -> String -- ^ cookie value
          -> Cookie
~~~~


Then we use the `addCookie` function to send the cookie to the
user. This adds the `Set-Cookie` header to the `Response`. So the
cookie will not actually be set until the `Response` is sent.

~~~~ {.haskell}
-- | add the 'Cookie' to the current 'Response'
addCookie :: (MonadIO m, FilterMonad Response m) => CookieLife -> Cookie -> m ()
~~~~


The first argument of `addCookie` specifies how long the browser should keep the cookie around. See the <a href="#cookie_life">cookie lifetime</a> section for more information on `CookieLife`.

To lookup a cookie, we use some `HasRqData` functions. There are only three cookie related functions:


~~~~ {.haskell}
-- | lookup a 'Cookie'
lookCookie :: (Monad m, HasRqData m) =>
              String -- ^ cookie name
           -> m Cookie

-- | lookup a 'Cookie' and return its value
lookCookieValue :: (Functor m, Monad m, HasRqData m) =>
                   String -- ^ cookie name
                -> m String

-- | look up a 'Cookie' value and try to convert it using 'read'
readCookieValue :: (Functor m, Monad m, HasRqData m, Read a) =>
                   String -- ^ cookie name
                -> m a
~~~~

The cookie functions work just like the other `HasRqData` functions. That means you can use `checkRq`, etc.

The following example puts all the pieces together. It uses the cookie to store a simple counter specifying how many requests have been made:

> module Main where
>
> import Control.Monad.Trans ( liftIO )
> import Control.Monad       ( msum, mzero )
> import Happstack.Server
>     ( CookieLife(Session), Request(rqPaths), ServerPart
>     , addCookie , askRq, look, mkCookie, nullConf
>     , ok, readCookieValue, simpleHTTP )
>
> homePage :: ServerPart String
> homePage = msum
>   [ do rq <- askRq
>        liftIO $ print (rqPaths rq)
>         mzero
>   , do requests <- readCookieValue "requests"
>        addCookie Session (mkCookie "requests"
>                           (show (requests + (1 :: Int))))
>        ok $ "You have made " ++ show requests ++
>             " requests to this site."
>   , do addCookie Session (mkCookie "requests" (show 2))
>        ok $ "This is your first request to this site."
>   ]
>
> main :: IO ()
> main = simpleHTTP nullConf $ homePage


Source code for the app is [here](http://srclink/CookieCounter.hs).

Now if you visit [http://localhost:8000/](http://localhost:8000/) you will get a message like:

    This is your first request to this site.

If you hit reload you will get:


    You have made 3 requests to this site.


Now wait a second! How did we go from 1 to 3, what happened to 2? The
browser will send the cookie with every request it makes to the
server. In this example, we ignore the request path and send a
standard response to every request that is made. The browser first
requests the page, but it also requests the `favicon.ico` for the
site. So, we are really getting two requests everytime we load the
page. Hence the counting by twos. It is important to note that the
browser does not just send the cookie when it is expecting an html
page -- it will send it when it is expecting a jpeg, a css file, a js,
or anything else.

There is also a race-condition bug in this example. See the cookie
issues section for more information.

