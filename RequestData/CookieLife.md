
Cookie Lifetime
---------------

When you set a cookie, you also specify the lifetime of that cookie. Cookies are referred to as `session cookies` or `permanent cookies` depending on how their lifetime is set.

session cookie

:    A cookie which expires when the browser is closed.

permanent cookie

:    A cookie which is saved (to disk) and is available even if the browser is restarted. The expiration time is set by the server.

The lifetime of a `Cookie` is specified using the `CookieLife` type:

~~~~ {.haskell}
-- | the lifetime of the cookie
data CookieLife
  = Session          -- ^ expire when the browser is closed
  | MaxAge Seconds   -- ^ expire after the specified
                     --   number of seconds
  | Expires UTCTime  -- ^ expire at a specific date and time
  | Expired          -- ^ expire immediately
~~~~


If you are intimately familiar with cookies, you may know that cookies have both an `expires` directive and a `max-age` directive, and wonder how they related to the constructors in `CookieLife`. Internet Explorer only supports the obsolete `expires` directive, instead of newer `max-age` directive. Most other browser will honor the `max-age` directive over `expires` if both are present. To make everyone happy, we always set both.

So, when setting `CookieLife` you can use `MaxAge` or `Expires` -- which ever is easiest, and the other directive will be calculated automatically.

Deleting a Cookie
-----------------

There is no explicit `Response` header to delete a cookie you have already sent to the client. But, you can convince the client to delete a cookie by sending a new version of the cookie with an expiration date that as already come and gone. You can do that by using the `Expired` constructor. Or, you can use the more convenient, `expireCookie` function.

~~~~ {.haskell}
 -- | Expire the cookie immediately and set the cookie value to ""
 expireCookie :: (MonadIO m, FilterMonad Response m) =>
                 String  -- ^ cookie name
              -> m ()
~~~~

