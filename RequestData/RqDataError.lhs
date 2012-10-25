
Using the `RqData` for better error reporting
---------------------------------------------

So far we have been using the `look` function in the
`ServerPart` monad. This means that if any
`look` fails, that handler fails. Unfortunately, we are not
told what parameter was missing -- which can be very frustrating when
you are debugging your code. It can be even more annoying if you are
providing a web service, and whenever a developer forgets a parameter,
they get a 404 with no information about what went wrong.

So, if we want better error reporting, we can use functions like
`look` in the `RqData Applicative Functor`.

We can use `getDataFn` to run the `RqData`:


~~~~ {.haskell}
getDataFn :: (HasRqData m, ServerMonad m, MonadIO m) =>
             RqData a
          -> m (Either [String] a)
~~~~

> module Main where
>
> import Control.Applicative ((<$>), (<*>))
> import Happstack.Server (ServerPart, badRequest, nullConf, ok, simpleHTTP)
> import Happstack.Server.RqData (RqData, look, getDataFn)
>
> helloRq :: RqData (String, String)
> helloRq =
>     (,) <$> look "greeting" <*> look "noun"
>
> helloPart :: ServerPart String
> helloPart =
>     do r <- getDataFn helloRq
>        case r of
>          (Left e) ->
>              badRequest $ unlines e
>          (Right (greet, noun)) ->
>              ok $ greet ++ ", " ++ noun
>
> main :: IO ()
> main = simpleHTTP nullConf $ helloPart

Source code for the app is [here](http://srclink/RqDataError.hs).

If we visit
[http://localhost:8000/?greeting=hello&amp;noun=world](http://localhost:8000/?greeting=hello&amp;noun=world),
we will get our familiar greeting `hello, world`.  But if we leave off
the query parameters [http://localhost:8000/](http://localhost:8000/),
we will get a list of errors:

    Parameter not found: greeting
    Parameter not found: noun

We could use the `Monad` instance `RqData` to build the
request. However, the monadic version will only show us the *first*
error that is encountered. So would have only seen that the `greeting`
was missing. Then when we added a `greeting` we would have gotten a
new error message saying that `noun` was missing.

In general, improved error messages are not going to help people
visiting your website. If the parameters are missing it is because a
form or link they followed is invalid. There are two places where
there error messages are useful:


 1. When you are developing and debugging your site
 2. Reporting errors to users of your web service API

If you are providing a REST API for developers to use, they are going
to be a lot happier if they get a detailed error messages instead of a
plain old 404.
