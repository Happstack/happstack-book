
Matching on request method `(GET, POST, etc)`
---------------------------------------------

We can specify that a route is only valid for specific HTTP request methods by using the `method` guard:

~~~~ {.haskell}
method :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
~~~~

Here is a simple demo app:


> module Main where
>
> import Control.Monad    (msum)
> import Happstack.Server ( Method(GET, POST), dir, method
>                         , nullConf, ok, simpleHTTP, seeOther
>                         )
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum
>        [ do dir "foo" $ do method GET
>                            ok $ "You did a GET request on /foo\n"
>        , do method GET
>             ok $ "You did a GET request.\n"
>        , do method POST
>             ok $ "You did a POST request.\n"
>        ]

Using `curl` we can see the expected results for normal `GET` and `POST` requests to `/`:

     $ curl http://localhost:8000/
    You did a GET request.
     $ curl -d '' http://localhost:8000/
    You did a POST request.

Note that `method` does not require that all the segments of request path have been consumed. We can see in here that `/foo` is accepted, and so is `/foo/bar`.

     $ curl http://localhost:8000/foo
    You did a GET request on /foo
     $ curl http://localhost:8000/foo/bar
    You did a GET request on /foo


You can use `nullDir` to assert that all the path segments have been consumed:

~~~~ {.haskell}
nullDir :: (ServerMonad m, MonadPlus m) => m ()
~~~~
