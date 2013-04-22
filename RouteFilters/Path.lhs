Matching on variable path segments
----------------------------------

Often times a path segment will contain a variable value we want to
extract and use, such as a number or a string. We can use the
`path` combinator to do that.

~~~~ {.haskell}
path :: (FromReqURI a, MonadPlus m, ServerMonad m) => (a -> m b) -> m b
~~~~

You may find that type to be a little hard to follow because it is pretty abstract looking. Fortunately, we can look at it in an easier way. A `ServerPart` is a valid instance of, `ServerMonad m`, so we can just replace the `m` with `ServerPart`. You can do this anywhere you see type signatures with `(ServerMonad m) =>` in them. In this case, the final result would look like:

~~~~ {.haskell}
path :: (FromReqURI a) => (a -> ServerPart b) -> ServerPart b
~~~~

`path` will attempt to extract and decode a path
segment, and if it succeeds, it will pass the decode value to the nested
server part.

Let's start with the most basic example, extracting a
`String` value. We will extend the Hello World server so
that we can say hello to anyone.

> module Main where
>
> import Control.Monad (msum)
> import Happstack.Server (nullConf, simpleHTTP, ok, dir, path)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum [ dir "hello" $ path $ \s -> ok $ "Hello, " ++ s
>                                   ]

Now, if we start the app and point our browser at: [http://localhost:8000/hello/World](http://localhost:8000/hello/World) we get the response `"Hello, World"`.
if we point it at [http://localhost:8000/hello/Haskell](http://localhost:8000/hello/Haskell), we get `"Hello, Haskell"`.

