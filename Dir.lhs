Using `dir` to match on static path components
----------------------------------------------

We can use `dir` to handle components of the URI path which are static. For example, we might have a site with the two URLs: `hello` and `goodbye`.

> module Main where
>
> import Control.Monad
> import Happstack.Server (nullConf, simpleHTTP, ok, dir)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum [ dir "hello"    $ ok "Hello, World!"
>                                   , dir "goodbye"  $ ok "Goodbye, World!"
>                                   ]

Source code for the app is [here](http://srclink/Dir.hs).

If we start the app and point our browser at [http://localhost:8000/hello](http://localhost:8000/hello) we get the `hello` message, and if we point it at [http://localhost:8000/goodbye](http://localhost:8000/goodbye), we get the `goodbye` message.

