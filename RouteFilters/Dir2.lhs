Using `dir` to match on multiple components
-------------------------------------------

We can match on multiple components by chaining calls to `dir` together:

> module Main where
>
> import Control.Monad (msum)
> import Happstack.Server (nullConf, simpleHTTP, ok, dir)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum [ dir "hello"    $ dir "world" $ ok "Hello, World!"
>                                   , dir "goodbye"  $ dir "moon"  $ ok "Goodbye, Moon!"
>                                   ]

If we start the app and point our browser at [http://localhost:8000/hello/world](http://localhost:8000/hello/world) we get the hello message, and if we point it at [http://localhost:8000/goodbye/moon](http://localhost:8000/goodbye/moon), we get the goodbye message.

