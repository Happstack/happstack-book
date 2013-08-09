
Using `dirs` as shorthand to match on multiple components
---------------------------------------------------------

As a shorthand, we can also use `dirs` to handle multiple static path components.

> module Main where
>
> import Control.Monad    (msum)
> import Happstack.Server (dirs, nullConf, ok, seeOther, simpleHTTP)
>
> main :: IO ()
> main = simpleHTTP nullConf $
>   msum [ dirs "hello/world"  $ ok "Hello, World!"
>        , dirs "goodbye/moon" $ ok "Goodbye, Moon!"
>        , seeOther "/hello/world" "/hello/world"
>        ]

If we start the app and point our browser at [http://localhost:8000/hello/world](http://localhost:8000/hello/world) we get the hello message, and if we point it at [http://localhost:8000/goodbye/moon](http://localhost:8000/goodbye/moon), we get the goodbye message.

