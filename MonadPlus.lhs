Choosing between multiple `ServerPartTs`
----------------------------------------

In the first example, we had only one `ServerPartT`. All `Request`s were
handled by the same part and returned the same `Response`.

In general, our applications will have many `ServerPartT`s. We combine
them into a single `ServerPartT` by using `MonadPlus`.
Typically via the `msum` function:

~~~~{.haskell}
msum :: (MonadPlus m) => [m a] -> m a
~~~~

In the following example we combine three `ServerPartT`s together.

> module Main where
>
> import Control.Monad
> import Happstack.Server (nullConf, simpleHTTP, ok, dir)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum [ mzero
>                                   , ok "Hello, World!"
>                                   , ok "Unreachable ServerPartT"
>                                   ]

Source code for the app is [here.](http://srclink/MonadPlus.hs)

The behaviour of `MonadPlus` is to try each `ServerPartT` in succession,
until one succeeds.

In the example above, the first part is `mzero`, so it will always fail.
The second part will always succeed. That means the third part will
never be reachable.

Alas, that means this application will appear to behave exactly like the
first application. What we need are some ways to have parts match or
fail depending on the contents of the HTTP `Request`.
