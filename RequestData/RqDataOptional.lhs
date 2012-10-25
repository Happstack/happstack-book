
Looking up optional parameters
------------------------------

Sometimes query parameters are optional. You may have noticed that the
`RqData` module does not seem to provide any functions for dealing
with optional values. That is because we can just use the
`Alternative` class from `Control.Applicative` which provides the
function `optional` for us:


~~~~ {.haskell}
optional :: Alternative f => f a -> f (Maybe a)
~~~~


Here is a simple example where the `greeting` parameter is optional:

> module Main where
>
> import Control.Applicative ((<$>), (<*>), optional)
> import Happstack.Server (ServerPart, look, nullConf, ok, simpleHTTP)
>
> helloPart :: ServerPart String
> helloPart =
>     do greet <- optional $ look "greeting"
>        ok $ (show greet)
>
> main :: IO ()
> main = simpleHTTP nullConf $ helloPart


Source code for the app is [here](http://srclink/RqDataOptional.hs).

If we visit [http://localhost:8000/?greeting=hello](http://localhost:8000/?greeting=hello), we will get `Just "hello"`.

if we leave off the query parameters we get [http://localhost:8000/](http://localhost:8000/), we will get `Nothing`.


