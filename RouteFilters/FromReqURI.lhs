
`FromReqURI:` extending `path`
------------------------------

We can extend path so that we can extract our own types from the path components as well. We simply add an instance to the FromReqURI class:

~~~ {.haskell}
class FromReqURI a where
    fromReqURI :: String -> Maybe a
~~~

For example, let's say that we want to create a type to represent subjects we can greet.

> -- show
> module Main where
>
> import Control.Monad (msum)
> import Data.Char (toLower)
> import Happstack.Server (FromReqURI(..), nullConf, simpleHTTP, ok, dir, path)
>
>-- show Subject data-type
>
> data Subject = World | Haskell
>
> sayHello :: Subject -> String
> sayHello World   = "Hello, World!"
> sayHello Haskell = "Greetings, Haskell!"
>
> -- show We simply add an instance such as:
>
> instance FromReqURI Subject where
>     fromReqURI sub =
>         case map toLower sub of
>           "haskell" -> Just Haskell
>           "world"   -> Just World
>           _         -> Nothing
>
> -- show Now when we use `path` it will extract a value of type `Subject`.
>
> main :: IO ()
> main = simpleHTTP nullConf $ dir "hello" $ path $ \subject -> ok $ (sayHello subject)


Now, if we start the app and point our browser at:  [http://localhost:8000/hello/World](http://localhost:8000/hello/World) we get the response `"Hello, World"`.
if we point it at [http://localhost:8000/hello/Haskell](http://localhost:8000/hello/Haskell), we get `"Greetings, Haskell!"`.
