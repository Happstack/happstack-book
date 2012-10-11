
Advanced method matching with `MatchMethod`
-------------------------------------------

The method routing functions use a class `(MatchMethod method)` instead of the concrete type `Method`. The `MatchMethod` class looks like this:


~~~~ {.haskell}
> class MatchMethod m where
>     matchMethod :: m -> Method -> Bool
>
> instance MatchMethod Method           where ...
> instance MatchMethod [Method]         where ...
> instance MatchMethod (Method -> Bool) where ...
> instance MatchMethod ()               where ...
~~~~


This allows us to easily match on more than one method by either
providing a list of acceptable matches, or by providing a function
which returns a boolean value. We can use this feature to support the
`HEAD` method. When the client does a `HEAD` request, the server is
supposed to return the same headers it would for a GET request, but
with an empty response body. Happstack includes special support for
handling this automatically in most cases.

> module Main where
>
> import Control.Monad (msum)
> import Happstack.Server (Method(GET, HEAD), dir, methodM, nullConf, ok, simpleHTTP)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum
>        [ do methodM [GET, HEAD]
>             ok $ "Hello, World\n"
>        ]
>

Source code for the app is [here](http://srclink/MatchMethod.hs).

We can now use curl to do a normal `GET` request, or we can
use the `-I` flag which does a `HEAD` request:

     $ curl http://localhost:8000/
    Hello, World
     $ curl -I http://localhost:8000/
    HTTP/1.1 200 OK
    Connection: Keep-Alive
    Content-Length: 13
    Content-Type: text/plain; charset=UTF-8
    Date: Tue, 15 Jun 2010 19:56:07 GMT
    Server: Happstack/0.5.0

Happstack automatically notices that it is a `HEAD` request, and does not send the body.
