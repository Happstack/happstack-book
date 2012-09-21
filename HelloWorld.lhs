Hello World
===========

Your first app!
---------------

Our first Happstack application is a simple server that responds to
all requests with the string, `Hello, World!`.


> module Main where
>
> import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)
>
> main :: IO ()
> main = simpleHTTP nullConf $ ok "Hello, World!"

Source code for the app is [here](http://srclink/HelloWorld.hs).

To build the application run:

    $ ghc --make -threaded HelloWorld.hs -o helloworld

The executable will be named `helloworld`.

Alternatively, you can use `runhaskell` and avoid the compilation step.

    $ runhaskell HelloWorld.hs

Run this app and point your browser at [`http://localhost:8000/`](http://localhost:8000/). (assuming
you are building the program on your local machine.)

If we point `curl` at the app we get the following output:

     $ curl http://localhost:8000/
    Hello, World!

`curl` is a command-line utility which can be used to create many
types of HTTP requests. Unlike a browser, it does not attempt to
render the results, it just prints the raw response to the
console. `curl` is not required by Happstack or this book, but it is a
useful tool for web development.

`curl` is not part of Happstack. The official curl website is [http://curl.haxx.se](http://curl.haxx.se). 

How it works
------------

*** Listening for HTTP requests

The `simpleHTTP` function is what actually starts the program listening for incoming HTTP requests:

~~~~ {.haskell}
simpleHTTP :: (ToMessage a) => Conf -> ServerPartT IO a -> IO ()
~~~~

We'll examine the various parts of this type signature in the following sections.

*** Configuring the HTTP listener

The first argument is some simple server configuration information. It is defined as:

~~~~ {.haskell}
data Conf = Conf { port       :: Int
                 , validator  :: Maybe (Response -> IO Response) 
                 , logAccess  :: forall t. FormatTime t => 
                      Maybe (String -> String -> t -> String -> Int -> 
                             Integer -> String -> String -> IO ())
                 , timeout    :: Int
                 }
~~~~


`port`

:    the TCP port to listen on for incoming connection

`validator`

:    on-the-fly validation of output during development

`logAccess`

:    logging function for HTTP requests

`timeout`

:    number of seconds to wait before killing an inactive connection

The default config is `nullConf` which is simply defined as:

~~~~ {.haskell}
-- | Default configuration contains no validator and the port is set to 8000
nullConf :: Conf
nullConf = Conf { port      = 8000
                , validator  = Nothing
                , logAccess = Just logMAccess
                }
~~~~

*** Processing a `Request`

The second argument is a bit more interesting. The `ServerPartT IO a` is the code that actually processes an incoming HTTP `Request` and generates a `Response`. The `ServerPartT IO` monad is essentially a fancy way of writing a function with the type:

~~~~ {.haskell}
Request -> IO Response
~~~~

`simpleHTTP` processes each incoming request in its own thread. It will parse the `Request`, call your `ServerPartT` handler, and then return the `Response` to the client. When developing your server part, it is natural to think about things as if you are writing a program which processes a single `Request`, generates a `Response`, and exits. However it is important when doing I/O, such as writing files to disk, or talking to a database to remember that there may be other threads running simultaneously.

*** Setting the HTTP response code

In this example, our server part is simply:

~~~~ {.haskell}
ok "Hello, World!"
~~~~

`ok` is one of several combinators which can be used to set the HTTP response code. In this case, it will set the response code to `200 OK`. `Happstack.Server.SimpleHTTP` contains similar functions for the common HTTP response codes including, `notFound`, `seeOther`, `badRequest` and more. These functions all act like the normal `return` function, except they also set the response code.

*** Creating a `Response`

The body of the response will be `"Hello, World!"`.

The `String` `"Hello, World!"` is turned into a `Response` because simpleHTTP calls `toResponse` from the `ToMessage` class on it. Often times we will opt to make this call explicit rather than implicit. For example:

~~~~ {.haskell}
main :: IO ()
main = simpleHTTP nullConf $ ok (toResponse "Hello, World!")
~~~~

The `toResponse` function takes care of setting the `Content-type` and converting the value into a lazy `ByteString` which will be sent back to the client. Happstack comes with pre-defined `ToMessage` instances for many types such as `Text.Html.Html`, `Text.XHtml.Html`, `String`, the types from HSP, and more.

