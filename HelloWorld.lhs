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

If you are reading this on [School of Haskell](https://www.fpcomplete.com/), the examples import the module `Happstack.Server.Env` instead of `Happstack.Server`. This is a (hopefully) temporary hack so that the interactive code examples work on School of Haskell. To run code from School of Haskell locally simply replace `Happstack.Server.Env` with `Happstack.Server`.

If you are reading this on School of Haskell, you can run the examples interactively with out installing anything.

If you want to run the code locally, and you have not already installed Happstack -- you will need to do that first. You can find instructions on how to install Happstack at [http://happstack.com/page/view-page-slug/2/download](http://happstack.com/page/view-page-slug/2/download).

To build the application run:

    $ ghc -threaded HelloWorld.hs -o helloworld

The executable will be named `helloworld`. You can run it like:

    $ ./helloworld

Alternatively, you can use `runhaskell` and avoid the compilation step:

    $ runhaskell HelloWorld.hs

Run this app and point your browser at [`http://localhost:8000/`](http://localhost:8000/). (assuming
you are building the program on your local machine.) The page should load and say `"Hello, World!"`.

Alternatively, we can use `curl`:

     $ curl http://localhost:8000/
    Hello, World!

`curl` is a command-line utility which can be used to create many
types of HTTP requests. Unlike a browser, it does not attempt to
render the results, it just prints the body of the response to the
console.

If you run `curl` with the `-v` option it will provide verbose output
which includes the headers sent back and forth between curl and the
server:

~~~~
 $ curl -v http://localhost:8000/
 * About to connect() to localhost port 8000 (#0)
 *   Trying 127.0.0.1... connected
 > GET / HTTP/1.1
 > User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu)
 > Host: localhost:8000
 > Accept: */*
 >
 < HTTP/1.1 200 OK
 < Transfer-Encoding: chunked
 < Connection: Keep-Alive
 < Content-Type: text/plain; charset=UTF-8
 < Date: Thu, 13 Dec 2012 00:19:01 GMT
 < Server: Happstack/7.0.7
 <
 * Connection #0 to host localhost left intact
 * Closing connection #0
 Hello, World!
~~~~

This can sometimes be useful for debugging why your site is not
working as you expect.

`curl` is not required by Happstack or this book, but it is a useful
tool for web development. `curl` is not part of Happstack. The
official curl website is [http://curl.haxx.se](http://curl.haxx.se).

The parts of `Hello World`
-------------------------

%%% Listening for HTTP requests

The `simpleHTTP` function is what actually starts the program listening for incoming HTTP requests:

~~~~ {.haskell}
simpleHTTP :: (ToMessage a) => Conf -> ServerPartT IO a -> IO ()
~~~~

We'll examine the various parts of this type signature in the following sections.

%%% Configuring the HTTP listener

The first argument, `Conf`, is some simple server configuration information. It is defined as:

~~~~ {.haskell}
data Conf = Conf
    { port       :: Int
    , validator  :: Maybe (Response -> IO Response)
    , logAccess  :: forall t. FormatTime t => Maybe (LogAccess t)
    , timeout    :: Int
    }
~~~~

The fields can be described as:

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
nullConf = Conf
    { port      = 8000
    , validator = Nothing
    , logAccess = Just logMAccess
    , timeout   = 30
    }
~~~~

%%% Processing a `Request`

If we imagined a stripped-down web server, the user would just pass in a handle function with the type:

~~~~ {.haskell}
Request -> IO Response
~~~~

where `Request` and `Response` correspond to HTTP requests and
response. For every incoming request, the server would fork off a new
thread thread and call the handler.

While this would work -- the poor developer would have to invent all manner of adhoc mechanisms for mapping routes, adding extra headers, sending compressed results, returning errors, and other common tasks.

Instead ,`simpleHTTP` takes a handler with the type:

~~~~ {.haskell}
(ToMessage a) => ServerPartT IO a
~~~~

There are three key differences:

 1. The `ServerPartT` monad transformer adds a bunch of functionality on top of the base IO monad

 2. the `Request` argument is now part of `ServerPartT` and can be read using `askRq`.

 3. The `ToMessage` class is used to convert the return value to a `Response`.

`simpleHTTP` processes each incoming request in its own thread. It will parse the `Request`, call your `ServerPartT` handler, and then return a `Response` to the client. When developing your handler, it is natural to think about things as if you are writing a program which processes a single `Request`, generates a single `Response`, and exits. However it is important when doing I/O, such as writing files to disk, or talking to a database to remember that there may be other threads running simultaneously.

%%% Setting the HTTP response code

In this example, our handler is simply:

~~~~ {.haskell}
ok "Hello, World!" :: ServerPartT IO String
~~~~

`ok` is one of several combinators which can be used to set the HTTP response code. In this case, it will set the response code to `200 OK`.  The type signature for `ok` can be simplified to:

~~~~ {.haskell}
ok :: a -> ServerPartT IO a
~~~~

`ok` acts much like `return` except it also sets the HTTP response code for a `Response`.

`Happstack.Server.SimpleHTTP` contains similar functions for the common HTTP response codes including, `notFound`, `seeOther`, `badRequest` and more.

%%% Creating a `Response`

The `ToMessage` class is used to turn values of different types into HTTP responses. It contains three methods:

~~~~ {.haskell}
class ToMessage a where
  toContentType :: a -> ByteString
  toMessage     :: a -> Lazy.ByteString
  toResponse    :: a -> Response
~~~~

A vast majority of the time we only call the `toResponse` method.

`simpleHTTP` automatically calls `toResponse` to convert the value returned by the handler into a `Response` -- so we did not have to call `toResponse` explicitly. It converted the `String` `"Hello, World!"` into a `Response` with the content-type `"text/plain"` and the message body `"Hello, World!"`

Often times we will opt to explicitly call `toResponse`. For example:

~~~ {.haskell .active .web}
-- / show
module Main where

import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)
-- show
main :: IO ()
main = simpleHTTP nullConf $ ok (toResponse "Hello, World!")
~~~

Happstack comes with pre-defined `ToMessage` instances for many types such as `Text.Html.Html`, `Text.XHtml.Html`, `String`, the types from HSP, and more.
