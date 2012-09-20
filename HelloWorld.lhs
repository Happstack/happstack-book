Hello World
===========

Your first app
--------------
<h2><a name="first_app">Your first app!</a></h2>

Our first happstack application is a simple server that responds to
all requests with the string, <kbd>Hello, World!</kbd>.


> module Main where
>
> import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)
>
> main :: IO ()
> main = simpleHTTP nullConf $ ok "Hello, World!"

<p class="source-code">[Source code for the app is <a href="HelloWorld.hs">here.</a>]</p>

To build the application run:

    $ ghc --make -threaded HelloWorld.hs -o helloworld


The executable will be named <kbd>helloworld</kbd>.

Alternatively, you can use <kbd>runhaskell</kbd> and avoid the compilation step.

    $ runhaskell HelloWorld.hs

Run this app and point your browser at <a
href="http://localhost:8000/">http://localhost:8000/</a>. (assuming
you are building the program on your local machine.)

If we point <kbd>curl</kbd> at the app we get the following output:

     $ curl http://localhost:8000/
    Hello, World!

How it works
------------

### Listening for HTTP requests

The top-level function <code>simpleHTTP</code> is what actually starts the program listening for incoming HTTP requests.

~~~~ {.haskell}
simpleHTTP :: (ToMessage a) => Conf -> ServerPartT IO a -> IO ()
~~~~

### Configuring the HTTP listener

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


<dl>
 <dt><code>port</code></dt>
 <dd>the TCP port to listen on for incoming connection</dd>
 <dt><code>validator</code></dt>   
 <dd>on-the-fly validation of output during development</dd> 
 <dt><code>logAccess</code></dt>
 <dd>logging function for HTTP requests</dd>
 <dt><code>timeout</code></dt>
 <dd>number of seconds to wait before killing an inactive connection</dd>
</dl>

The default config is <code>nullConf</code> which is simply defined as:

~~~~ {.haskell}
-- | Default configuration contains no validator and the port is set to 8000
nullConf :: Conf
nullConf = Conf { port      = 8000
                , validator  = Nothing
                , logAccess = Just logMAccess
                }
~~~~

### Processing a <code>Request</code>

The second argument is a bit more interesting. The <code>ServerPartT IO a</code> is the code that actually processes an incoming HTTP <code>Request</code> and generates a <code>Response</code>. The <code>ServerPartT IO</code> monad is essentially a fancy way of writing a function with the type:

~~~~ {.haskell}
> Request -> IO Response
~~~~

<code>simpleHTTP</code> processes each incoming request in its own thread. It will parse the <code>Request</code>, call your <code>ServerPartT</code> handler, and then return the <code>Response</code> to the client. When developing your server part, it is natural to think about things as if you are writing a program which processes a single <code>Request</code>, generates a <code>Response</code>, and exits. However it is important when doing I/O, such as writing files to disk, or talking to a database to remember that there may be other threads running simultaneously.

### Setting the HTTP response code

In this example, our server part is simply:

~~~~ {.haskell}
> ok "Hello, World!"
~~~~

<code>ok</code> is one of several combinators which can be used to set the HTTP response code. In this case, it will set the response code to <code>200 OK</code>. <a href="http://happstack.com/docs/0.5.0/happstack-server/Happstack-Server-SimpleHTTP.html"><code>Happstack.Server.SimpleHTTP</code></a> contains similar functions for the common HTTP response codes including, <code>notFound</code>, <code>seeOther</code>, <code>badRequest</code> and more. These functions all act like the normal <code>return</code> function, except they also set the response code.

### Creating a <code>Response</code>

The body of the response will be "Hello, World!".

The <code>String</code> "Hello, World!" is turned into a <code>Response</code> because simpleHTTP calls <code>toResponse</code> from the <code>ToMessage</code> class on it. Often times we will opt to make this call explicit rather than implicit. For example:

~~~~ {.haskell}
main :: IO ()
main = simpleHTTP nullConf $ ok (toResponse "Hello, World!")
~~~~

The <code>toResponse</code> function takes care of setting the <code>Content-type</code> and converting the value into a lazy <code>ByteString</code> which will be sent back to the client. Happstack comes with pre-defined <code>ToMessage</code> instances for many types such as <code>Text.Html.Html</code>, <code>Text.XHtml.Html</code>, <code>String</code>, the types from HSP, and more.
