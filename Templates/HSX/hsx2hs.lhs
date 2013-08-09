
Using HSX/HSP
=============

To enable HSX support, you must install the `happstack-hsp` package.

HSX is an XML-based templating system that allows you to embed XML in your Haskell source files. If you have ever had to use PHP, you may want to run screaming from this idea. However, the HSX solution is far saner than the PHP solution, so you may want to give it a chance.

There are two ways you can use `hsx`. One way is to use an external preprocessor `hsx2hs`. The other way is to use the `[hsx| |]` quasiquoter.

`hsx2hs`
--------

The `hsx2hs` is the traditional way of embedding literal XML into Haskell. It predates the existance of the `QuasiQuotes` extension.

There are two benefits to the `hsx2hs` preprocessor over the `QuasiQuotes`:

 1. it does not require extra syntax to delimit where the XML starts

 2. it can be used with Haskell compilers that do not support `QuasiQuotes` such as `Fay`.

However it has a few drawbacks as well:

 1. it strips haddock comments from the source file

 2. it can screw up the line numbers in error messages

Those drawbacks are fixable, but require some serious effort.

The first thing we will see is a funny `OPTIONS_GHC` pragma at the top of our file:

> {-# LANGUAGE FlexibleContexts, OverlappingInstances #-}
> {-# OPTIONS_GHC -F -pgmFhsx2hs #-}
> module Main where
>

This pragma at the top is how we tell GHC that this file needs to be run through the `hsx2hs` pre-processor in order to work. So, that options line looks a bit like line noise. You can try to remember it like this:

 1. `-F` says we want to filter the source code (or maybe trans*F*orm the source code)
 2. `-pgmF` specifies the program we want to do the transformation
 3. `hsx2hs` is the preprocessor that converts the `hsx` markup to plain-old `hs`

Next we have some imports:

> import Control.Applicative        ((<$>))
> import Control.Monad.Identity     (Identity(runIdentity))
> import Data.String                (IsString(fromString))
> import Data.Text                  (Text)
> import HSP
> import HSP.Monad                  (HSPT(..))
> import Happstack.Server.HSP.HTML
> import Happstack.Server.XMLGenT
> import Happstack.Server           ( Request(rqMethod), ServerPartT
>                                   , askRq, nullConf, simpleHTTP
>                                   )

Now we can define a function which generates an HTML page:

> hello :: ServerPartT IO XML
> hello = unHSPT $ unXMLGenT
>   <html>
>    <head>
>     <title>Hello, HSP!</title>
>    </head>
>    <body>
>     <h1>Hello HSP!</h1>
>     <p>We can insert Haskell expression such as this:
>         <% show $ sum [1 .. (10 :: Int)] %></p>
>     <p>We can use the ServerPartT monad too.
>        Your request method was: <% getMethod %></p>
>     <hr/>
>     <p>We don't have to escape & or >. Isn't that nice?</p>
>     <p>If we want <% "<" %> then we have to do something funny.</p>
>     <p>But we don't have to worry about
>        escaping <% "<p>a string like this</p>" %></p>
>     <p>We can also nest <% <span>like <% "this." %> </span> %></p>
>    </body>
>   </html>
>       where
>       getMethod :: XMLGenT (HSPT XML (ServerPartT IO)) String
>       getMethod = show . rqMethod <$> askRq
>

> main :: IO ()
> main = simpleHTTP nullConf $ hello
>

The first thing we notice is that syntax looks pretty much like normal HTML syntax. There are a few key differences though:

  1. like XML, all tags must be closed
  2. like XML, we can use shortags (e.g. `<hr />`)
  3. We do not have to escape & and >
  4. To embed < we have to do something extra funny

The syntax:

    <% haskell-expression %>

allows us to embed a Haskell expression inside of literal XML.

As shown in this line:

~~~~{.haskell}
<p>We can also nest <% <span>like <% "this." %> </span> %></p>
~~~~

we can freely nest Haskell and XML expressions.
