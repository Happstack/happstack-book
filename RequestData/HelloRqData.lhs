
Hello RqData
------------

Let's start with a simple `hello, world!` example that uses request
parameters in the URL.


> module Main where
>
> import Happstack.Server (ServerPart, look, nullConf, simpleHTTP, ok)
>
> helloPart :: ServerPart String
> helloPart =
>     do greeting <- look "greeting"
>        noun     <- look "noun"
>        ok $ greeting ++ ", " ++ noun
>
> main :: IO ()
> main = simpleHTTP nullConf $ helloPart


Source code for the app is [here](http://srclink/HelloRqData.hs).

Now if we visit [http://localhost:8000/?greeting=hello&amp;noun=rqdata](http://localhost:8000/?greeting=hello&amp;noun=rqdata), we will get the message `hello, rqdata`.

we use the `look` function to look up some keys by name. The `look` function has the type:


~~~~ {.haskell}
look :: (Functor m, Monad m, HasRqData m) => String -> m String
~~~~


Since we are using `look` in the `ServerPart` monad it has the simplified type:


~~~~ {.haskell}
look :: String -> ServerPart String
~~~~


The `look` function looks up a key and decodes the associated value as
a `String`. It assumes the underlying `ByteString` was utf-8
encoded. If you are using some other encoding, then you can use
`lookBS` to construct your own lookup function.

If the key is not found, then `look` will fail. In `ServerPart` that
means it will call `mzero`.

