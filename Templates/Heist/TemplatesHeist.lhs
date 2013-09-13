Heist HTML5 and XML Templates
-----------------------------

To enable Heist support you need to install the `happstack-heist` package.

Heist is a hybrid Haskell+XML templating solution for generating XML
and HTML documents.

The XML/XHTML markup and static content for a template is stored in an
external XML file. Any programming logic needed to fill in template
values is done via Haskell code.

The Heist system reads the template file and runs the Haskell code to
transform the template, producing the output document which is
served to the user.

Here is an example Heist template:

``` XML
<html>
  <head>
    <title>Factorial Page</title>
  </head>
  <body>
    <h1>Factorial Page</h1>
    <p>The factorial of 6 is <fact>6</fact></p>
  </body>
</html>
```

The template is almost an XHTML document, except that it contains the
special tag `<fact>6</fact>`.

In the Haskell code, we will create a `Splice` that will replace that
tag with the value of `6!`.

>
> module Main where
>
> import Control.Applicative    ((<$>))
> import Control.Monad          (msum)
> import qualified Data.Text    as T
> import Happstack.Server       ( dir, nullConf, nullDir, simpleHTTP
>                               , seeOther, toResponse
>                               )
> import Happstack.Server.Heist (heistServe, initHeistCompiled)
> import Heist                  (getParamNode)
> import Heist.Compiled         (Splice, yieldRuntimeText)
> import qualified Text.XmlHtml as X
>
> -- | factorial splice
> factSplice :: (Monad m) => Splice m
> factSplice = do
>   intStr <- T.unpack . X.nodeText <$> getParamNode
>   let res = yieldRuntimeText $ do
>         case reads intStr of
>           [(n,[])] ->
>             return (T.pack $ show $ product [1..(n :: Integer)])
>           _ ->
>             return (T.pack $ "Unable to parse " ++
>                     intStr ++ " as an Integer.")
>   return $ res
>
> main :: IO ()
> main = do
>   heistState <- do
>     r <- initHeistCompiled [(T.pack "fact", factSplice)] [] "."
>     case r of
>       (Left e) -> error $ unlines e
>       (Right heistState) -> return $ heistState
>   simpleHTTP nullConf $ msum
>     [ dir "heist" $ heistServe heistState
>     , nullDir >>
>       seeOther "/heist/factorial" (toResponse "/heist/factorial")
>     ]

