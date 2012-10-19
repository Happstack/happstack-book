
Using Heist
===========

To enable Heist support, you must install the `happstack-heist` package.

Heist is an XML templating engine. The static HTML portions of your
web pages reside in XML files which can be edited and reloaded with
out having to recompile your server. The dynamic portions are
generated in Haskell and spliced into the templates.

The following template is almost an XHTML document, except that it contains the special tag `<fact>6</fact>`:

~~~~ {.haskell}

\#include "factorial.tpl-inc"

~~~~

The `<fact/>` tag is an application specific tag which performs a factorial and splices in the result.

The following example shows how to initialize the Heist template system and how to create your own custom tags.

First a bunch of boring imports:


> module Main where

> import Control.Monad          (msum)
> import Control.Monad.Trans    (MonadIO)
> import qualified Data.Text    as T
> import Happstack.Server       (dir, nullConf, nullDir, simpleHTTP, seeOther, toResponse)
> import Happstack.Server.Heist (templateServe, templateReloader)
> import Text.Templating.Heist  (HeistT, Template, HeistState
>                               , bindSplice, defaultHeistState, getParamNode)
> import Text.Templating.Heist.TemplateDirectory (newTemplateDirectory')
> import qualified Text.XmlHtml as X


Next we have the factorial splice:

> factSplice :: (Monad m) => HeistT m Template
> factSplice = do
>   input <- getParamNode
>   let text = T.unpack $ X.nodeText input
>       n    = read text :: Int
>   return [X.TextNode $ T.pack $ show $ product [1..n]]
>


The splice runs in the `HeistT` monad transformer. The `getParamNode` function:

~~~~ {.haskell}
> getParamNode :: (Monad m) => HeistT m Node
~~~~

returns the XHTML node that triggered this splice function to be called. In this case it would be, `<fact>6</fact>`

We then use `textContent`:

~~~~ {.haskell}
> X.textContent :: Node -> ByteString
~~~~

to extract the string "6" from the node, which we convert to an Int.

Finally, we calculate the factorial, and convert the result back into the XML that we want to splice into the template.

The mapping from tag names to template functions is stored in the `HeistState m`. New tags can be added by using `bindSplice`:

~~~~ {.haskell}
bindSplice :: Monad m =>
              ByteString               -- ^ name to use for splice tag
           -> HeistT m Template -- ^ template function to handle the splice
           -> HeistState m          -- ^ template state to update
           -> HeistState m
~~~~


So here we bind `<fact/>` to `factSplice`


> templateState :: (MonadIO m) =>
>                  FilePath -- ^ path to template directory
>               -> HeistState m
> templateState templateDir = bindSplice (T.pack "fact") factSplice defaultHeistState
>

In our main function, we must first initialize the Heist template system by using `newTemplateDirectory'`:

~~~~ {.haskell}
newTemplateDirectory' :: (MonadIO m, MonadIO n) =>
                         FilePath        -- ^ path to template directory on disk
                      -> HeistState m -- ^ the template state
                      -> n (TemplateDirectory m) -- ^ a handle to the template directory
~~~~

In this example, we would put the templates in same directory the app is running from:

> main :: IO ()
> main = do
>     let templateDir = "."
>     td <- newTemplateDirectory' templateDir (templateState templateDir)

to serve templates we simply use the `templateServe` function:

~~~~ {.haskell}
templateServe :: (ServerMonad m, MonadPlus m, MonadIO m) =>
                 TemplateDirectory m -- ^ the handle returned by newTemplateDirectory'
              -> m Response
~~~~


`templateServe` will look at the path in the URL, add .tpl to the end, and try to find a matching template file on disk to return.

Because the templates are loaded into memory, updating the files on disk will not have any immediate effect. You can use `templateReloader` to force the templates to be reloaded:


~~~~ {.haskell}
templateReloader :: (MonadIO m, MonadIO n) =>
                    TemplateDirectory m -- ^ handle returned by newTemplateDirectory'
                 -> n Response
~~~~


so putting those together we get our handlers:

>     simpleHTTP nullConf $ msum
>        [ templateServe td
>        , dir "reload" $ nullDir >> templateReloader td
>        , nullDir >> seeOther "/factorial" (toResponse ())
>        ]


Source code for the app is [here](http://srclink/TemplatesHeist.hs). You will also need to download the source for [factorial.tpl](http://srclink/factorial.tpl) and save it in the same directory as `TemplateHeist.hs`

If you point your browser at [http://localhost:8000/factorial](http://localhost:8000/factorial) you should see the factorial page. You can point your browser at [http://localhost:8000/reload](http://localhost:8000/reload) to reload the template.

Other Heist Features
--------------------

Heist offers a variety of other features not shown here. We have only covered the Happstack integration aspects. For more general information on Heist look at the [Official Heist Template Tutorial](http://snapframework.com/docs/tutorials/heist) and the [Heist Haddock Documenation](http://snapframework.com/docs/latest/heist/index.html).
