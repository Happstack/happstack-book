
Handling Submissions
--------------------

In the previous example we only looked at parameters in the
URL. Looking up values from a form submission (a POST or PUT request)
is almost the same. The only difference is we need to first decode the
request body using `decodeBody`:


> {-# LANGUAGE OverloadedStrings #-}
> import Control.Monad                      (msum)
> import Happstack.Server                   ( Response, ServerPart, Method(POST)
>                                           , BodyPolicy(..), decodeBody, defaultBodyPolicy
>                                           , dir, look, nullConf, ok, simpleHTTP
>                                           , toResponse, methodM
>                                           )
> import Text.Blaze                         as B
> import Text.Blaze.Html4.Strict            as B hiding (map)
> import Text.Blaze.Html4.Strict.Attributes as B hiding (dir, label, title)
>
> main :: IO ()
> main = simpleHTTP nullConf $ handlers
>
> myPolicy :: BodyPolicy
> myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)
>
> handlers :: ServerPart Response
> handlers =
>     do decodeBody myPolicy
>        msum [ dir "hello" $ helloPart
>             , helloForm
>             ]
>
> helloForm :: ServerPart Response
> helloForm = ok $ toResponse $
>     html $ do
>       B.head $ do
>         title "Hello Form"
>       B.body $ do
>         form ! enctype "multipart/form-data" ! B.method "POST" ! action "/hello" $ do
>              B.label "greeting: " >> input ! type_ "text" ! name "greeting" ! size "10"
>              B.label "noun: "     >> input ! type_ "text" ! name "noun" ! size "10"
>              input ! type_ "submit" ! name "upload"
>
> helloPart :: ServerPart Response
> helloPart =
>     do methodM POST
>        greeting <- look "greeting"
>        noun     <- look "noun"
>        ok $ toResponse (greeting ++ ", " ++ noun)


Source code for the app is [here](http://srclink/RqDataPost.hs).

Why is `decodeBody` even needed?
--------------------------------

The body of the HTTP request is ignored unless we call
`decodeBody`. The obvious question is,
*"Why isn't the request body automatically decoded?"*

If servers had unlimited RAM, disk, CPU and bandwidth available,
then automatically decoding the body would be a great idea. But, since
that is generally not the case, we need a way to limit or ignore form
submission data that is considered excessive.

A simple solution would be to impose a static quota an all form
data submission server-wide. But, in practice, you might want finer
granularity of control. By explicitly calling `decodeBody`
you can easily configure a site-wide static quota. But you can also
easily adapt the quotas depending on the user, particular form, or
other criteria.

In this example, we keep things simple and just call
`decodeBody` for all incoming requests. If the incoming
request is not a `PUT` or `POST` request with
`multipart/form-data` then calling `decodeBody`
has no side-effects.

Using `BodyPolicy` and `defaultBodyPolicy` to impose quotas
-----------------------------------------------------------

The only argument to `decodeBody` is a `BodyPolicy`. The easiest way to define a `BodyPolicy` is by using the `defaultBodyPolicy` function:


~~~~ {.haskell}
defaultBodyPolicy :: FilePath  -- ^ directory to *temporarily* store uploaded files in
                  -> Int64     -- ^ max bytes to save to disk (files)
                  -> Int64     -- ^ max bytes to hold in RAM (normal form values, etc)
                  -> Int64     -- ^ max header size (this only affects headers
                               --                    in the multipart/form-data)
                  -> BodyPolicy
~~~~


In the example, we define this simple policy:

~~~~ {.haskell}
myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)
~~~~

Since the form does not do file uploads, we set the file quota to 0. We allow 1000 bytes for the two form fields and 1000 bytes for overhead in the `multipart/form-data` encoding.

Using `decodeBody`
------------------

Using `decodeBody` is pretty straight-forward. You simple call it with a `BodyPolicy`. The key things to know are:


 1. You must call it anytime you are processing a POST or PUT request and you want to use `look` and friends
 2. `decodeBody` only works once per request. The first time you call it the body will be decoded. The second time you call it, nothing will happen, even if you call it with a different policy.

Other tips for using `<form>`

When using the `<form>` element there are two important recommendations you should follow:

 1. Set the `enctype` to `multipart/form-data`. This is especially important for forms which contain file uploads.
 2. Make sure to set `method` to `POST` or the form values will show up in the URL as query parameters.

