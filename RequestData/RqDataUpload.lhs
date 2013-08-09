
File Uploads
------------

The `lookFile` function is used to extract an uploaded file:


~~~~ {.haskell}
lookFile :: String -> RqData (FilePath, FilePath, ContentType)
~~~~


It returns three values:

 1. The location of the temporary file which holds the contents of the file
 2. The *local* filename supplied by the browser. This is typically the name of the file on the users system.
 3. The `content-type` of the file (as supplied by the browser)

The temporary file will be automatically deleted after the `Response`
is sent. Therefore, it is essential that you move the file from the
temporary location.

In order for file uploads to work correctly, it is also essential that
your `<form>` element contains the attributes
`enctype="multipart/form-data"` and `method="POST"`

The following example has a form which allows a user to upload a
file. We then show the temporary file name, the uploaded file name,
and the content-type of the file. In a real application, the code
should use `System.Directory.renameFile` (or similar) to move the
temporary file to a permanent location. This example looks a bit long,
but most of the code is just HTML generation using BlazeHtml. The only
really new part is the use of the `lookFile` function. Everything else
should already have been covered in previous sections. So if you don't
understand something, try looking in earlier material.


> {-# LANGUAGE OverloadedStrings #-}
> import Control.Monad    (msum)
> import Happstack.Server
>     ( Response, ServerPart, defaultBodyPolicy
>     , decodeBody, dir, lookFile, nullConf, ok
>     , simpleHTTP, toResponse )
> import Text.Blaze                         as B
> import Text.Blaze.Html4.Strict            as B hiding (map)
> import Text.Blaze.Html4.Strict.Attributes as B hiding (dir, title)

> main :: IO ()
> main = simpleHTTP nullConf $ upload
>
> upload :: ServerPart Response
> upload =
>     do decodeBody (defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000)
>        msum [ dir "post" $ post
>             , uploadForm
>             ]
>
> uploadForm :: ServerPart Response
> uploadForm = ok $ toResponse $
>     html $ do
>       B.head $ do
>         title "Upload Form"
>       B.body $ do
>         form ! enctype "multipart/form-data"
>              ! B.method "POST"
>              ! action "/post" $ do
>                input ! type_ "file" ! name "file_upload" ! size "40"
>                input ! type_ "submit" ! value "upload"
>
> post :: ServerPart Response
> post =
>    do r <- lookFile "file_upload"
>       ok $ toResponse $
>          html $ do
>            B.head $ do
>              title "Post Data"
>            B.body $ mkBody r
>     where
>       mkBody (tmpFile, uploadName, contentType) = do
>         p (toHtml $ "temporary file: " ++ tmpFile)
>         p (toHtml $ "uploaded name:  " ++ uploadName)
>         p (toHtml $ "content-type:   " ++ show contentType)

Source code for the app is [here](http://srclink/RqDataUpload.hs).

File uploads important reminder
-------------------------------

Remember that you must move the temporary file to a new location or it
will be garbage collected after the Response is sent. In the example
code we do *not* move the file, so it is automatically deleted.

