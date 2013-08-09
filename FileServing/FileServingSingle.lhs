
Serving a Single File
---------------------

Sometimes we want to serve files from disk whose name is not a direct
mapping from the URL. For example, let's say that you have an image
and you want to allow the client to request the images in different
sizes by setting a query parameter. e.g.

    http://localhost:8000/images/photo.jpg?size=medium

Clearly, we can not just map the path info portion of the URL to a file disk, because all the different sizes have the same name -- only the query parameter is different. Instead, the application will use some custom algorithm to calculate where the image lives on the disk. It may even need to generate the resized image on-demand. Once the application knows where the file lives on disk it can use `serveFile` to send that file as a `Response` using `sendFile`:


~~~~ {.haskell}
serveFile :: ( ServerMonad m
             , FilterMonad Response m
             , MonadIO m
             , MonadPlus m
             ) =>
             (FilePath -> m String) -- ^ function for determining
                                    --   content-type of file.
                                    --   Usually 'asContentType'
                                    --   or 'guessContentTypeM'
          -> FilePath               -- ^ path to the file to serve
          -> m Response
~~~~


The first argument is a function which calculates the mime-type for a `FilePath`. The second argument is path to the file to send. So we might do something like:


~~~~ {.haskell}
serveFile (guessContentTypeM mimeTypes) "/srv/photos/photo.jpg"
~~~~


Note that even though the file is named `photo_medium.jpg` on the disk, that name is not exposed to the client. They will only see the name they requested, i.e., `photo.jpg`.

`guessContentTypeM` will guess the content-type of the file by looking at the filename extension. But, if our photo app only supports JPEG files, there is no need to guess. Furthermore, the name of the file on the disk may not even have the proper extension. It could just be the md5sum of the file or something. So we can also hardcode the correct content-type:


~~~~ {.haskell}
serveFile (asContentType "image/jpeg") "/srv/photos/photo.jpg"
~~~~


The following, example attempts to serve its own source code for any incoming request.


> module Main where
>
> import Happstack.Server ( asContentType, nullConf
>                          , serveFile, simpleHTTP)
>
> main :: IO ()
> main =
>  simpleHTTP nullConf $
>   serveFile (asContentType "text/x-haskell") "FileServingSingle.hs"

Source code for the app is [here](http://srclink/FileServingSingle.hs).

