
Serving Files from a Directory
------------------------------

The most common way to serve files is by using `serveDirectory`:


~~~~ {.haskell}
data Browsing  = EnableBrowsing | DisableBrowsing

serveDirectory :: ( WebMonad Response m, ServerMonad m, FilterMonad Response m
                  , MonadIO m, MonadPlus m
                  ) =>
                  Browsing    -- ^ enable/disable directory browsing
               -> [FilePath]  -- ^ index file names
               -> FilePath    -- ^ file/directory to serve
               -> m Response
~~~~


For example:

~~~~ {.haskell}
serveDirectory EnableBrowsing ["index.html"] "path/to/directory/on/disk"
~~~~

If the requested path does not map to a file or directory, then `serveDirectory` returns `mzero`.

If the requested path is a file then the file is served normally using `serveFile`.

When a directory is requested, `serveDirectory` will first try to find one of the index files (in the order they are listed). If that fails, it will show a directory listing if `EnableBrowsing`, otherwise it will return `forbidden "Directory index forbidden"`.

The formula for mapping the URL to a file on disk is just what you would expect:


    path/to/directory/on/disk </> unconsumed/portion/of/request/url


So if the handler is:


~~~~ {.haskell}
 dir "static" $
  serveDirectory EnableBrowsing ["index.html"] "/srv/mysite/data"
~~~~


And the request URL is:


    http://localhost/static/foo/bar.html


Then we are going to look for:


    /srv/mysite/data </> foo/bar.html => /srv/mysite/data/foo/bar.html


The following demo will allow you to browse the directory that the server is running in. (So be careful where you run it).


> module Main where
>
> import Happstack.Server ( Browsing(EnableBrowsing), nullConf
>                         , serveDirectory, simpleHTTP
>                         )
>
> main :: IO ()
> main = simpleHTTP nullConf $ serveDirectory EnableBrowsing [] "."

Source code for the app is [here](http://srclink/FileServingDirectory.hs).

Simply run it and point your browser at [http://localhost:8000/](http://localhost:8000/)

File Serving Security
---------------------

The request URL is sanitized so that users can not escape the top-level directory by adding extra `..` or `/` characters to the URL.

The file serving code *will* follow symlinks. If you do not want that behavior then you will need to roll your own serving function. See the section on *Advanced File Serving* for more information.
