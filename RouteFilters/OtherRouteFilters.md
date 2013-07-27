Other Routing Filters
---------------------

SimpleHTTP includes a number of other useful routing filters, such as:

`nullDir :: (ServerMonad m, MonadPlus m) => m ()`
:   check that there are no path segments remaining
`host :: (ServerMonad m, MonadPlus m) => String -> m a -> m a`
:   match on a specific host name in the Request
`withHost :: (ServerMonad m, MonadPlus m) => (String -> m a) -> m a`
:   Lookup the host header and pass it to the handler.
`uriRest :: (ServerMonad m) => (String -> m a) -> m a`
:   Grab the rest of the URL (dirs + query) and passes it to your
    handler
`anyPath :: (ServerMonad m, MonadPlus m) => m r -> m r`
:   Pop any path element and ignore when choosing a 'ServerPartT' to
    handle the request.
`trailingSlash :: (ServerMonad m, MonadPlus m) => m ()`
:   Guard which checks that the Request URI ends in `/`. Useful for
    distinguishing between `foo` and `foo/`

