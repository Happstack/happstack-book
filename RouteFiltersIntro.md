
Route Filters
=============

*a.k.a Responding to different url paths*

Happstack provides a variety of ways to match on parts of the `Request`
(such as the path or request method) and respond appropriately.

Happstack provides two different systems for mapping the request path
to a handler. In this section we will cover a simple, untyped routing
system. Later we will look at fancier, type-safe routing sytem known
as `web-routes`.

Matching on static path segments
--------------------------------

\#include "Dir.lhs" \#include "Dir2.lhs" \#include "Dirs.lhs" \#include
"Path.lhs" \#include "FromReqURI.lhs" \#include "MethodM.lhs" \#include
"MatchMethod.lhs"

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

