Route Filters
=============

*a.k.a Responding to different url paths*

Happstack provides a variety of ways to match on parts of the `Request`
(such as the path or request method) and respond appropriately.

Happstack provides two different systems for mapping the request path
to a handler. In this section we will cover a simple, untyped routing
system. Later we will look at fancier, type-safe routing sytem known
as `web-routes`.
