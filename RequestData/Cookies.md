
Working with Cookies
--------------------

HTTP is a stateless protocol. Each incoming `Request` is processed
with out any memory of any previous communication with the
client. Though, from using the web, you know that it certainly doesn't
feel that way. A website can remember that you logged in, items in
your shopping cart, etc. That functionality is implemented by using
`Cookies`.

When the server sends a `Response` to the client, it can include a special `Response` header named `Set-Cookie`, which tells the client to remember a certain `Cookie`. A `Cookie` has a name, a string value, and some extra control data, such as a lifetime for the cookie.

The next time the client talks to the server, it will include a copy of the `Cookie` value in its `Request` headers. One possible use of cookies is to store a session id. When the client submits the cookie, the server can use the session id to look up information about the client and <i>remember</i> who they are. Sessions and session ids are not built-in to the HTTP specification. They are merely a common idiom which is provided by many web frameworks.

