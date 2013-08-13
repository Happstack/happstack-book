
`web-routes`
============

The `web-routes` libraries provide a system for type-safe url routing. The basic concept behind type-safe urls is very simple. Instead of working directly with url as strings, we create a type that represents all the possible urls in our web application. By using types instead of strings we benefit in several ways:


fewer runtime errors due to typos

:    If you mistype `"/hmoe"` instead of `"/home"`, the compiler will gleefully compile it. But if you mistype the constructor as `Hmoe` instead of `Home` you will get a compile time error.

Compile type assurance that all routes are mapped to handlers

:    Routing is performed via a simple `case` statement on the url type. If you forget to handle a route, the compiler will give you a `Pattern match(es) are non-exhaustive` warning.

unique URLs for 3rd party libraries

:    Libraries (such as a blog or image gallery component) need a safe way to create urls that do no overlap with the routes provided by other libraries. For example, if a blog component and image component both try to claim the url `/upload`, something bad is going to happen. With `web-routes`, libraries do not have to take any special steps to ensure that the urls they generate are unique. `web-routes` are composable and result in unique urls.

Compile time errors when routes change

:    As a website evolves, existing routes might change or be removed entirely. With `web-routes` this will result in a change to the type. As a result, code that has not been updated will generate a compile-time error, instead of a runtime error. This is especially valuable when using 3rd party libraries, since you may not even be aware that the route had changed otherwise.

better separation of presentation and behavior

:    In `web-routes`, the parsing and printing of a url is separated from the mapping of a url to a handler or creating hyperlinks in your code. This makes it trivial to change the way the url type is converted to a string and back. You need only modify the function that does the conversion, and everything else can stay the same. You do not need to hunt all over the code trying to find places that use the old format.

automatic sitemap

:    Because the url type represents all the valid routes on your site, it also acts as a simple sitemap.


`web-routes` is designed to be very flexible. For example, it does not require that you use any particular mechanism for defining the mapping between the url type and the url string. Instead, we provide a variety of addon packages that provide different methods including, template-haskell, generics, parsec, quasi-quotation, and more. This means it is also easy to add your own custom mechanism. For example, you might still use template-haskell, but with a different set of rules for converting a type to a string.

`web-routes` is also not limited to use with any particular framework, templating system, database, etc.
