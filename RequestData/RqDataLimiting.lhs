
Limiting lookup to `QUERY_STRING` or request body
-----------------------------------------------

By default, `look` and friends will search both the QUERY_STRING the
request body (aka, `POST`/`PUT` data) for a key. But sometimes we want
to specify that only the `QUERY_STRING` or request body should be
searched. This can be done by using the `body` and `queryString`
filters:


~~~~ {.haskell}
body        :: (HasRqData m) => m a -> m a
queryString :: (HasRqData m) => m a -> m a
~~~~

Using these filters we can modify `helloPart` so that the `greeting` must come from the QUERY_STRING and the `noun` must come from the request body:

~~~~ {.haskell}
helloPart :: ServerPart String
helloPart =
    do greeting <- queryString $ look "greeting"
       noun     <- body        $ look "noun"
       ok $ greeting ++ ", " ++ noun
~~~~

`queryString` and `body` act as filters which
only pass a certain subset of the data through. If you were to
write:

~~~~ {.haskell}
greetingRq :: ServerPart String
greetingRq =
    body (queryString $ look "greeting")
~~~~

This code would never match anything because the
`body` filter would hide all the `QUERY_STRING` values, and
the `queryString` filter would hide all the request body
values, and hence, there would be nothing left to search.
