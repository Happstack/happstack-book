Note on Next Two Sections
--------------------------

The errors describe in the next section do not happen anymore due to improvements to HSX. However, similar errors can arise so they are still instructive even though they are a bit out of date.

Overlapping Instances
---------------------

Another common error is that of overlapping instances. For example, if
we wrote the following:

~~~~{.haskell}
overlapping = [hsx| <p>overlapping</p> |]
~~~~

We would get an error like:

~~~~
TemplatesHSP.markdown.lhs:495:36:
    Overlapping instances for EmbedAsChild m0 [Char]
      arising from a use of `asChild'
    Matching instances:
      instance [overlap ok] XMLTypeGen m => EmbedAsChild m String
        -- Defined in `HSX.XMLGenerator'
      instance EmbedAsChild Identity String -- Defined in `HSP.Identity'
      instance Monad m => EmbedAsChild (ServerPartT m) String
        -- Defined in `HSP.ServerPartT'
    (The choice depends on the instantiation of `m0'
     To pick the first instance above, use -XIncoherentInstances
     when compiling the other instance declarations)
    In the expression: asChild ("overlapping")
    In the third argument of `genElement', namely
      `[asChild ("overlapping")]'
    In the expression:
      (genElement (Nothing, "p") [] [asChild ("overlapping")])
~~~~

I have never enabled `IncoherentInstances` and actually had it do what
I wanted. In this case, the solution is to add an explicit type
signature that mentions the missing constraint:

> {-
> overlapping :: (EmbedAsChild m String) => XMLGenT m (XMLType m)
> overlapping = <p>overlapping</p>
> -}

In general, there can be a lot of required `EmbedAsChild` and
`EmbedAsAttr` instances. So, often times you can save a lot of typing
by using the `XMLGenerator` class alias:

> {-
> overlapping' :: (XMLGenerator m) => XMLGenT m (XMLType m)
> overlapping' = <p>overlapping</p>
> -}

Ambiguous Types
---------------

Sometimes a type signature for the parent function is not enough. For example, let's say we have:

~~~~{.haskell}

> ambiguous :: (EmbedAsChild m String, StringType m ~ Text) =>
>              XMLGenT m (XMLType m)
> ambiguous = [hsx| <p><% fromString "ambiguous" %></p> |]

~~~~

That will generate an error like this one:

~~~~
TemplatesHSP.markdown.lhs:557:28:
    Ambiguous type variable `c0' in the constraints:
      (IsString c0)
        arising from a use of `fromString'
        at TemplatesHSP.markdown.lhs:557:28-37
      (EmbedAsChild m c0)
        arising from a use of `asChild'
        at TemplatesHSP.markdown.lhs:557:19-25
    Probable fix: add a type signature that fixes these type variable(s)
    In the first argument of `asChild', namely
      `(fromString "ambiguous")'
    In the first argument of `asChild', namely
      `((asChild (fromString "ambiguous")))'
    In the expression: asChild ((asChild (fromString "ambiguous")))
Failed, modules loaded: none.
~~~~

Here we are trying to use `fromString` to convert `"ambiguous"` into
some type, and then we embed that type using `asChild`. But there is
not enough information to figure out what the intermediate type should
be. It is the same problem we have if we try to write:

~~~~{.haskell}
\str -> show (read str)
~~~~

The solution here is to add an explicit type signature to the result of `fromString`:

> {-
> ambiguous :: (EmbedAsChild m Text) => XMLGenT m (XMLType m)
> ambiguous = <p><% (fromString "ambiguous") :: Text %></p> 
> -}

