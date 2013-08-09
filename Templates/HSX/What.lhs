
What do `hsx2hs` and `[hsx| |]` actually do?
--------------------------------------------

In order to use HSX it is very useful to understand what is actually
going on behind the magic. In these examples we are actually use to
separate, but related libraries, the `hsx2hs` library and the `hsp`
library.

If we have the line:

~~~~{.haskell}
foo :: XMLGenT (ServerPartT IO) XML
foo = <span class="bar">foo</span>
~~~~

and we run `hsx2hs`, it gets turned into a line like this:

~~~~{.haskell}
foo :: XMLGenT (ServerPartT IO) XML
foo = genElement (Nothing, "span")
        [ asAttr ("class" := "bar") ] [asChild ("foo")]
~~~~

We see that the XML syntax has simply been translated into normal haskell function calls.

The `hsx` `QuasiQuoter` performs the same transformation as `hsx2hs`.

This is all that `hsx2hs` does. An important thing to note is that `hsx2hs` does not include any functions of those names or even specify their type signatures. The functions come from another library -- in this case `hsp`. However, you could implement different behavior by importing a different library.

the `XMLGenT` type
------------------

There are a few types and classes that you will need to be familiar
with from the `hsp` library. The first type is the `XMLGenT` monad transformer:

~~~~{.haskell}
newtype XMLGenT m a = XMLGenT (m a)

-- | un-lift.
unXMLGenT :: XMLGenT m a -> m a
unXMLGenT (XMLGenT ma) =  ma
~~~~

This seemingly useless type exists solely to make the type-checker happy. Without it we would need an instance like:

~~~~{.haskell}
instance ( EmbedAsChild (IdentityT m) a
         , Functor m
         , Monad m
         , m ~ n
         ) =>
         EmbedAsChild (IdentityT m) (n a) where
  asChild = ...
~~~~

Unfortunately, because `(n a)` is so vague, that results in
overlapping instances which cannot be resolved without
`IncohorentInstances`. And, in my experience, enabling
`IncohorentInstances` is *never* the right solution.

So, when generating XML you will generally need to apply `unXMLGenT`
to the result to remove the `XMLGenT` wrapper as we did in the `hello`
function. Anyone who can figure out to do away with the `XMLGenT`
class will be my personal hero.

the `XMLGen` class
------------------

Next we have the `XMLGen` class:

~~~~{.haskell}
class Monad m => XMLGen m where
 type XMLType m
 type StringType m
 data ChildType m
 data AttributeType m
 genElement    :: Name (StringType m) -> [XMLGenT m [AttributeType m]]
               -> [XMLGenT m [ChildType m]] -> XMLGenT m (XMLType m)
 genEElement   :: Name (StringType m) -> [XMLGenT m [AttributeType m]]                              -> XMLGenT m (XMLType m)
 genEElement n ats = genElement n ats []
 xmlToChild    :: XMLType m    -> ChildType m
 pcdataToChild :: StringType m -> ChildType m
~~~~

You will notice that we have a type-class instead of just simple
functions and types. One feature of HSX is that it is not tied to any
particular XML representation. Instead, the XML representation is
based on the monad we are currently inside. For example, inside of a
javascript monad, we might generate javascript code that renders the
XML, inside of another monad, we might generate the `Node` type used
by the `heist` template library. We will see some examples of this in
a later section.

The `data` and `type` declarations appearing inside the class
declaration are allowed because of the `TypeFamilies` extension. For a
detailed coverage of type families see [this wiki
entry](http://www.haskell.org/haskellwiki/GHC/Type_families).

Most of these functions and types are used internally and not used
directly by the developer. You will, however, see two of the
associated types appear in your type signatures.

the `XMLType m` type synonym
------------------------

The `XMLGen` type-class defines an associated type synonym `XMLType m`:

~~~~{.haskell}
type XMLType m
~~~~

`XMLType m` is a synonym for whatever the xml type is for the monad `m`. We can write an XML fragment that is parameterized over an arbitrary monad and xml type like this:

~~~~~{.haskell}
bar :: (XMLGenerator m) => XMLGenT m (XMLType m)
bar = <span>bar</span>
~~~~~

the `StringType m` type synonym
--------------------------------

The `XMLGen` type-class also defines an associated type synonym `StringType m`:

~~~~{.haskell}
type StringType m
~~~~

That is because some types, such as `HSP.XML.XML` are based around `Text`, while others, such as `JMacro` are based around `String`.


the `EmbedAsChild` class
------------------------

The `EmbedAsChild` is used to turn a value into a list of children of an element:

~~~~{.haskell}
type GenChildList m     = XMLGenT m [Child m]

-- | Embed values as child nodes of an XML element. The parent type
-- will be clear from the context so it is not mentioned.
class XMLGen m => EmbedAsChild m c where
  asChild :: c -> GenChildList m
~~~~

There are generally many instances of `EmbedAsChild` allowing you to embed `String`, `Text`, `Int`, and other values. You might find it useful to create additional instances for types in your program. We will some some examples later in this tutorial.

To use the `EmbedAsChild` class we us the `<% %>` syntax shown earlier. For example, when we write:

~~~~{.haskell}
a :: (XMLGenerator m) => GenChildList m
a = <% 'a' %>
~~~~

It gets turned into:

~~~~{.haskell}
a :: (XMLGenerator m) => GenChildList m
a = (asChild ('a'))
~~~~

the `EmbedAsAttr` class
-----------------------

The `EmbedAsAttr` class is similar to the `EmbedAsChild` class. It is used to turn arbitrary values into element attributes.

~~~~{.haskell}
type GenAttributeList m = XMLGenT m [Attribute m]

-- | Similarly embed values as attributes of an XML element.
class XMLGen m => EmbedAsAttr m a where
   asAttr :: a -> GenAttributeList m
~~~~

If we have some attributes like this:

~~~~{.haskel}
foo = <span class="foo" size=(80 :: Int) bogus=False>foo</span>
~~~~

It will get translated to:

~~~~{.haskell}
foo
 = (genElement (Nothing, "span")
      [asAttr ("class" := "foo"), asAttr ("size" := (80 :: Int)),
       asAttr ("bogus" := False)]
      [asChild ("foo")])
~~~~

which might be rendered as:

    <span class="foo" size="80" bogus="false">foo</span>

the `XMLGenerator` class

You may have noticed that some of the examples had a class constraint `(XMLGenerator m)`:

~~~~{.haskell}
bar :: (XMLGenerator m) => XMLGenT m (XMLType m)
bar = <span>bar</span>
~~~~

`XMLGenerator` is just a class alias. It is defined as such:

~~~~{.haskell}
class ( XMLGen m
      , SetAttr m (XMLType m)
      , AppendChild m (XMLType m)
      , EmbedAsChild m (XMLType m)
      , EmbedAsChild m [XMLType m]
      , EmbedAsChild m Text
      , EmbedAsChild m Char -- for overlap purposes
      , EmbedAsChild m ()
      , EmbedAsAttr m (Attr Text Text)
      , EmbedAsAttr m (Attr Text Int)
      , EmbedAsAttr m (Attr Text Bool)
      ) => XMLGenerator m
~~~~

It contains a list of common instances that all xml generation monads are expected to provide. It just saves you from having to list all thoses instances by hand when you use them.

`HSPT` Monad
------------

The module `HSP.Monad` defines a type:

~~~~{.haskell}
newtype HSPT xml m a = HSPT { unHSPT :: m a }
~~~~

There is an `XMLGenerator` instance for it which can be used to generate `XML`:

~~~~{.haskell}
instance (Functor m, Monad m) => XMLGenerator (HSPT XML m)
~~~~

The `HSPT` type is basically the same as the `IdentityT` type except it has the `phantom` parameter `xml`. This makes it possible to create multiple `XMLGenerator` instances for `HSPT` that generate different xml types.

HSX by Example
--------------

First we have a simple function to render the pages and print them to stdout:

> {-# LANGUAGE FlexibleContexts, QuasiQuotes,
>     TypeFamilies, OverloadedStrings #-}
> import Control.Monad.Identity      (Identity(..))
> import           Data.Text.Lazy    (Text)
> import qualified Data.Text.Lazy.IO as Text
> import Data.String                 (fromString)
> import Language.Haskell.HSX.QQ     (hsx)
> import HSP
> import HSP.Monad
> import HSP.HTML4
> import Happstack.Server.HSP.HTML (defaultTemplate)
>
> printXML :: HSPT XML Identity XML -> IO ()
> printXML = Text.putStrLn . renderAsHTML . runIdentity . unHSPT
>

HSX and `do` syntax
-------------------

It is possible to use hsx markup inside a `do`-block:

> doBlock :: (XMLGenerator m, StringType m ~ Text) =>
>            XMLGenT m (XMLType m)
> doBlock =
>    do [hsx| <div>
>        <p>A child element</p>
>       </div> |]

Notice that we indent the closing `</div>` tag. That indentation
rule is consistent with the specification for how do-notation
works. It is intend for the same reason that `if .. then .. else ..`
blocks have to be idented in a special way inside `do`-blocks.

In newer versions of HSX, this restriction has been lifted.

`defaultTemplate`
-----------------

There is a bit of boiler plate that appears in ever html document such as the `<html>`, `<head>`, `<title>`, and `<body>`; tags. The `defaultTemplate` function from `happstack-hsp` provides a minimal skeleton template with those tags:

~~~~{.haskell}
module Happstack.Server.HSP.HTML where

defaultTemplate :: ( XMLGenerator m, EmbedAsChild m headers
                   , EmbedAsChild m body, StringType m ~ Text) =>
                   Text     -- ^ text for \<title\> tag
                -> headers  -- ^ extra headers for \<head\> tag.
                                 Use @()@ if none.
                -> body     -- ^ content for \<body\> tags.
                -> m (XMLType m)

~~~~

How to embed empty/nothing/zero
-------------------------------

`defaultTemplate` requires that we pass in `headers` and a `body`. But what if we don't have any headers that we want to add?

Most `XMLGenerator` monads provide an `EmbedAsChild m ()` instance, such as this one:

~~~~{.haskell}
instance EmbedAsChild (HSPT XML m) () where
    asChild () = return []
~~~~

So, we can just pass in `()` like so:


> empty :: IO ()
> empty = printXML $ defaultTemplate "empty" () ()

Which will render as such:

~~~~
 <html
  ><head
   ><title
     >empty</title
     ></head
   ><body
   ></body
   ></html
 >
~~~~


Creating a list of children
---------------------------

Sometimes we want to create a number of child elements without knowing
what their parent element will be. We can do that using the:

    <%> ... </%>

syntax. For example, here we return two paragraphs:


> twoParagraphs :: (XMLGenerator m, StringType m ~ Text) =>
>                  XMLGenT m [ChildType m]
> twoParagraphs = [hsx|
>   <%>
>    <p>Paragraph one</p>
>    <p>Paragraph two</p>
>   </%>
> |]


We can embed `twoParagraphs` in parent element using the normal `<% %>` syntax:

> twoParagraphsWithParent :: (XMLGenerator m, StringType m ~ Text) =>
>                            XMLGenT m (XMLType m)
> twoParagraphsWithParent = [hsx|
>     <div>
>      <% twoParagraphs %>
>     </div>
> |]

`if .. then .. else .. `
------------------------

Using an `if .. then .. else ..` is straight-foward. But what happens
when you don't really want an `else` case? This is another place we
can use `()`:

> ifThen :: Bool -> IO ()
> ifThen bool =
>     printXML $ defaultTemplate "ifThen" () $ [hsx|
>      <div>
>       <% if bool
>          then <%
>                <p>Showing this thing.</p>
>               %>
>         else <% () %>
>        %>
>      </div> |]
>
>

Lists of attributes & optional attributes
-----------------------------------------

Normally attributes are added to an element using the normal html
attribute syntax. HSX, has a special extension where the last
attribute can be a Haskell expression which returns a list of
attributes to add to the element. For example:


> attrList :: IO ()
> attrList =
>    printXML $ defaultTemplate "attrList" () $ [hsx|
>     <div id="somediv" [ "class" := "classy"
>                       , "title" := "untitled" :: Attr Text Text
>                       ] >
>     </div> |]


The type of the elements of the list can be anything with an
`EmbedAsAttr m a` instance. In this case we create a list of `Attr`
values:

~~~~{.haskell}
data Attr n a = n := a
~~~~

We need the type annotation `Attr Text Text` because, due to `OverloadedStrings` the compiler can't automatically determine what type we want for the string literals.

We can use the attribute list feature to conditionally add attributes using a simple
`if .. then .. else ..` statment:



> optAttrList :: Bool -> IO ()
> optAttrList bool =
>   printXML $ defaultTemplate "attrList" () $ [hsx|
>    <div id="somediv" (if bool
>                       then [ "class" := "classy"
>                            , "title" := "untitled" :: Attr Text Text]
>                       else []) >
>    </div> |]

A clever trick here is to use the list comprehension syntax as an alternative to the `if .. then .. else`:

> optAttrList2 :: Bool -> IO ()
> optAttrList2 bool =
>     printXML $ defaultTemplate "attrList" () $ [hsx|
>      <div id="somediv"
>         [ attr | attr <- [ "class" := "classy"
>                          , "title" := "untitled" :: Attr Text Text]
>         , bool] >
>     </div> |]

this trick works better when you are only attempting to add a single extra attribute:

> optAttrList3 :: Bool -> IO ()
> optAttrList3 bool =
>     printXML $ defaultTemplate "attrList" () $ [hsx|
>      <div id="somediv"
>        [ "class" := "classy" :: Attr Text Text | bool] >
>      </div> |]


Source code for the app is [here](http://srclink/HSX/What.hs).

HSX and compilation errors
--------------------------

One drawback to HSX is that it can result in some pretty ugly (and
sometimes very long) error messages. Fortunately, the errors are
almost always the same type of thing, so after a little experience it
is easy to see what is going wrong. Here are some tips if you run into
errors:

`hsx2hs` line numbers are usually wrong
---------------------------------------

As we saw, `hsx2hs` transforms the literal XML into normal Haskell
code. Unfortunately, the error positions reported by GHC reflect where
the error occurred in the transformed code, not the original
input. `hsx2hs` tries to help GHC by inserting `LINE` pragmas. While that
helps to a degree, it still leaves a fair bit of fuzz.

The trick is to look towards the bottom of the error message where it
will usually show you the expression that contained the error. For
example, if we have:

~~~~{.haskel}
typeError :: (XMLGenerator m, StringType m ~ Text) =>
             XMLGenT m (XMLType m)
typeError = [hsx| <foo><% 1 + 'a' %></foo> |]
~~~~

We will get an error like:

~~~~
Templates/HSX/What.lhs:456:20:
Could not deduce (Num Char) arising from a use of `+'
from the context (XMLGenerator m, StringType m ~ Text)
  bound by the type signature for
     typeError :: (XMLGenerator m, StringType m ~ Text) =>
                  XMLGenT m (XMLType m)
  at Templates/HSX/What.lhs:455:16-77
Possible fix: add an instance declaration for (Num Char)
In the first argument of `asChild', namely `(1 + 'a')'
In the first argument of `asChild', namely `((asChild (1 + 'a')))'
In the expression: asChild ((asChild (1 + 'a')))
~~~~

The last line says:

~~~~
    In the expression: asChild ((asChild (1 + 'a')))
~~~~

And the sub-expresion `1 + 'a'` is, indeed, where the type error is.

A bug report about the line number issue has been filed, and there are ideas on how to fix it. You can [read more here](http://groups.google.com/group/haskell-server-pages/browse_thread/thread/1b136c7acb448136).

Using the new hsx quasi-quoters helps significantly with the accuracy of line numbers.
