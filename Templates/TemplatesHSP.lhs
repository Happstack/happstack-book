
Using HSX/HSP
=============

To enable HSX support, you must install the happstack-hsp package.

HSX is an XML-based templating system that allows you to embed XML in your Haskell source files. If you have ever had to use PHP, you may want to run screaming from this idea. However, the HSX solution is far saner than the PHP solution, so you may want to give it a chance.

The first thing we will see is a funny `OPTIONS_GHC` pragma at the top of our file:

> {-# LANGUAGE FlexibleContexts, OverlappingInstances #-}
> {-# OPTIONS_GHC -F -pgmF trhsx #-}
> module Main where
>

HSX works by running the code through an external pre-processor named `trhsx`. This pragma at the top is how we tell GHC that this file needs to be run through the `trhsx` pre-processor in order to work. So, that options line looks a bit like line noise. You can try to remember it like this:

 1. `-F` says we want to filter the source code (or maybe trans*F*orm the source code)
 2. `-pgmF` specifies the program we want to do the transformation
 3. `trhsx` is short for *tr*ansform using *hsx*

Next we have some imports:

> import Control.Applicative ((<$>))
> import Control.Monad.Identity (Identity(runIdentity))
> import Data.String (IsString(fromString))
> import Data.Text   (Text)
> import qualified HSX.XMLGenerator as HSX
> import Happstack.Server.HSP.HTML
> import Happstack.Server (Request(rqMethod), ServerPartT, askRq, nullConf, simpleHTTP)
> import HSP.Identity () -- instance (XMLGen Identity)
>

Now we can define a function which generates an HTML page:

> hello :: ServerPartT IO XML
> hello = unXMLGenT
>   <html>
>    <head>
>     <title>Hello, HSP!</title>
>    </head>
>    <body>
>     <h1>Hello HSP!</h1>
>     <p>We can insert Haskell expression such as this: <% sum [1 .. (10 :: Int)] %></p>
>     <p>We can use the ServerPartT monad too. Your request method was: <% getMethod %></p>
>     <hr/>
>     <p>We don't have to escape & or >. Isn't that nice?</p>
>     <p>If we want <% "<" %> then we have to do something funny.</p>
>     <p>But we don't have to worry about escaping <% "<p>a string like this</p>" %></p>
>     <p>We can also nest <% <span>like <% "this." %> </span> %></p>
>    </body>
>   </html>
>       where
>       getMethod :: XMLGenT (ServerPartT IO) String
>       getMethod = show . rqMethod <$> askRq
>

> main :: IO ()
> main = simpleHTTP nullConf $ hello
>

The first thing we notice is that syntax looks pretty much like normal HTML syntax. There are a few key differences though:

  1. like XML, all tags must be closed
  2. like XML, we can use shortags (e.g. `<hr />`)
  3. We do not have to escape & and >
  4. To embed < we have to do something extra funny

The syntax:

    <% haskell-expression %>

allows us to embed a Haskell expression inside of literal XML.

As shown in this line:

~~~~{.haskell}
<p>We can also nest <% <span>like <% "this." %> </span> %></p>
~~~~

we can freely nest Haskell and XML expressions.

What does `trhsx` do?
---------------------

In order to use HSX it is very useful to understand what is actually
going on behind the magic. If we have the line:

~~~~{.haskell}
foo :: XMLGenT (ServerPartT IO) XML
foo = <span class="bar">foo</span>
~~~~

and we run `trhsx`, it gets turned into a line like this:


~~~~{.haskell}
foo :: XMLGenT (ServerPartT IO) XML
foo = genElement (Nothing, "span") [ asAttr ("class" := "bar") ] [asChild ("foo")]
~~~~

We see that the XML syntax has simply been translated into normal haskell function calls.

the `XMLGenT` type
------------------

There are a few types and classes that you will need to be familiar
with. The first type is the `XMLGenT` monad transformer:

~~~~{.haskell}
newtype XMLGenT m a = XMLGenT (m a)

-- | un-lift.
unXMLGenT :: XMLGenT m a -> m a
unXMLGenT (XMLGenT ma) =  ma
~~~~

This seemingly useless type exists solely to make the type-checker happy. Without it we would need an instance like:

~~~~{.haskell}
instance (EmbedAsChild (IdentityT m) a, Functor m, Monad m, m ~ n) =>
         EmbedAsChild (IdentityT m) (n a) where
  asChild = ...
~~~~

Unfortunately, because `(n a)` is so vague, that results in
overlapping instances that cannot be resolved without
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
 type XML       m
 data Child     m
 data Attribute m
 genElement    :: Name
               -> [XMLGenT m [Attribute m]]
               -> [XMLGenT m [Child m]]
               -> XMLGenT m (XML m)
 genEElement   :: Name
               -> [XMLGenT m [Attribute m]]
               -> XMLGenT m (XML m)
 genEElement n ats = genElement n ats []
 xmlToChild    :: XML m -> Child m
 pcdataToChild :: String -> Child m
~~~~

Most of these functions and types are used internally and not used directly by the developer.

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

the `XML m` type synonym
------------------------

The `XMLGen` type-class defines an associated type synonym `XML m`:

~~~~{.haskell}
type XML m
~~~~

`XML m` is a synonym for whatever the xml type is for the monad `m`. We can write an XML fragment that is parameterized over an arbitrary monad and xml type like this:

~~~~~{.haskell}
bar :: (XMLGenerator m) => XMLGenT m (XMLType m)
bar = <span>bar</span>
~~~~~

Note that we had this qualified import:

~~~~{.haskell}
import qualified HSX.XMLGenerator as HSX
~~~~

That is because we need to differentiate the `XML` associated type
synonym from the plain-old `XML` data type that is declared
elsewhere. Having two types with the same name is a bit silly, but
that is the way it is for now.

the `EmbedAsChild` class
------------------------

The `EmbedAsChild` is used to turn a value into a list of children of an element:

~~~~{.haskell}
type GenChildList m     = XMLGenT m [Child m]

-- | Embed values as child nodes of an XML element. The parent type will be clear
-- from the context so it is not mentioned.
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
      , SetAttr      m (XMLType m)
      , AppendChild  m (XMLType m)
      , EmbedAsChild m (XMLType m)
      , EmbedAsChild m [XMLType m]
      , EmbedAsChild m String
      , EmbedAsChild m Char
      , EmbedAsAttr  m (Attr String String)
      , EmbedAsAttr  m (Attr String Int)
      , EmbedAsAttr  m (Attr String Bool)
      ) => XMLGenerator m
~~~~

It contains a list of common instances that all xml generation monads are expected to provide. It just saves you from having to list all thoses instances by hand when you use them.

HSX by Example
--------------

First we have a simple function to render the pages and print them to stdout:

~~~~{.haskell}
printXML :: Identity XML -> IO ()
printXML = putStrLn . renderAsHTML . runIdentity
~~~~

HSX and `do` syntax
-------------------

It is possible to use hsx markup inside a `do`-block. If you are using an older version of hsx, you just need to be aware of one little catch. In this example:

~~~~{.haskell}
doBlock :: (XMLGenerator m) => XMLGenT m (XMLType m)
doBlock =
    do <div>
        <p>A child element</p>
        </div>
~~~~

Notice that we indent the closing `</div>` tag. That indentation
rule is consistent with the specification for how do-notation
works. It is intend for the same reason that `if .. then .. else ..`
blocks have to be idented in a special way inside `do`-blocks.

In newer versions of HSX, this restriction has been lifted.

`defaultTemplate`
-----------------

There is a bit of boiler plate that appears in ever html document such as the `<html>`, `<head>`, `<title>`, and `<body>`; tags. The `defaultTemplate` function provides a minimal skeleton template with those tags:

~~~~{.haskell}
defaultTemplate :: ( XMLGenerator m
                   , EmbedAsChild m body
                   , EmbedAsChild m headers
                   ) =>
                   String   -- string to put in <title>
                -> headers  -- additional elements to put in <head>
                -> body     -- elements to put in <body>
                -> m (XMLType m)
~~~~

How to embed empty/nothing/zero
-------------------------------

`defaultTemplate` requires that we pass in `headers` and a `body`. But what if we don't have any headers that we want to add?

Most `XMLGenerator` monads provide an `EmbedAsChild m ()` instance, such as this one:

~~~~{.haskell}
instance EmbedAsChild Identity () where
    asChild () = return []
~~~~

So, we can just pass in `()` like so:

~~~~{.haskell}
empty :: IO ()
empty = printXML $ defaultTemplate "empty" () ()
~~~~

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

~~~~{.haskell}
twoParagraphs :: (XMLGenerator m) => XMLGenT m [HSX.Child m]
twoParagraphs =
    <%>
     <p>Paragraph one</p>
     <p>Paragraph two</p>
   </%>
~~~~

We can embed those in parent element like this:

~~~~{.haskell}
twoParagraphsWithParent :: (XMLGenerator m) => XMLGenT m (XMLType m)
twoParagraphsWithParent =
    <div>
     <% twoParagraphs %>
    </div>
~~~~

`if .. then .. else .. `
------------------------

Using an `if .. then .. else ..` is straight-foward. But what happens
when you don't really want an `else` case? This is another place we
can use `()`:

~~~~{.haskell}
ifThen :: Bool -> IO ()
ifThen bool =
    printXML $ defaultTemplate "ifThen" () $
     <div>
      <% if bool
         then <%
               <p>Showing this thing.</p>
              %>
         else <% () %>
       %>
     </div>
~~~~

Lists of attributes & optional attributes
-----------------------------------------

Normally attributes are added to an element using the normal html
attribute syntax. HSX, has a special extension where the last
attribute can be a Haskell expression which returns a list of
attributes to add to the element. For example:

~~~~{.haskell}
attrList :: IO ()
attrList =
    printXML $ defaultTemplate "attrList" () $
     <div id="somediv" ["class" := "classy", "title" := "untitled"] >
     </div>
~~~~

The type of the elements of the list can be anything with an
`EmbedAsAttr m a` instance. In this case we create a list of `Attr`
values:

~~~~{.haskell}
data Attr n a = n := a
~~~~

We can use this feature to conditionally add attributes using a simple
`if .. then .. else ..` statment:


~~~~{.haskell}
optAttrList :: Bool -> IO ()
optAttrList bool =
    printXML $ defaultTemplate "attrList" () $
     <div id="somediv" (if bool
                          then ["class" := "classy", "title" := "untitled"]
                          else []) >
     </div>
~~~~

Source code for the app is [here](http://srclink/TemplatesHSP.hs).

HSX and compilation errors
--------------------------

One drawback to HSX is that it can result in some pretty ugly (and
sometimes very long) error messages. Fortunately, the errors are
almost always the same type of thing, so after a little experience it
is easy to see what is going wrong. Here are some tips if you run into
errors:

Line numbers are usually wrong
------------------------------

As we saw, `trhsx` transforms the literal XML into normal Haskell
code. Unfortunately, the error positions reported by GHC reflect where
the error occurred in the transformed code, not the original
input. HSX tries to help GHC by inserting LINE pragmas. While that
helps to a degree, it still leaves a fair bit of fuzz.

The trick is to look towards the bottom of the error message where it
will usually show you the expression that contained the error. For
example, if we have:

~~~~{.haskel}
typeError :: XMLGenT (ServerPartT IO) XML
typeError = <foo><% 1 + 'a' %></foo>
~~~~

We will get an error like:

~~~~
TemplatesHSP.markdown.lhs:459:59:
    No instance for (Num Char)
      arising from a use of `+'
    Possible fix: add an instance declaration for (Num Char)
    In the first argument of `asChild', namely `(1 + 'a')'
    In the first argument of `asChild', namely `((asChild (1 + 'a')))'
    In the expression: asChild ((asChild (1 + 'a')))
~~~~

The last line says:

~~~~
    In the expression: asChild ((asChild (1 + 'a')))
~~~~

which is, indeed, where the type error is.

A bug report about the line number issue has been filed, and there are ideas on how to fix it. You can [read more here](http://groups.google.com/group/haskell-server-pages/browse_thread/thread/1b136c7acb448136).

Overlapping Instances
---------------------

Another common error is that of overlapping instances. For example, if
we wrote the following:

~~~~{.haskell}
overlapping = <p>overlapping</p>
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

> overlapping :: (EmbedAsChild m String) => XMLGenT m (XMLType m)
> overlapping = <p>overlapping</p>

In general, there can be a lot of required `EmbedAsChild` and
`EmbedAsAttr` instances. So, often times you can save a lot of typing
by using the `XMLGenerator` class alias:

> overlapping' :: (XMLGenerator m) => XMLGenT m (XMLType m)
> overlapping' = <p>overlapping</p>

Ambiguous Types
---------------

Sometimes a type signature for the parent function is not enough. For example, let's say we have:

~~~~{.haskell}
ambiguous :: (EmbedAsChild m String) => XMLGenT m (XMLType m)
ambiguous = <p><% fromString "ambiguous" %></p>
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

> ambiguous :: (EmbedAsChild m Text) => XMLGenT m (XMLType m)
> ambiguous = <p><% (fromString "ambiguous") :: Text %></p>

