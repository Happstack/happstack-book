
Type-Safe `Form` processing using `reform`
==========================================

`reform` is a library for creating type-safe, composable, and
validated HTML forms. It is built around applicative functors and is
based on the same principles as `formlets` and `digestive-functors <=
0.2`.

The core `reform` library is designed to be portable and can be used
with a wide variety of Haskell web frameworks and template solutions
-- though only a few options are supported at the moment.

The most basic method of creating and processing forms with out the
assistance of `reform` is to:

 1. create a `<form>` tag with the desired elements by hand

 2. write code which processes the form data set and tries to extract a value from it

The developer will encounter a number of difficulties using this method:

 1. the developer must be careful to use the same `name` field in the
 HTML and the code.

 2. if a new field is added to the form, the code must be manually
 updated. Failure to do so will result in the new field being
 silently ignored.

 3. form fragments can not be easily combined because the `name` or
`id` fields might collide. Additionally, there is no simple way to
combine the validation/value extraction code.

 4. if the form fails to validate, it is difficult to redisplay the
 form with the error messages and data that was submitted.

`reform` solves these problems by combining the view generation code
and validation code into a single `Form` element. The `Form` elements
can be safely combined to create more complex forms.

In theory, `reform` could be applied to other domains, such as
command-line or GUI applications. However, `reform` is based around
the pattern of:

 1. generate the entire form at once
 2. wait until the user has filled out all the fields and submitted it
 3. process the results and generate an answer or redisplay the form with validation errors

For most interactive applications, there is no reason to wait until
the entire form has been filled out to perform validation.

Brief History
-------------

`reform` is an extension of the OCaml-based `formlets` concept
originally developed by Ezra Cooper, Sam Lindley, Philip Wadler and
Jeremy Yallop. The original `formlets` code was ported to Haskell as the
`formlets` library, and then revamped again as the
`digestive-functors <= 0.2` library.

`digestive-functors` 0.3 represents a major break from the traditional
`formlets` model. The primary motivation behind `digestive-functors`
0.3 was (mostly likely) to allow the separation of validators from the
view code. This allows library authors to define validation for forms,
while allowing the library users to create the view for the forms. It also
provides a mechanism to support templating systems like `Heist`, where
the view is defined in an external XML file rather than Haskell code.

In order to achieve this, `digestive-functors` 0.3 unlinks the
validation and view code and requires the developers to stitch them
back together using `String` based names. This, of course, leads to
runtime errors. If the library author adds new required fields to the
validator, the user gets no compile time warnings or errors to let
them know their code is broken.

The `Reform` library is a heavily modified fork of
`digestive-functors` 0.2. It builds on the the traditional `formlets`
safety and style and extends it to allow view and validation
separation in a type-safe manner.

You can find the original papers on `formlets` [here](http://groups.inf.ed.ac.uk/links/formlets/).

Hello `Form`!
-------------

You will need to install the following optional packages for this section:


    cabal install reform reform-happstack reform-hsp


The easiest way to learn `Reform` is through example. We will start
with a simple form that does not require any special validation. We
will then extend the form, adding some simple validators. And then we
will show how we can split the validation and view for our form into
separate libraries.

This example uses Happstack for the web server and HSP for the templating library.

First we have some pragmas:


> {-# LANGUAGE FlexibleContexts, FlexibleInstances,
>     MultiParamTypeClasses, ScopedTypeVariables,
>     TypeFamilies, TypeSynonymInstances,
>     QuasiQuotes, OverloadedStrings #-}
> module Main where
>

And then some imports. We import modules from three different `reform`
packages: the core `reform` library, the `reform-happstack` package,
and the `reform-hsp` package:

> import Control.Applicative
> import Control.Applicative.Indexed
>     (IndexedFunctor(..), IndexedApplicative(..))
> import Control.Monad               (msum)
> import Data.Text.Lazy              (Text)
> import qualified Data.Text.Lazy    as Lazy
> import qualified Data.Text         as Strict
> import Happstack.Server
> import Happstack.Server.HSP.HTML   ()
> import HSP
> import HSP.Monad                   (HSPT(..))
> import Language.Haskell.HSX.QQ     (hsx)
> import Text.Reform
>     ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
>     , (<++), commonFormErrorStr, decimal, prove
>     , transformEither, transform )
> import Text.Reform.Happstack
> import Text.Reform.HSP.Text
>

Next we will create a type alias for our application's server monad:

> type AppT m  = XMLGenT (HSPT XML (ServerPartT m))
> type AppT' m = HSPT XML (ServerPartT m)

We will also want a function that generates a page template for our app:

> appTemplate :: ( Functor m, Monad m
>                , EmbedAsChild (AppT' m) headers
>                , EmbedAsChild (AppT' m) body
>                ) =>
>                Text    -- ^ contents of <title> tag
>             -> headers -- ^ extra content for <head> tag.
>                        --   use () for nothing
>             -> body    -- ^ contents of <body> tag
>             -> AppT m Response
> appTemplate title headers body =
>   toResponse <$> [hsx|
>     <html>
>      <head>
>       <title><% title %></title>
>       <% headers %>
>      </head>
>      <body>
>       <% body %>
>      </body>
>     </html>
>     |]
>



Forms have the type `Form` which looks like:


~~~~{.haskell}
newtype Form m input error view proof a = Form { ... }
~~~~


As you will note it is heavily parameterized:


`m`
:    a monad which can be used to validate the result
`input`
:    the framework specific type containing the fields from the form data set.
`error`
:    An application specific type for form validation errors.
`view`
:    The type of the view for the form.
`proof`
:    A datatype which names something that has been proven about the result.
`a`
:    The value returned when the form data set is successfully decoded and validated.

In order to keep our type signatures sane, it is convenient to create an application specific type alias for the `Form` type:

> type SimpleForm =
>  Form (AppT IO) [Input] AppError [AppT IO (XMLType (ServerPartT IO))] ()
>

`AppError` is an application specific type used to report form validation errors:

> data AppError
>     = Required
>     | NotANatural String
>     | AppCFE (CommonFormError [Input])
>       deriving Show
>


Instead of having one error type for all the forms, we could have per-form error types -- or even just use `String`. The advantage of using a type is that it makes it easier to provide I18N translations, or for users of a library to customize the text of the error messages. The disadvantage of using a custom type over a plain `String` is that it can make it more difficult to combine forms into larger forms since they must all have the same error type. Additionally, it is a bit more work to create the error type and the `FormError` instance.

We will want an `EmbedAsChild` instance so that we can easily embed the errors in our HTML:

> instance (Functor m, Monad m) =>
>     EmbedAsChild (AppT' m) AppError where
>   asChild Required          =
>     asChild $ "required"
>
>   asChild (NotANatural str) =
>     asChild $ "Could not decode as a positive integer: " ++ str
>
>   asChild (AppCFE cfe)      =
>      asChild $ commonFormErrorStr show cfe
>

> instance (Functor m, Monad m) =>
>          EmbedAsChild (AppT' m) Strict.Text where
>     asChild t = asChild (Lazy.fromStrict t)
>
> instance (Functor m, Monad m) =>
>          EmbedAsAttr (AppT' m) (Attr Text Strict.Text) where
>     asAttr (n := v) = asAttr (n := Lazy.fromStrict v)


The error type also needs a `FormError` instance:

> instance FormError AppError where
>     type ErrorInputType AppError = [Input]
>     commonFormError = AppCFE
>

Internally, `reform` has an error type `CommonFormError` which is used
to report things like missing fields and other internal errors. The
`FormError` class is used to lift those errors into our custom error
type.

Now we have the groundwork laid to create a simple form. Let's create
a form that allows users to post a message. First we will want a type to
represent the message -- a simple record will do:


> data Message = Message
>     { name    :: Strict.Text -- ^ the author's name
>     , title   :: Strict.Text -- ^ the message title
>     , message :: Strict.Text -- ^ contents of the message
>     } deriving (Eq, Ord, Read, Show)
>

and a simple function to render the `Message` as `XML`:

> renderMessage :: ( Functor m
>                  , Monad m
>                  , EmbedAsChild (AppT' m) Strict.Text) =>
>                  Message -> AppT m XML
> renderMessage msg =
>    [hsx|
>     <dl>
>       <dt>name:</dt>    <dd><% name msg    %></dd>
>       <dt>title:</dt>   <dd><% title msg   %></dd>
>       <dt>message:</dt> <dd><% message msg %></dd>
>     </dl>
>    |]
>

Now we can create a very basic form:

> postForm :: SimpleForm Message
> postForm =
>   Message
>    <$> labelText "name:"             ++> inputText ""      <++ br
>    <*> labelText "title: "           ++> inputText ""      <++ br
>    <*> (labelText "message:" <++ br) ++> textarea 80 40 "" <++ br
>    <*  inputSubmit "post"
>

This form contains all the information needed to generate the form elements and to parse the submitted form data set and extract a `Message` value.

The following functions come from `reform-hsp`. `reform-blaze` provides similar functions.

 * `label` function creates a `<label>` element using the supplied label.

 * `inputText` function creates a `<input type="text">` input element using the argument as the initial value.

 * `inputSubmit` function creates a `<input type="submit">` using the argument as the value.

 * `textarea` function creates `<textearea>`. The arguments are the number of cols, rows, and initial contents.

 * `br` functions creates a `Form` element that doesn't do anything except insert a `<br>` tag.

The `<$>`, `<*>` and `<*` operators come from `Control.Applicative`. If you are not familiar with applicative functors then you will want to read a [tutorial such as this one](http://en.wikibooks.org/wiki/Haskell/Applicative_Functors).

`++>` comes from the `reform` library and has the type:

~~~~{.haskell}
(++>) :: (Monad m, Monoid view) =>
         Form m input error view () ()
      -> Form m input error view proof a
      -> Form m input error view proof a
~~~~



The `++>` operator is similar to the `*>` operator with one important difference. If we were to write:


~~~~{.haskell}
label "name: " *> inputText
~~~~


then the `label` and `inputText` would each have unique `FormId` values. But when we write:


~~~~{.haskell}
label "name: " ++> inputText
~~~~

they have the same `FormId` value. The `FormId` value is typically used to create unique `name` and `id` attributes for the form elements. But, in the case of `label`, we want the `for` attribute to refer to the `id` of the element it is labeling. There is also a similar operator <++ for when you want the label after the element.

We also use `<++` and `++>` to attach error messages to form elements.

Using the `Form`
----------------

The easiest way to use `Form` is with the `happstackEitherForm` function:

> postPage :: AppT IO Response
> postPage =
>   dir "post" $ do
>    let action = "/post" :: Text
>    result <- happstackEitherForm (form action) "post" postForm
>    case result of
>      (Left formHtml) ->
>          appTemplate "post" () formHtml
>      (Right msg)     ->
>          appTemplate "Your Message" () $ renderMessage msg
>

`happstackEitherForm` has the type:

~~~~{.haskell}
happstackEitherForm :: (Happstack m) =>
     ([(Text, Text)] -> view -> view)  -- ^ wrap raw form html
                                       --   inside a <form> tag
  -> Text                              -- ^ form prefix
  -> Form m [Input] error view proof a -- ^ Form to run
  -> m (Either view a)                 -- ^ Result
~~~~


For a `GET` request, `happstackEitherForm` will view the form with `NoEnvironment`. It will always return `Left view`.

For a `POST` request, `happstackEitherForm` will attempt to validate the form using the form submission data. If successful, it will return `Right a`. If unsuccessful, it will return `Left view`. In this case, the view will include the previously submitted data plus any error messages.

Note that since `happstackEitherForm` is intended to handle both `GET` and `POST` requests, it is important that you do not have any `method` calls guarding `happstackEitherForm` that would interfere.

The first argument to `happstackEitherForm` is a function what wraps the view inside a `<form>` element. This function will typically be provided by template specific reform package. For example, `reform-hsp` exports:


~~~~{.haskell}
-- | create <form action=action
--                method="POST"
--                enctype="multipart/form-data">
form :: (XMLGenerator x, EmbedAsAttr x (Attr Text action)) =>
        action                  -- ^ action url
     -> [(Text,Text)]           -- ^ extra hidden fields
     -> [XMLGenT x (XMLType x)] -- ^ children
     -> [XMLGenT x (XMLType x)]
~~~~~


The first argument to `form` is the attribute to use for the `action` attribute. The other arguments will be filled out by `happstackEitherForm`.

The second argument to `happstackEitherForm` is a unique `String`. This is used to ensure that each `<form>` on a page generates unique `FormId` values. This is required since the `FormId` is typically used to generate `id` attributes, which must be unique.

The third argument to `happstackEitherForm` is the the form we want to use.

`reform` function
-----------------

`happstackEitherForm` is fairly straight-forward, but can be a bit tedious at times:

 1. having to do `case result of` is a bit tedious.
 2. when using `HSP`, it is a bit annoying that the `happstackEitherForm` appears outside of the rest of the page template

These problems are even more annoying when a page contains multiple forms.

`reform-happstack` exports `reform` which can be used to embed a `Form` directly inside an `HSP` template:


> postPage2 :: AppT IO Response
> postPage2 =
>  dir "post2" $
>   let action = ("/post2" :: Text) in
>   appTemplate "post 2" () $[hsx|
>    <% reform (form action) "post2" displayMsg Nothing postForm %>
>   |]
>  where
>   displayMsg msg =
>     appTemplate "Your Message" () $ renderMessage msg
>



`reform` has a pretty intense looking type signature but it is actually pretty straight-forward, and similar to `eitherHappstackForm`:


~~~~{.haskell}
reform :: ( ToMessage b
          , Happstack m
          , Alternative m
          , Monoid view) =>
    ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside
                                     -- a @\<form\>@ tag
 -> Text                             -- ^ prefix
 -> (a -> m b)                       -- ^ success handler used when
                                     --   form validates
 -> Maybe ([(FormRange, error)] -> view -> m b) -- ^ failure handler
                                                --   used when form
                                                --   does not validate
 -> Form m [Input] error view proof a           -- ^ the formlet
 -> m view
reform toForm prefix success failure form = ...
~~~~



`toForm`
:    should wrap the view returned by the form in a `&lt;form&gt;` tag. Here we use the `form` function from `reform-happstack`. The first argument to `form` is the `action` url.
`prefix`
:    the `FormId` prefix to use when rendering this form.
`handleSuccess`
:     is the function to call if the form validates successfully. It gets the value extracted from the form.
`hHandleFailure`
:    is a function to call if for validation fails. If you pass in `Nothing` then the form will simple by redisplayed in the original context.
`form`
:    is the `Form` to process.

Cross-Site Request Forgery (CSRF) Protection
--------------------------------------------

The `happstackEitherForm` and `reform` functions also have a hidden benefit -- they provide cross-site request forgery (CSRF) protection, using the double-submit method. When the `<form>` is generated, the `reform` or `happstackEitherForm` function will create a secret token and add it to a hidden field in the form. It will also put the secret token in a cookie. When the user submits the form, the `reform` function will check that the value in the cookie and the hidden field match. This prevents rogue sites from tricking users into submitting forms, because the rogue site can not get access to the secret token in the user's cookie.

That said, if your site is vulnerable to cross site script (XSS) attacks, then it may be possible for a remote site to steal the cookie value.

Benefits So Far
---------------

The form we have so far is very simple. It accepts any input, not caring if the fields are empty or not. It also does not try to convert the `String` values to another type before adding them to the record.

However, we do still see a number of benefits. We specified the form once, and from that we automatically extract the code to generate HTML and the code to extract the values from the form data set. This adheres to the DRY (don't repeat yourself) principle. We did not have to explicitly name our fields, keep the names in-sync in two different places, worry if the HTML and processing code contain the same set of fields, or worry if a name/id has already been used. Additionally, we get automatic CSRF protection.

`Form` with Simple Validation
-----------------------------

The next step is to perform some validation on the input fields. If the fields validate successfully, then we get a `Message`. But if the input fails to validate, then we will automatically regenerate the `Form` showing the data the user submitted plus validation errors.

For this example, let's simply make sure they entered something in all the fields. To do that we will create a simple validation function:


> required :: Strict.Text -> Either AppError Strict.Text
> required txt
>     | Strict.null txt = Left Required
>     | otherwise       = Right txt
>


In this case we are simply checking that the `String` is not null. If it is null we return an error, otherwise we return the `String` unmodified. Some validators will actually transform the value -- such as converting the `String` to an `Integer`.

To apply this validation function we can use `transformEither`:


~~~~{.haskell}
transformEither :: Monad m =>
                   Form m input error view anyProof a
                -> (a -> Either error b)
                -> Form m input error view () b
~~~~

We can update our `Form` to:

> validPostForm :: SimpleForm Message
> validPostForm =
>     Message <$> name <*> title <*> msg <*  inputSubmit "post"
>  where
>    name  = errorList ++> labelText "name:"             ++>
>             (inputText ""     `transformEither` required)  <++ br
>
>    title = errorList ++> labelText "title:"            ++>
>             (inputText ""      `transformEither` required) <++ br
>
>    msg   = errorList ++> (labelText "message:" <++ br) ++>
>             (textarea 80 40 "" `transformEither` required) <++ br
>



The `errorList` will add a list of error messages to a `Form`
element. This gives greater control over where error messages appear
in the form. The list of errors is literally a list of errors inside
a `<ul>` tag:

    <ul class="reform-error-list">
      <li>error 1</li>
      <li>error 2</li>
      <li>error n</li>
    </ul>

You can use CSS to control the theming.

For even greater control we could use the `Text.Reform.Generalized.errors` function:


~~~~{.haskell}
errors :: Monad m =>
          ([error] -> view) -- ^ convert the error messages into a view
       -> Form m input error view () ()
~~~~


This allows you to provide your own custom view code for rendering the errors.

We can wrap up the `validForm` the exact same way we did `postForm`:


> validPage :: AppT IO Response
> validPage =
>  dir "valid" $
>   let action = "/valid" :: Text in
>   appTemplate "valid post" () $ [hsx|
>    <% reform (form action) "valid" displayMsg Nothing validPostForm %>
>   |]
>  where
>   displayMsg msg =
>     appTemplate "Your Message" () $ renderMessage msg
>

A few names have been changed, but everything else is exactly the same.

Separating Validation and Views
-------------------------------

One of the primary motivations behind the changes in
`digestive-functors 0.3` is allowing developers to separate the
validation code from the code which generates the view. We can do this
using `reform` as well -- in a manner that is both more flexible and
which provides greater type safety. The key is the `proof` parameter
-- which we have so far set to `()` and otherwise ignored.

In `reform` we divide the work into two pieces:

 1. `Proofs`
 2. a `Form` that returns a `Proved` value

This allows the library authors to create `Proofs` and demand that a `Form` created by another developer satisfies the `Proof`. At the same time, it gives the developer unrestricted control over the layout of the `Form` -- including choice of templating library.

Let's create a new type alias for `Form` that allows us to actually set the `proof` parameter:

> type ProofForm proof =
>   Form IO [Input] AppError [AppT IO (XMLType (ServerPartT IO))] proof
>

First we will explore the `Proof` related code that would go into a library.

The `proof` parameter for a `Form` is used to indicate that something has been proven about the form's return value.

Two create a `proof` we need two things:

 1. a type which names the proof
 2. a function which performs the proof

We wrap those two pieces up into a `Proof`:


~~~~{.haskell}
data Proof m error proof a b = Proof
    { proofName     :: proof                   -- ^ name of the
                                               --   thing to prove
    , proofFunction :: a -> m (Either error b) -- ^ function which
                                               --   provides the proof
    }
~~~~

In `validPostForm`, we checked that the input fields were not empty
`Strings`. We could turn that check into a proof by first creating a
type to name that proof:


> data NotNull = NotNull
>

and then creating a proof function like this:

> assertNotNull :: (Monad m) =>
>                  error
>               -> Strict.Text
>               -> m (Either error Strict.Text)
> assertNotNull errorMsg txt
>     | Strict.null txt = return (Left errorMsg)
>     | otherwise       = return (Right txt)
>

We can then wrap the two pieces up into a proof:

> notNullProof :: (Monad m) =>
>                 error -- ^ error to return if list is empty
>              -> Proof m error NotNull Strict.Text Strict.Text
> notNullProof errorMsg =
>     Proof { proofName     = NotNull
>           , proofFunction = assertNotNull errorMsg
>           }
>

We can also create proofs that combine existing proofs. For example, a `Message` is only valid if all its fields are not null. So, first thing we want to do is create a proof name for valid messages:


> data ValidMessage = ValidMessage
>


The `Message` constructor has the type:

~~~~{.haskell}
Message :: String -> String -> String -> Message
~~~~


For `SimpleForm` we would use `pure` to turn `Message` into a `SimpleForm`:


~~~~{.haskell}
mkSimpleMessage :: SimpleForm (String -> String -> String -> Message)
mkSimpleMessage = pure Message
~~~~

For `ProofForm`, we can do the same thing use `ipure`:

> mkMessage :: ProofForm (NotNull -> NotNull -> NotNull -> ValidMessage)
>                        (Strict.Text -> Strict.Text -> Strict.Text -> Message)
> mkMessage = ipure (\NotNull NotNull NotNull -> ValidMessage) Message
>

This creates a chain of validation since `mkMessage` can only be applied to `String` values that have been proven `NotNull`.

The library author can now specify that the user supplied `Form` has the type:

~~~~{.haskell}
someFunc :: ProofForm ValidMessage Message -> ...
~~~~

You will notice that what we have constructed so far has imposes no restrictions on what types of form elements can be used, what template library must be used, or what web server must be used. At the same time, in order for the library user to create a `ProofForm` with the required type, they must apply the supplied validators. Now, clearly a devious library user could use evil tricks to circumvent the system -- and they will get what they deserve.

To construct the `Form`, we use a pattern very similar to what we did when using `SimpleForm`. They only real differences are:

 1. we use `prove` instead of `transformEither`
 2. we use `<<*>>` instead of `<*>`

To apply a `Proof` we use the `prove` function:

~~~~{.haskell}
prove :: (Monad m) =>
         Form m input error view q a
      -> Proof m error proof a b
      -> Form m input error view proof b
~~~~

So, we can make a `ProofForms` for non-empty `Strings` like this:

> inputText' :: Strict.Text -> ProofForm NotNull Strict.Text
> inputText' initialValue =
>     inputText initialValue `prove` (notNullProof Required)
>

> textarea' :: Int         -- ^ cols
>           -> Int         -- ^ rows
>           -> Strict.Text -- ^ initial value
>           -> ProofForm NotNull Strict.Text
> textarea' cols rows initialValue =
>     textarea cols rows initialValue `prove` (notNullProof Required)
>

to create the `ValidMessage` form we can then combine the pieces like:

> provenPostForm :: ProofForm ValidMessage Message
> provenPostForm =
>     mkMessage
>       <<*>> errorList ++> labelText "name: "    ++> inputText' ""
>       <<*>> errorList ++> labelText "title: "   ++> inputText' ""
>       <<*>> errorList ++> labelText "message: " ++> textarea' 80 40 ""
>

This code looks quite similar to our `validPostForm` code. The primary
difference is that we use `<<*>>` instead of `<*>`. That brings is to the topic of type-indexed applicative functors.

Type Indexed / Parameterized Applicative Functors
-------------------------------------------------

Lets look at the type for `Form` again:

~~~~{.haskell}
newtype Form m input error view proof a = Form { ... }
~~~~


In order to make an `Applicative` instance of `Form`, all the proof type variables must be the same type and must form a `Monoid`:

~~~~{.haskell}
instance (Functor m, Monad m, Monoid view, Monoid proof) =>
             (Form m input error view proof)
~~~~

for `SimpleForm` we used the following instance, which is defined for us already in `reform`:

~~~~{.haskell}
instance (Functor m, Monoid view, Monad m) =>
             Applicative (Form m input error view ())
~~~~


With this instance, `reform` feels and works almost exactly like `digestive-functors <= 0.2`.

But, for the `provenPostForm`, that `Applicative` instance won't work for us. `mkMessage` has the type:

~~~~{.haskell}
mkMessage :: ProofForm (NotNull -> NotNull -> NotNull -> ValidMessage)
                       (String -> String -> String -> Message)
~~~~

and we want to apply it to `ProofForms` created by:

~~~~{.haskell}
inputText' :: String -> ProofForm NotNull String
~~~~


Here the proof types don't match up. Instead we need a `Applicative
Functor` that allows us to transform the return value *and* the proof
value. We need, what I believe is called, a `Type-Indexed Applicative
Functor` or a `Parameterized Applicative Functor`. Most literature on
this subject is actually dealing with type-indexed or parameterized
`Monads`, but the idea is the same.

The `reform` library defines two new classes, `IndexedFunctor` and `IndexedApplicative`:

~~~~{.haskell}
class IndexedFunctor f where
    -- | imap is similar to fmap
    imap :: (x -> y) -- ^ function to apply to first parameter
         -> (a -> b) -- ^ function to apply to second parameter
         -> f x a    -- ^ indexed functor
         -> f y b

class (IndexedFunctor f) => IndexedApplicative f where
    -- | similar to 'pure'
    ipure   :: x -> a -> f x a
    -- | similar to '<*>'
   (<<*>>) :: f (x -> y) (a -> b) -> f x a -> f y b
~~~~

These classes look just like their non-indexed counterparts, except that they transform an extra parameter. Now we can create instances like:

~~~~{.haskell}
instance (Monad m)              =>
   IndexedFunctor     (Form m input view error) where

instance (Monad m, Monoid view) =>
   IndexedApplicative (Form m input error view) where
~~~~



We use these classes the same way we would use the normal `Functor` and `Applicative` classes. The only difference is that the type-checker can now enforce the proofs.

Using `Proofs` in unproven `Forms`
----------------------------------

The `Proof` module provides a handful of useful `Proofs` that perform
transformations, such as converting a `String` to a `Int`:


~~~~{.haskell}
decimal :: (Monad m, Eq i, Num i) =>
           (Text -> error) -- ^ create an error message
                           --   ('Text' is the value
                           --   that did not parse)
        -> Proof m error Decimal String i
~~~~

We can use this `Proof` with our `SimpleForm` by using the `transform` function:

~~~~{.haskell}
transform :: (Monad m) =>
             Form m input error view anyProof a
          -> Proof m error proof a b
          -> Form m input error view () b
~~~~


`transform` is similar to the `prove` function, except it ignores the proof name and sets the proof to `()`. Technically `()` is still a proof -- but we consider it to be the proof that proves nothing.

Here is an example of using `transform` with `decimal` to create a
simple form that parses a positive `Integer` value:


~~~~{.haskell}
inputInteger :: SimpleForm Integer
inputInteger = inputText "" `transform` (decimal NotANatural)
~~~~

Conclusion
----------

And, that is the essence of `reform`. The Haddock documentation should cover the remainder -- such as other types of input controls (radio buttons, checkboxes, etc).

main
----

Here is a main function that ties all the examples together:

> main :: IO ()
> main = simpleHTTP nullConf $ unHSPT $ unXMLGenT $ do
>  decodeBody (defaultBodyPolicy "/tmp/" 0 10000 10000)
>  msum [ postPage
>       , postPage2
>       , validPage
>       , do nullDir
>            appTemplate "forms" () $ [hsx|
>             <ul>
>              <li><a href="/post">Simple Form</a></li>
>              <li>
>                <a href="/post2">
>                  Simple Form (postPage2 implementation)
>                </a>
>              </li>
>              <li><a href="/valid">Valid Form</a></li>
>             </ul> |]
>       ]
>



There is nothing `reform` specific about it.

Source code for the app is [here](http://srclink/Reform.hs).

