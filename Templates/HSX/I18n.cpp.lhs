
HSP and internationalization (aka, i18n)
========================================

You will need to install `happstack-hsp` and `shakespeare-i18n` for
this section.

Internationalization (abbreviated to the numeronym i18n) and
localization (L10n) generally refer to the processing of making an
application usuable by people that speak different
languages, use different alphabets and keyboards, and have different
conventions for things like formatting times and dates, currency, etc.

Proper handling of these issues can run deep into your code. For
example, English speakers often think of people as having a first name
and a last name -- but when you look at how people's names are used
around the world, you realize these familiar terms are not universally
applicable. So, a type like:

~~~~{.haskell}
data Name = Name { firstName :: Text, lastNime :: Text }
~~~~

may not be sufficient.

The haskell wiki lists [a bunch of methods](http://www.haskell.org/haskellwiki/I18N)
for translating strings into multiple languages.

In this example, we show how we can use native haskell types datas, a
translator friendly file format, and HSP to do some simple
internationalization. We will build on top of the [`shakespeare-i18n`](http://hackage.haskell.org/package/shakespeare-i18n) library.

As usual, we start off with a bunch of imports and pragmas:


> {-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell,
>     MultiParamTypeClasses, OverloadedStrings, QuasiQuotes,
>     TypeFamilies #-}
> module Main where
>
> import Control.Applicative   ((<$>))
> import Control.Monad         (msum)
> import Control.Monad.Reader  (ReaderT, ask, runReaderT)
> import Control.Monad.Trans   (MonadIO(liftIO))
> import Data.Map              (Map, fromList)
> import qualified Data.Map    as Map
> import Data.Monoid           ((<>))
> import qualified Data.Text   as Strict
> import qualified Data.Text.Lazy as Lazy
> import Happstack.Server      ( ServerPart, ServerPartT, dir
>                              , lookTexts', mapServerPartT
>                              , nullConf, nullDir, queryString
>                              , simpleHTTP , acceptLanguage
>                              , bestLanguage
>                              )
> import Happstack.Server.HSP.HTML
> import Happstack.Server.XMLGenT
> import HSP
> import HSP.Monad             (HSPT(..))
> import Language.Haskell.HSX.QQ (hsx)
> import Text.Shakespeare.I18N ( RenderMessage(..), Lang, mkMessage
>                              , mkMessageFor, mkMessageVariant)
> import System.Random (randomRIO)
>


HSP + i18n Core Concept
-----------------------

Instead of using strings directly in our templates we could create a
data type where each constructor represents a phrase, sentence, or
paragraph that we want to put on the page. For example, we could
define the type:

> data Message = Hello | Goodbye

Then we could provide a translation function for each language we support:

> translation_en :: Message -> Strict.Text
> translation_en Hello       = "hello"
> translation_en Goodbye     = "goodbye"
>
> translation_lojban :: Message -> Strict.Text
> translation_lojban Hello   = "coi"
> translation_lojban Goodbye = "co'o"
>
> translations :: Map Strict.Text (Message -> Strict.Text)
> translations =
>     fromList [ ("en"    , translation_en)
>              , ("lojban", translation_lojban)
>              ]
>
> translate :: Strict.Text -> Message -> Strict.Text
> translate lang msg =
>     case Map.lookup lang translations of
>       Nothing           -> "missing translation"
>       (Just translator) ->
>           translator msg
>

and then in our templates we can write:

> helloPage :: ( XMLGenerator m
>               , EmbedAsChild m Strict.Text
>               , StringType m ~ Lazy.Text
>               ) =>
>              Strict.Text -> XMLGenT m (XMLType m)
> helloPage lang = [hsx|
>     <html>
>      <head>
>       <title><% translate lang Hello %></title>
>      </head>
>      <body>
>       <p><% translate lang Hello %></p>
>      </body>
>     </html>
>     |]

The principle behind this approach is nice, but in practice, it has a few problems:

 1. having to write the translation functions in the Haskell source is
    not a very friendly format for the people who will be doing the
    translations.

 2. having to call 'translate' explicitly is boring, tedious, and error prone

 3. having to pass around the desired 'lang' manually is also boring, tedious, and error prone

Fortunately, we can work around all these issues quite simply.

the `RenderMessage` class
-------------------------

`shakespeare-i18n` provides a simple class for providing translations:

~~~~{.haskell}
type Lang = Text

class RenderMessage master message where
  renderMessage :: master    -- ^ translation variant
                -> [Lang]    -- ^ desired languages in descending
                             --   order of preference
                -> message   -- ^ message we want translated
                -> Text      -- ^ best matching translation
~~~~

`renderMessage` is pretty straight-forward. It takes a list of
preferred languages and a message datatype (such as `Message` type we
defined above) and returns the best matching translation. The only
mysterious part is the `master` argument. (Personally, I think
`variant` might be a better name for the argument). The argument
exists so that you can provide more than one set of translations for
the same message type.

For example, let's say that we had defined the `Message` type in a
library. Being the nice people we are, we also provide a set of
translations for the `Message` type. However, someone using our
library may want to provide a completely different set of translations
that are more appropriate to their application. For example, in the
library we might have:

~~~~{.haskell}
data LibraryI18N = LibraryI18N

instance RenderMessage LibraryI18N Message where
    renderMessage = ...
~~~~

But the user could provide their own translations for `Message` via:

~~~~{.haskell}
data AppI18N = AppI18N

instance RenderMessage AppI18N Message where
    renderMessage = ...
~~~~

`shakespeare-i18n` translation files
------------------------------------

Writing the translations in your Haskell source can be pretty
inconvenient. Especially if you are working with a team of outsourced
translators. Fortunately, `shakespeare-i18n` has support for external
translation files.

To keep things simple:

 1. each language will have its own translation file

 2. the file will be named _lang_`.msg` where `lang` is a language code
    such as `en`, `en-GB`, `fr`, etc

 3. the translation files will all be in a subdirectory which contains
    nothing but translations

 4. the `.msg` files must be UTF-8 encoded

So for this example we will have three files:

~~~~
messages/standard/en.msg
messages/standard/en-GB.msg
messages/standard/jbo.msg
~~~~

 - `en.msg` is a set of generic English translations.
 - `en-GB.msg` is a set of English translations using spellings and idioms common to Great Britain
 - `jbo.msg` is a set of Lojban translations

The contents of the files are:

`messages/standard/en.msg`

~~~~ {.extra-wide}
#include "messages/standard/en.msg"
~~~~

`messages/standard/en-GB.msg`

~~~~ {.wide}
#include "messages/standard/en-GB.msg"
~~~~

`messages/standard/jbo.msg`

~~~~
#include "messages/standard/jbo.msg"
~~~~

The format is very simple. Each line looks like:

    Constructor arg0 arg1 .. argn: translation text

 1. `Constructor` is a valid Haskell constructor name that we will use to reference this translation
 2. it is followed by 0 or more variable names
 3. then there is a `:`
 4. and then there is the translation

You may also notice that in `en.msg` the arguments contain types like `n@Int`. And some of translations contain markup like `#{show n}`. You can probably guess what those things mean -- we will come back to them shortly.

You may also notice that the Lojban translation is missing the `Problems` constructor. Since there is no translation provided, `renderMessage` will use the default translation (which, in this case will come from `en.msg`).


Due to TH staging restrictions this code must come before the `mkMessage` call below. But we are not ready to talk about it yet in the tutorial. So ignore it until later.

> plural_en :: (Integral i) => i -> String -> String -> String
> plural_en 1 x _ = x
> plural_en _ _ y = y
>
> data Thing = TypeError | SegFault deriving (Enum, Bounded, Show)
>
> mkMessageFor "DemoApp" "Thing" "messages/thing" ("en")
>
> thing_tr :: Lang -> Thing -> Strict.Text
> thing_tr lang thing = renderMessage DemoApp [lang] thing
>

To load the message files we first need to define our `master` type:

> data DemoApp = DemoApp
>

Then we just call `mkMessage`:

> mkMessage  "DemoApp" "messages/standard" ("en")
>

`mkMessage` is a Template Haskell function which:

 1. reads the `.msg` files
 2. creates a new datatype based on the constructors it found
 3. creates a `RenderMessage` instance

`mkMessage` has the following type:

~~~~{.haskell}
mkMessage :: String    -- ^ name of master translation type
          -> FilePath  -- ^ path to folder which contains the `.msg` files
          -> Lang      -- ^ default language
          -> Q [Dec]
~~~~

If we use `-ddump-splices` we see that the `mkMessages` call above generated the following for us:

~~~~{.haskell}
data DemoAppMessage
    = MsgHello
    | MsgGoodbye
    | MsgProblems { translationsMessageN     :: Int
                  , translationsMessageThing :: Thing
                 }


instance RenderMessage DemoApp DemoAppMessage where
    renderMessage = ...
~~~~

It has created a new type for us `DemoAppMessage` where each
constructor is derived from the constructors found in the `en.msg`
file. The constructor names all have the prefix `Msg`. That is just to
avoid name collisions with the other constructors in your application.

It has also created a `RenderMessage` instance with all the
translations (not shown for the sake of readability).

Now we can do:

~~~~
*Main> renderMessage DemoApp ["en"] MsgHello
"greetings"
~~~~

Note that because the message files are read in using Template Haskell
at compile time, we do not need to install them on the live
server. Also, if you change the `.msg` files, you will not see the
changes until you recompile.

Constructor arguments, `#{ }`, and plurals
------------------------------------------

The `Problems` constructor in the `en.msg` file appears considerably
more complicate than the `Hello` and `Goodbye` cases:

~~~~
Problems n@Int thing@Thing: Got #{show n} #{plural_en n "problem" "problems" } but a #{thing_tr "en" thing} ain't #{plural_en n "it" "one"}.
~~~~

There are a few things going on here.

Type Annotations
----------------

The `Problems` constructor takes two arguments: `n` and `thing`. In
order to create the `MsgProblems` constructor, `mkMessage` needs to
know the types of those arguments. So, we add the type annotations
using the `@` syntax. We only need the type annotations in the default
translation file. The default translation file is specified as the
third argument to `mkMessage` -- which in this example is `"en"`.

The types of the arguments can be any valid Haskell type. In this case
'Int' and 'Thing'. 'Thing' is just a normal Haskell datatype which we
will define right now as:

~~~~{.haskell}
data Thing = TypeError | SegFault deriving (Enum, Bounded, Show)
~~~~

Variable Splices
----------------

The `#{ }` syntax allows you to call a Haskell function and splice the result into the message. For example:

~~~~{.haskell}
 #{show n}
~~~~

will convert `n` to a `String` and splice the `String` into the message. The expression inside the `#{ }` must be a pure expression and it must have a type that is an instance of the `ToMessage` class:

~~~~{.haskell}
class ToMessage a where
  toMessage :: a -> Text
~~~~

By default, only `String` and `Text` have `ToMessage` instances.

Remember that `mkMessage` generates code which gets spliced into the
current module. That means the code inside `#{ }` has access to any
functions and types which are available in the module that calls
`mkMessage`.

Handling plurals and other language specifics
---------------------------------------------

In English, we say:

 * I have 1 problem
 * I have 0 problems
 * I have 10 problems

In our translations, we don't want to say *I have 1 problem(s).* We can handle this pluralization issue by creating a simple helper function such as this one:

~~~~{.haskell}
plural_en :: (Integral i) => i -> String -> String -> String
plural_en 1 x _ = x
plural_en _ _ y = y
~~~~

Looking at `en.msg` you notice that we need to use `plural_en` twice
to make the grammar sound natural. When creating messages is good to
use whole phrases and sentences because changes in one part of a
sentence can affect other parts of the sentence. Rules about plurals,
word order, gender agreement, etc, vary widely from one language to
the next. So it is best to assume as little as possible and give the
translators as much flexibility as possible.

Translating Existing Types
--------------------------

`mkMessage` creates a new type from the constructors it finds in the
`.msg` files. But sometimes we want to create a translation for an
existing type. For example, we need to translate the `Thing` type. We
can do that by creating a function like:

~~~~{.haskell}
thing_tr :: Lang -> Thing -> Text
~~~~

Which we can call in the translation file like:

~~~~{.haskell}
 #{thing_tr "en" thing}
~~~~

But, how do we implement `thing_tr`?  One option is to simply write a function like:

~~~~{.haskell}
thing_tr :: Lang -> Thing -> Text
thing_tr lang TypeError | lang == "en" = "type error"
thing_tr lang SegFault  | lang == "en" = "segmentation fault"
thing_tr _    thing     = thing_tr "en" thing
~~~~

But, now someone has to update the Haskell code to add new translations. It would be nice if all the translations came from `.msg` files.

The `mkMessageFor` function allows us to create translations for an existing type:

~~~~{.haskell}
mkMessageFor ::
     String    -- ^ master type
  -> String    -- ^ data to translate
  -> FilePath  -- ^ path to `.msg` files
  -> Lang      -- ^ default language
  -> Q [Dec]
~~~~

We can create a set of `.msg` files for the `Thing` type like this (note the file path):

`messages/thing/en.msg`
\#include "messages/thing/en.msg"

And then use `mkMessageFor` to create a `RenderMessage` instance:

~~~~{.haskell}
mkMessageFor "DemoApp" "Thing" "messages/thing" "en"
~~~~

That will create this instance for us:

~~~~{.haskell}
-- autogenerated by `mkMessageFor`
instance RenderMessage DemoApp Thing where
    renderMessage = ...
~~~~

Because `mkMessageFor` is creating a `RenderMessage` for an existing
type, it does not need to append `Message` to the type name or prefix
the constructors with `Msg`. Now we can define our `thing_tr` function
like this:

~~~~{.haskell}
thing_tr :: Lang -> Thing -> Text
thing_tr lang thing = renderMessage DemoApp [lang] thing
~~~~

This is definitely a bit roundabout, but it is the best solution I can
see using the existing `shakespeare-i18n` implementation.

Alternative Translations
------------------------

We can use `mkMessageVariant` to create an alternative set of
translations for a type that was created by `mkMessage`. For example:

~~~~{.haskell}
data DemoAppAlt = DemoAppAlt

mkMessageVariant "DemoAppAlt" "DemoApp" "messages/alt" "en"
~~~~

Using messages in `HSX` templates
---------------------------------

To use the `DemoAppMessage` type in an `HSX` template, all we need is
an `EmbedAsChild` instance.

The instance will need to know what the client's preferred languages
are. We can provide that by putting the users language preferences in
a `ReaderT` monad:

> type I18N  = HSPT XML (ServerPartT (ReaderT [Lang] IO))
>

Next we create the `EmbedAsChild` instance:

> instance EmbedAsChild I18N DemoAppMessage where
>     asChild msg =
>         do lang <- ask
>            asChild $ Lazy.fromStrict $ renderMessage DemoApp lang msg
>

Now we can use the message constructors inside our templates:

> pageTemplate :: (EmbedAsChild I18N body) =>
>                 Lazy.Text -> body -> I18N XML
> pageTemplate title body =
>     defaultTemplate title () [hsx|
>      <div>
>       <% body %>
>       <ul>
>        <% mapM (\lang ->
>            <li>
>             <a ["href" := ("?_LANG="<> lang) :: Attr Lazy.Text Lazy.Text]>
>              <% lang %>
>             </a>
>            </li>)
>            (["en", "en-GB", "jbo"]) %>
>       </ul>
>      </div> |]
>
> homePage :: I18N XML
> homePage =
>    pageTemplate "home"
>        [hsx| <p><% MsgHello %></p> |]
>
> goodbyePage :: I18N XML
> goodbyePage =
>     pageTemplate "goodbye"
>         [hsx| <p><% MsgGoodbye %></p> |]
>
> problemsPage :: Int -> Thing -> I18N XML
> problemsPage n thing =
>     pageTemplate "problems"
>         [hsx| <p><% MsgProblems n thing %></p> |]
>

Instead of putting text in the `<p> </p>` tags we just use our message constructors.

Getting the language preferences from `ReaderT [Lang]` is just one possibility. Your application may already have a place to store session data that you can get the preferences from, or you might just stick the preferences in a cookie.

Detecting the preferred languages
---------------------------------

The `Accept-Language` header is sent by the client and, in theory, specifies what languages the client prefers, and how much they prefer each one. So, in the absence of any additional information, the `Accept-Language` header is a good starting place.  You can retrieve and parse the `Accept-Language` header using the `acceptLanguage` function and then sort the preferences in descending order using `bestLanguage`:

~~~~{.haskell}
acceptLanguage :: (Happstack m) => m [(Text, Maybe Double)]
bestLanguage   :: [(Text, Maybe Double)] -> [Text]
~~~~

You should not assume that the `Accept-Language` header is always correct. It is best to allow the user a way to override the `Accept-Language` header. That override could be stored in their user account, session data, a cookie, etc. In this example we will just use a `QUERY_STRING` parameter `_LANG` to override the `Accept-Language` header.

We can wrap this all up in a little function that converts our `I18N` part into a normal `ServerPart`:

> withI18N :: I18N a -> ServerPart a
> withI18N part = do
>   langsOverride <- queryString $ lookTexts' "_LANG"
>   langs         <- bestLanguage <$> acceptLanguage
>   mapServerPartT (flip runReaderT (langsOverride ++ langs)) (unHSPT part)
>

And finally, we just have our `route` table and `main` function:

> routes :: I18N XML
> routes =
>     msum [ do nullDir
>               homePage
>          , dir "goodbye"  $ goodbyePage
>          , dir "problems" $
>              do n     <- liftIO $ randomRIO (1, 99)
>                 let things = [TypeError .. SegFault]
>                 index <- liftIO $ randomRIO (0, length things - 1)
>                 let thing  = things !! index
>                 problemsPage n thing
>          ]
>
>
> main :: IO ()
> main = simpleHTTP nullConf $ withI18N routes

Source code for the app is [here](http://srclink/TemplatesHSP.hs).
You will also need to download and unzip the message files [here](http://srclink/messages.zip).

Conclusions
-----------

In this section we showed how to use `HSX` and `Happstack.Server.I18N`, and `shakespeare-i18n` together to provide an i18n solution. However, there are no dependencies between those libraries and modules. So, you can use other solutions to provide translations for `HSX`, or you can use `shakespeare-i18n` with other template systems.

One thing that would make `shakespeare-i18n` better is a utility to help keep the `.msg` files up-to-date. I have describe [my ideas for a tool here](https://github.com/yesodweb/hamlet/issues/40). We just need a volunteer to implement it.

