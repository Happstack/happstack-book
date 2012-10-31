
Passing multiple `AcidState` handles around transparently
---------------------------------------------------------

Manually passing around the `acid-state` handle gets tedious very quickly. A common solution is to stick the `AcidState` handle in a `ReaderT` monad. For example:


~~~~ {.haskell}
newtype MyApp = MyApp { unMyApp :: ReaderT (AcidState MyAppState) (ServerPartT IO) Response }
~~~~


We could then write some variants of the `update` and `query` functions which automatically retrieve the acid handle from `ReaderT`.

In this section we will show a slightly more sophisticated version of that solution which allows us to work with multiple `AcidState` handles and works well even if our app can be extended with optional plugins that contain additional `AcidState` handles.



> {-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
>   , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TemplateHaskell
>   , TypeFamilies, FlexibleInstances #-}

> module Main where

> import Control.Applicative  (Applicative, Alternative, (<$>))
> import Control.Exception.Lifted    (bracket)
> import Control.Monad.Trans.Control (MonadBaseControl)
> import Control.Monad        (MonadPlus, mplus)
> import Control.Monad.Reader (MonadReader, ReaderT(..), ask)
> import Control.Monad.Trans  (MonadIO(..))
> import Data.Acid            ( AcidState(..), EventState(..), EventResult(..)
>                             , Query(..), QueryEvent(..), Update(..), UpdateEvent(..)
>                             , IsAcidic(..), makeAcidic, openLocalState
>                             )
> import Data.Acid.Local      ( createCheckpointAndClose
>                             , openLocalStateFrom
>                             )
> import Data.Acid.Advanced   (query', update')
> import Data.Maybe           (fromMaybe)
> import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
> import Data.Data            (Data, Typeable)
> import Data.Lens            ((%=), (!=))
> import Data.Lens.Template   (makeLens)
> import Data.Text.Lazy       (Text)
> import Happstack.Server     ( Happstack, HasRqData, Method(GET, POST), Request(rqMethod)
>                             , Response
>                             , ServerPartT(..), WebMonad, FilterMonad, ServerMonad
>                             , askRq, decodeBody, dir, defaultBodyPolicy, lookText
>                             , mapServerPartT, nullConf, nullDir, ok, simpleHTTP
>                             , toResponse
>                             )
> import Prelude hiding       (head, id)
> import System.FilePath      ((</>))
> import Text.Blaze           ((!))
> import Text.Blaze.Html4.Strict (body, head, html, input, form, label, p, title, toHtml)
> import Text.Blaze.Html4.Strict.Attributes (action, enctype, for, id, method, name, type_, value)


The first thing we have is a very general class that allows us to
retrieve a specific `AcidState` handle by its type from an arbitrary
monad:


> class HasAcidState m st where
>    getAcidState :: m (AcidState st)


Next we redefine `query` and `update` so that they use `getAcidState`
to automatically retrieve the the correct acid-state handle from whatever monad they are in:


> query :: forall event m.
>          ( Functor m
>          , MonadIO m
>          , QueryEvent event
>          , HasAcidState m (EventState event)
>          ) =>
>          event
>       -> m (EventResult event)
> query event =
>     do as <- getAcidState
>        query' (as :: AcidState (EventState event)) event


> update :: forall event m.
>           ( Functor m
>           , MonadIO m
>           , UpdateEvent event
>           , HasAcidState m (EventState event)
>           ) =>
>           event
>        -> m (EventResult event)
> update event =
>     do as <- getAcidState
>        update' (as :: AcidState (EventState event)) event

> -- | bracket the opening and close of the `AcidState` handle.
>
> -- automatically creates a checkpoint on close
> withLocalState :: (MonadBaseControl IO m, MonadIO m, IsAcidic st, Typeable st) =>
>                   Maybe FilePath           -- ^ path to state directory
>                  -> st                     -- ^ initial state value
>                  -> (AcidState st -> m a) -- ^ function which uses the `AcidState` handle
>                  -> m a
> withLocalState mPath initialState =
>     bracket (liftIO $ (maybe openLocalState openLocalStateFrom mPath) initialState)
>             (liftIO . createCheckpointAndClose)



(These functions will eventually reside in `acid-state` itself, or some other library).

Now we can declare a couple `acid-state` types:



> -- State that stores a hit count
>
> data CountState = CountState { _count :: Integer }
>                 deriving (Eq, Ord, Data, Typeable, Show)
>
> $(deriveSafeCopy 0 'base ''CountState)
> $(makeLens ''CountState)
>
> initialCountState :: CountState
> initialCountState = CountState { _count = 0 }
>
> incCount :: Update CountState Integer
> incCount = count %= succ
>
> $(makeAcidic ''CountState ['incCount])

> -- State that stores a greeting

> data GreetingState = GreetingState {  _greeting :: Text }
>                 deriving (Eq, Ord, Data, Typeable, Show)
>
> $(deriveSafeCopy 0 'base ''GreetingState)
> $(makeLens ''GreetingState)
>
> initialGreetingState :: GreetingState
> initialGreetingState = GreetingState { _greeting = "Hello" }
>
> getGreeting :: Query GreetingState Text
> getGreeting = _greeting <$> ask
>
> setGreeting :: Text -> Update GreetingState Text
> setGreeting txt = greeting != txt
>
> $(makeAcidic ''GreetingState ['getGreeting, 'setGreeting])



Now that we have two states we can create a type to bundle them up like:



> data Acid = Acid { acidCountState    :: AcidState CountState
>                  , acidGreetingState :: AcidState GreetingState
>                  }
>
> withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
> withAcid mBasePath action =
>     let basePath = fromMaybe "_state" mBasePath
>     in withLocalState (Just $ basePath </> "count")    initialCountState    $ \c ->
>        withLocalState (Just $ basePath </> "greeting") initialGreetingState $ \g ->
>            action (Acid c g)



Now we can create our `App` monad that stores the `Acid` in the `ReaderT`:



> newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
>     deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
>                , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
>                , Happstack, MonadReader Acid)
>
> runApp :: Acid -> App a -> ServerPartT IO a
> runApp acid (App sp) = mapServerPartT (flip runReaderT acid) sp



And finally, we need to write the `HasAcidState` instances:



> instance HasAcidState App CountState where
>     getAcidState = acidCountState    <$> ask
>
> instance HasAcidState App GreetingState where
>     getAcidState = acidGreetingState <$> ask



And that's it. We can now use `update` and `query` in the remainder of our code with out having to worry about the `AcidState` argument anymore.

Here is a page function that uses both the `AcidStates` in a transparent manner:



> page :: App Response
> page =
>     do nullDir
>        g <- greet
>        c <- update IncCount -- ^ a CountState event
>        ok $ toResponse $
>           html $ do
>             head $ do
>               title "acid-state demo"
>             body $ do
>               form ! action "/" ! method "POST" ! enctype "multipart/form-data" $ do
>                 label "new message: " ! for "msg"
>                 input ! type_ "text" ! id "msg" ! name "greeting"
>                 input ! type_ "submit" ! value "update message"
>               p $ toHtml g
>               p $ do "This page has been loaded "
>                      toHtml c
>                      " time(s)."
>     where
>     greet =
>         do m <- rqMethod <$> askRq
>            case m of
>              POST ->
>                  do decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
>                     newGreeting <- lookText "greeting"
>                     update (SetGreeting newGreeting)   -- ^ a GreetingState event
>                     return newGreeting
>              GET  ->
>                  do query GetGreeting                  -- ^ a GreetingState event



If have used `happstack-state` in the past, then this may remind you of how `happstack-state` worked. However, there is a critical different. In `happstack-state` it was possible to call `update` and `query` on events for state components that were not actually loaded. In this solution, however, the `HasAcidState` class ensures that we can only call `update` and `query` for valid `AcidState` handles.

Our main function is simply:



> main :: IO ()
> main =
>     withAcid Nothing $ \acid ->
>         simpleHTTP nullConf $ runApp acid page



<h3><a name="acid-state-optional">Optional Plugins/Components</a></h3>

In an upcoming section we will explore various methods of extending your app via plugins and 3rd party libraries. These plugins and libraries may contain their own `AcidState` components. Very briefly, we will show how that might be handled.

Let's imagine we have this dummy plugin:


> newtype FooState = FooState { foo :: Text }
>     deriving (Eq, Ord, Data, Typeable, SafeCopy)
>
> initialFooState :: FooState
> initialFooState = FooState { foo = "foo" }
>
> askFoo :: Query FooState Text
> askFoo = foo <$> ask
>
> $(makeAcidic ''FooState ['askFoo])

> fooPlugin :: (Happstack m, HasAcidState m FooState) => m Response
> fooPlugin =
>     dir "foo" $ do
>        txt <- query AskFoo
>        ok $ toResponse txt


We could integrate it into our app by extending the `Acid` type to hold
the `FooState` and then add an appropriate `HasAcidState` instance:



> data Acid' = Acid' { acidCountState'    :: AcidState CountState
>                    , acidGreetingState' :: AcidState GreetingState
>                    , acidFooState'      :: AcidState FooState
>                    }

> withAcid' :: Maybe FilePath -> (Acid' -> IO a) -> IO a
> withAcid' mBasePath action =
>     let basePath = fromMaybe "_state" mBasePath
>     in withLocalState (Just $ basePath </> "count")    initialCountState    $ \c ->
>        withLocalState (Just $ basePath </> "greeting") initialGreetingState $ \g ->
>        withLocalState (Just $ basePath </> "foo")      initialFooState      $ \f ->
>            action (Acid' c g f)

> newtype App' a = App' { unApp' :: ServerPartT (ReaderT Acid' IO) a }
>     deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
>                , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
>                , Happstack, MonadReader Acid')
>

> instance HasAcidState App' FooState where
>     getAcidState = acidFooState' <$> ask



Now we can use `fooAppPlugin` like any other part in our app:



> fooAppPlugin :: App' Response
> fooAppPlugin = fooPlugin



An advantage of this method is that `fooPlugin` could also have access to the other `AcidState` components like `CountState` and `GreetingState`.

A different option would be for `fooPlugin` to use its own `ReaderT`



> fooReaderPlugin :: ReaderT (AcidState FooState) (ServerPartT IO) Response
> fooReaderPlugin = fooPlugin

> instance HasAcidState (ReaderT (AcidState FooState) (ServerPartT IO)) FooState where
>     getAcidState = ask

> withFooPlugin :: (MonadIO m, MonadBaseControl IO m) =>
>                  FilePath                          -- ^ path to state directory
>               -> (ServerPartT IO Response -> m a)  -- ^ function that uses fooPlugin
>               -> m a
> withFooPlugin basePath f =
>        do withLocalState (Just $ basePath </> "foo") initialFooState $ \fooState ->
>               f $ runReaderT fooReaderPlugin fooState

> main' :: IO ()
> main' =
>     withFooPlugin "_state" $ \fooPlugin' ->
>         withAcid Nothing $ \acid ->
>             simpleHTTP nullConf $ fooPlugin' `mplus` runApp acid page



We will come back to this in detail later when we explore plugins and libraries.

Source code for the app is [here](http://srclink/AcidStateAdvanced.hs).



