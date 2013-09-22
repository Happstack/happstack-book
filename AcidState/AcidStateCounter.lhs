
`acid-state` counter
--------------------

Our first example is a very simple hit counter app.

First a bunch of `LANGUAGE` pragmas and imports:


> {-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
>   GeneralizedNewtypeDeriving, MultiParamTypeClasses,
>   TemplateHaskell, TypeFamilies, RecordWildCards #-}
>
> module Main where
>
> import Control.Applicative  ( (<$>) )
> import Control.Exception    ( bracket )
> import Control.Monad        ( msum )
> import Control.Monad.Reader ( ask )
> import Control.Monad.State  ( get, put )
> import Data.Data            ( Data, Typeable )
> import Happstack.Server     ( Response, ServerPart, dir
>                             , nullDir, nullConf, ok
>                             , simpleHTTP, toResponse )
> import Data.Acid            ( AcidState, Query, Update
>                             , makeAcidic, openLocalState )
> import Data.Acid.Advanced   ( query', update' )
> import Data.Acid.Local      ( createCheckpointAndClose )
> import Data.SafeCopy        ( base, deriveSafeCopy )


Next we define a type that we wish to store in our state. In this case we just create a simple record with a single field `count`:


> data CounterState = CounterState { count :: Integer }
>     deriving (Eq, Ord, Read, Show, Data, Typeable)
>
> $(deriveSafeCopy 0 'base ''CounterState)
>


`deriveSafeCopy` creates an instance of the `SafeCopy` class for
`CounterState`. `SafeCopy` is class for versioned serialization,
deserilization, and migration. The `SafeCopy` class is a bit like a
combination of the `Read` and `Show` classes, except that it converts
the data to a compact `ByteString` representation, and it includes
version information in case the type changes and old data needs to be
migrated.

Since this is the first version of the `CounterState` type, we give it
version number 0 and declare it to be the `base` type. Later if we
change the type, we would increment the version to 1 and declare it to
be an `extension` of a previous type. We would also provide a migration
instance to migrate the old type to the new type. The migration would
happen automatically when the old state is read. For more information
on `SafeCopy, base, extension` and migration see the <a
href="http://hackage.haskell.org/packages/archive/safecopy/0.6.1/doc/html/Data-SafeCopy.html">haddock
docs</a>. (A detailed section on migration for the Crash Course is
planned, but not yet written).

If you are not familiar with Template Haskell be sure to read the Template Haskell appendix for brief intro to Template Haskell.

Next we will define an initial value that is suitable for initializing
the `CounterState` state.

> initialCounterState :: CounterState
> initialCounterState = CounterState 0

Now that we have our types, we can define some update and query functions.

First let's define an update function which increments the count and returns the incremented value:

> incCountBy :: Integer -> Update CounterState Integer
> incCountBy n =
>     do c@CounterState{..} <- get
>        let newCount = count + n
>        put $ c { count = newCount }
>        return newCount
>

In this line:

~~~~ {.haskell}
c@CounterState{..} <- get
~~~~

we are using the `RecordWildCards` extension. The `{..}` binds all the fields of the record to symbols with the same name. That is why in the next line we can just write `count` instead of `(count c)`. Using `RecordWildCards` here is completely optional, but tends to make the code less cluttered, and easier to read.

Also notice that we are using the `get` and `put` functions from `MonadState` to get and put the ACID state. The `Update` monad is basically an enchanced version of the `State` monad. For the moment it is perhaps easiest to just pretend that `incCountBy` has the type signature:


~~~~ {.haskell}
incCountBy :: Integer -> State CounterState Integer
~~~~


And then it becomes clearer that `incCountBy` is just a simple function in the `State` monad which updates `CounterState` and returns an `Integer`.

Note that even though we are using a monad here.. the code is still pure. If we wanted we could have required the update function to have a type like this instead:


~~~~ {.haskell}
incCountBy :: Integer -> CounterState -> (CounterState, Integer)
~~~~


In that version, the current state is explicitly passed in, and the function explicitly returns the updated state. The monadic version does the same thing, but uses `>>=` to make the plumbing easier. This makes the monadic version easier to read and reduces mistakes.

When we later use the `update` function to call `incCountBy`, `incCountBy` will be run in an isolated manner (the 'I' in ACID). That means that you do not need to worry about some other thread modifying the `CounterState` between the `get` and the `put`. It will also be run atomically (the 'A' in ACID), meaning that either the whole function will run or it will not run at all. If the server is killed mid-transaction, the transaction will either be completely applied or not applied at all.

You may also note that `Update` (and `State`) are not instances of the `MonadIO` class. This means you can not perform IO inside the update. This is by design. In order to ensure Durability and to support replication, events need to be pure. That allows us to be confident that if the event log has to be replayed -- it will result in the same state we had before.

We can also define a query which reads the state, and does not update it:


> peekCount :: Query CounterState Integer
> peekCount = count <$> ask
>


The `Query` monad is an enhanced version of the `Reader` monad. So we can pretend that `peekCount` has the type:


~~~~ {.haskell}
peekCount :: Reader CounterState Integer
~~~~


Although we could have just used `get` in the `Update` monad, it is better to use the `Query` monad if you are doing a read-only operation because it will not block other database transactions. It also lets the user calling the function know that the database will not be affected.

Next we have to turn the update and query functions into acid-state events. This is almost always done by using the Template Haskell function `makeAcidic`


> $(makeAcidic ''CounterState ['incCountBy, 'peekCount])
>


The `makeAcidic` function creates a bunch of boilerplate types and type class instances. If you want to see what is happening under the hood, check out the examples [here](http://mirror.seize.it/acid-state/examples/). The examples with names like, `HelloWorldNoTH.hs` show how to implement the boilerplate by hand. In practice, you will probably never want to or need to do this. But you may find it useful to have a basic understanding of what is happening. You could also use the `-ddump-splices` flag to GHC to see the auto-generated instances -- but the lack of formatting makes it difficult to read.

Here we actually call our query and update functions:

> handlers :: AcidState CounterState -> ServerPart Response
> handlers acid = msum
>   [ dir "peek" $ do
>       c <- query' acid PeekCount
>       ok $ toResponse $"peeked at the count and saw: " ++ show c
>   , do nullDir
>        c <- update' acid (IncCountBy 1)
>        ok $ toResponse $ "New count is: " ++ show c
>   ]
>



Note that we do not call the `incCountBy` and `peekCount` functions directly. Instead we invoke them using the `update'` and `query'` functions:


~~~~ {.haskell}
update' :: (UpdateEvent event, MonadIO m) =>
           AcidState (EventState event) -- ^ handle to acid-state
        -> event                        -- ^ update event to execute
        -> m (EventResult event)
query'  :: (QueryEvent event , MonadIO m) =>
           AcidState (EventState event) -- ^ handle to acid-state
        -> event                        -- ^ query event to execute
        -> m (EventResult event)
~~~~


Thanks to `makeAcidic`, the functions that we originally defined now have types with the same name, but starting with an uppercase letter:


~~~~ {.haskell}
data PeekCount  = PeekCount
data IncCountBy = IncCountBy Integer
~~~~


The arguments to the constructors are the same as the arguments to the original function.

So now we can decipher the meaning of the type for the `update'` and `query'` functions. For example, in this code:


~~~~ {.haskell}
c <- update' acid (IncCountBy 1)
~~~~


The event is `(IncCountBy 1)` which has the type `IncCountBy`. Since there is an `UpdateEvent IncCountBy` instance, we can use this event with the `update'` function. That gives us:


~~~~ {.haskell}
update' :: (UpdateEvent IncCountBy, MonadIO m) =>
           AcidState (EventState IncCountBy)
        -> IncCountBy
        -> m (EventResult IncCountBy)
~~~~

`EventState` is a type function. `EventState IncCountBy` results in the type `CounterState`. So that reduces to `AcidState CounterState`. So, we see that we can not accidently call the `IncCountBy` event against an acid state handle of the wrong type.

`EventResult` is also a type function. `EventResult IncCountBy` is `Integer`, as we would expect from the type signature for `IncCountBy`.

As mentioned earlier, the underlying update and query events we created are pure functions. But, in order to have a durable database (aka, be able to recover after powerloss, etc) we do need to log the update events to disk so that we can reply them in the event of a recovery. So, rather than invoke our update and query events directly, we call them indirectly via the `update` and `query` functions. `update` and `query` interact with the `acid-state` system to ensure that the acid-state events are properly logged, called in the correct order, run atomitically and isolated, etc.

There is no way in Haskell to save a function to save a function to disk or send it over the network. So, `acid-state` has to cheat a little. Instead of storing the function, it just stores the name of the function and the value of its arguments. That is what the `IncCountBy` type is for -- it is the value that can be serialized and saved to disk or sent over the network.

Finally, we have our main function:

> main :: IO ()
> main =
>   bracket (openLocalState initialCounterState)
>           (createCheckpointAndClose)
>            (\acid ->
>                simpleHTTP nullConf (handlers acid))

`openLocalState` starts up `acid-state` and returns an handle. If existing state is found on the disk, it will be automatically restored and used. If no pre-existing state is found, then `initialCounterState` will be used. `openLocalState` stores data in a directory named `state/[typeOf state]`. In this example, that would be, `state/CounterState`. If you want control over where the state information is stored use `openLocalStateFrom` instead.

The shutdown sequence creates a checkpoint when the server exits. This is good practice because it helps the server start faster, and makes migration go more smoothly. Calling `createCheckpointAndClose` is not critical to data integrity. If the server crashes unexpectedly, it will replay all the logged transactions (Durability). However, it is a good idea to create a checkpoint on close. If you change an existing update event, and then tried to replay old versions of the event, things would probably end poorly. However, restoring from a checkpoint does not require the old events to be replayed. Hence, always creating a checkpoint on shutdown makes it easier to upgrade the server.

Source code for the app is [here](http://srclink/AcidState/AcidStateCounter.hs).
