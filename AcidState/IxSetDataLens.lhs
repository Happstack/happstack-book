
`IxSet` and `Data.Lens`
-----------------------

To use `IxSet` with `Data.Lens` you will need to install the optional `data-lens-ixset`, `data-lens-template`, and `data-lens-fd` packages.

It is very common to use records and nested records with `IxSet` and `acid-state`. Unfortunately, Haskell record support is pretty pitiful at the moment. People have been proposing improvements for years -- but until some proposals get implemented we need some way to make life more pleasant. One popular solution is the `data-lens` library.

At first, lenses sound like they must be something really crazy or difficult -- like `Arrows` but even worse! But, in reality, lenses are pretty simple. Lenses are really just some new syntax to make it easy to compose getters, setters, and modifiers.

It can take a bit of practice to get used to lenses. But, fortunately, using them is completely optional -- so if they are not your thing, you don't have to use them. In this tutorial we will start with a general introduction to using lenses, and then finish up with showing how to use them with `IxSet` and `acid-state`.



> {-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving
>   , OverloadedStrings, TemplateHaskell #-}

> module Main where
>
> import Control.Applicative (pure)
> import Control.Category   ((.), (>>>))
> import Control.Comonad.Trans.Store.Lazy
> import Data.Acid          (Update)
> import Data.Data          (Data, Typeable)
> import Data.IxSet         ( IxSet, Indexable(empty), (@=)
>                           , fromList, ixFun, ixSet
>                           )
> import Data.Lens          ( Lens, (^$), (^.), (^=), (^%=), (^%%=)
>                           , (^+=), (%=), getL, setL, modL
>                           )
> import Data.Lens.Template (makeLens)
> import Data.Lens.IxSet    (ixLens)
> import Data.Lens.Partial.Common ( PartialLens(..), maybeLens
>                                 , totalLens)
> import Data.SafeCopy      (SafeCopy, base, deriveSafeCopy)
> import Data.Text          (Text)
> import Prelude            hiding ((.))



%%% Creating Lenses

We start by defining a simple `User` record which contains some nested records:


> newtype UserId = UserId { _userInt :: Integer }
>     deriving (Eq, Ord, Data, Typeable, SafeCopy, Show)
>
> $(makeLens ''UserId)
>
> data Name = Name
>     { _nickName  :: Text
>     , _firstName :: Text
>     , _lastName  :: Text
>     }
>     deriving (Eq, Ord, Data, Typeable, Show)
>
> $(deriveSafeCopy 0 'base ''Name)
> $(makeLens ''Name)
>
> data User = User
>     { _userId :: UserId
>     , _name   :: Name
>     }
>     deriving (Eq, Ord, Data, Typeable, Show)
>
> $(deriveSafeCopy 0 'base ''User)
> $(makeLens ''User)
>
> -- | example user
> stepcut :: User
> stepcut =
>     User { _userId = UserId 0
>          , _name   = Name { _nickName  = "stepcut"
>                           , _firstName = "Jeremy"
>                           , _lastName  = "Shaw"
>                           }
>          }



There are two things to notice:

 1. the field names all start with an underscore. That is because new helper functions will be generated that do not contain the underscore.

 2. there is a Template Haskell call `$(makeLens ''Type)`. This Template Haskell call generates lens functions based on the fields in the record. If you want to see what it is actually generating you can compile with `-ddump-splices`. Here is one of the lenses generated by `$(makeLens ''User)`:


~~~~ {.haskell}
userId :: Lens User UserId
userId = lens _userId (\ uid user -> user { _userId = uid })
~~~~


We see that a lens is basically just a getter function and a setter function. In this case, a getter function that can get a `UserId` from a `User` and a setter function that can set the `UserId` in a `User`.

%%% Getters

There are 4 infix getter operators: two for left-to-right composition `((^$), (^$!))` and two for right-to-left composition `((^.), (^!))`. There is also a normal function, `getL`. They all serve the same purpose -- it's just a matter of taste which you use.

The left-to-right operators are:

~~~~ {.haskell}
(^$), (^$!)  :: Lens a b -> a -> b
~~~~

These functions basically do the same thing except `^$` is lazy and `^$!` uses uses `$!` internally to more strictly evaluate the calculation.

They are used like this:

> stepcutFirstName :: Text
> stepcutFirstName = firstName ^$ name ^$ stepcut

Notice that `^$` is used a lot like `$`. In fact we could write this with out lenses at all like this:

> stepcutFirstName2 :: Text
> stepcutFirstName2 = _firstName $ _name $ stepcut

The right-to-left operators are:

~~~~ {.haskell}
(^.), (^!) :: a -> Lens a b -> b
~~~~

Where `^.` is the lazier version and `^!` is the stricter version.

We can use it like this:

> stepcutFirstName3 :: Text
> stepcutFirstName3 = (stepcut ^. name) ^. firstName

So, here `^.` is meant to act like a field accessor. In a traditional object oriented language where we would write:

~~~~
stepcut.name.firstName
~~~~

Finally, we have the `getL` function:

~~~~ {.haskell}
getL :: Lens a b -> a -> b
~~~~


`getL` is useful for creating partially applied functions. For
example, we can create a function that gets a `User`'s first name like
this:


> getFirstName :: User -> Text
> getFirstName = getL firstName . getL name


%%% `Lens` is an instance of `Category`

`Lens` is an instance of `Category`. That means we can use the `.` operator from `Category` to compose lenses.

The normal `.` looks like this:


~~~~ {.haskell}
-- | as defined in 'Prelude'
(.) :: (b -> c) -> (a -> b) -> a -> c
~~~~


But `.` can be generalized to work for any `Category` like this:


~~~~ {.haskell}
-- | as defined in 'Control.Category'
(.)  :: (Category cat) => cat b c -> cat a b -> cat a c
~~~~


If you look closely at the imports at the top, you will notice that we hide `.` from the `Prelude` and imported the version from `Control.Category` instead. Now we can write this:



> stepcutFirstName4 :: Text
> stepcutFirstName4 = firstName . name ^$ stepcut



Which looks very similar to the non-lens version:



> stepcutFirstName5 :: Text
> stepcutFirstName5 = _firstName . _name $ stepcut



If we look at the type of `firstName . name` we see that we just get a lens that goes straight from `User` to `Text`:


~~~~ {.haskell}
*Main> :t firstName . name<br>
firstName . name :: Lens User Text
~~~~

%%% Setters

Next we have the setter operators. This is where lenses start to shine. The setter operators are:

~~~~ {.haskell}
(^=), (^!=) :: Lens a b -> b -> a -> a
~~~~

Once again we have a lazier version `^=` and a stricter version `^!=`.

We can use it to update the `UserId` in the `User` type like this:

> setUserId :: (User -> User)
> setUserId = userId ^= (UserId 1)

So, we see that `^=` is used to create an update function. If we wanted to update a specific record we could write it like this:

> setStepcutUserId :: User
> setStepcutUserId = userId ^= (UserId 1) $ stepcut

Instead of the infix operator we could use `setL`:

~~~~ {.haskell}
setL :: Lens a b -> b -> a -> a
~~~~

as such:

> setUserId' :: (User -> User)
> setUserId' = setL userId (UserId 1)

%%% Modifiers

Often times we want to apply a function to transform an existing value rather than just setting a new value. For that we use:


~~~~ {.haskell}
(^%=), (^!%=) :: Lens a b -> (b -> b) -> a -> a
~~~~


For example, we can increment the `Integer` inside the `UserId` like so:



> incUserId :: UserId -> UserId
> incUserId = (userInt ^%= succ)



Or we could use the `modL` function:


~~~~ {.haskell}
modL :: Lens a b -> (b -> b) -> a -> a
~~~~




> incUserId' :: UserId -> UserId
> incUserId' = modL userInt succ




%%% Updating Nested Records

If we want to update a nested record then we need to combine setters and modifiers. For example, we can update the `nickName` like this:

> setNick :: Text -> (User -> User)
> setNick nick = name ^%= (nickName ^= nick)


That says we want to modify the `name` field of a `User` by setting the `nickName` of the `Name`.

Another option would be to leverage the `Category` instance for `Lens` and use the `.` operator:



> setNick2 :: Text -> (User -> User)
> setNick2 newNick = (nickName . name) ^= newNick



However, I find that a bit confusing to read, because the field names are listed right-to-left, but the overall flow of that line is left-to-right. If we want a consistent left-to-right feel we can use the `>>>` operator:


~~~~ {.haskell}
(>>>) :: Control.Category.Category cat =>
         cat a b
      -> cat b c
      -> cat a c
~~~~

> setNick3 :: Text -> (User -> User)
> setNick3 newNick = (name >>> nickName) ^= newNick


%%% Other Modifiers

The lens library also provides some operators that encapsulate common updates such as addition and subtraction. For example,


> addToUserId :: Integer -> (UserId -> UserId)
> addToUserId i = (userInt ^+= i)

%%% Lens for `IxSet`

So far we have examined updating fields in a record. But there is no reason why a lens need to be limited to a record. The idea can be used with just about any type where we have the ability to focus on a single element. We can create a lens for an `IxSet` by using this `ixLens` function:


~~~~ {.haskell}
ixLens :: (Typeable key, Indexable a, Typeable a, Ord a) =>
          key -> Lens (IxSet a) (Maybe a)
~~~~


For records, the names of the fields are known at compile time, so we were able to automatically create helper functions like `userId`, `name`, etc, to address those fields. For an `IxSet` we generally want to address some value by a key that is determined at runtime, so we can not automatically generate helper functions.

First we need an `Indexable User` instance so that we can use `User` in an `IxSet`:

> instance Indexable User where
>     empty = ixSet [ ixFun $ \u -> [ userId ^$ u ]
>                   ]



And then we will add the `IxSet` to a state record:



> data UserState = UserState
>     { _nextUserId :: UserId
>     , _users      :: IxSet User
>     }
>     deriving (Eq, Ord, Data, Typeable, Show)
>
> $(deriveSafeCopy 0 'base ''UserState)
> $(makeLens ''UserState)
>
> userState :: UserState
> userState =
>     UserState { _nextUserId = UserId 1
>               , _users      = fromList [ stepcut ]
>               }
>



%%% Using a getter with `IxSet`

It is not a bad idea to define an alias for `ixLens` that has a more meaningful name and a more explicit type:


> user :: (Typeable key) => key -> Lens (IxSet User) (Maybe User)
> user = ixLens


That will help make it easier to read the code, and will make type errors more readable.

Now we can extract the `User` with `UserId 0` from `userState`:

> user0 :: Maybe User
> user0 = user (UserId 0) ^$ users ^$ userState

%%% Inserting an element into an `IxSet`

We can use the setter operator to add a new record to an `IxSet`:

> addUserId1 :: UserState
> addUserId1 =
>  -- create a duplicate of the stepcut
>  -- record but with 'UserId 1'
>  let stepcut1 = userId ^= (UserId 1) $ stepcut
>  in (users ^%= user (userId ^$ stepcut1) ^= (Just stepcut1)) $
>      userState

So, there is something a little tricky going on here. Under the hood,
we are using `updateIx` to insert the record. In this case, we are
updating the non-existing record for `UserId 1`.

An `updateIx` is performed by deleting the old record (if it exists) and inserting
the new one. However, the key used to delete the old record may not
match the key in the new record we are inserting. For example, if we
did:

> addUserId1' :: UserState
> addUserId1' =
>  -- create a duplicate of the stepcut
>  -- record but with 'UserId 1'
>  let stepcut1 = userId ^= (UserId 1) $ stepcut
>  in (users ^%= user (UserId 0) ^= (Just stepcut1)) userState

That would delete the existing `UserId 0` record and add a `UserId 1` record instead. It would *not* insert the `stepcut1` record as `UserId 0`.

Alas, this is actually a violation of the lens laws. There isn't
really a way to work around it, but the `IxSet` lens is still useful
as long as you are aware of this issue.

%%% Deleting an element from an `IxSet`

We can delete an element from an `IxSet` by updating it with `Nothing`.



> deleteUserId0 :: UserState
> deleteUserId0 = (users ^%= user (UserId 0) ^= Nothing) userState



%%% Using a modifier with `IxSet`

Here we update the `nickName` for `UserId 0`:

> changeNick :: UserState
> changeNick =
>   (users ^%= user (UserId 0)
>          ^%= fmap (name ^%= (nickName ^= "stepkut"))) userState


In a traditional imperative language we write `changeNick` something like:


    changeNick() { userState.users[0].name.nickName = "stepkut"; }


Looking at the two, you can see the similarity, even if the syntax is not as nice.

One important things to note is that the `ixLens` returns a `Maybe` value since we might request a non-existent `UserId`. Here we use `fmap` to set the nick inside the `Maybe` value. However, that means that for a non-existent `UserId` the update silently does nothing. Sometimes that is ok, but if not, then you will need to take a different approach.

We can also try to use `>>>`  instead of all those `^%=`, but the `fmap` is a bit troublesome:



> changeNick2 :: UserState
> changeNick2 =
>   ((users >>> user (UserId 0)) ^%=
>    fmap ((name >>> nickName) ^= "stepkut")) userState


Additionally, it seems like `^%=` binds too tightly and so we need some extra `( )` to keep things happy.

%%% Using `partial-lens` with `IxSet`

The [`partial-lens`](http://hackage.haskell.org/package/partial-lens) attempts to address the `fmap` problem that we saw in the last section. A `partial-lens` is similar to a `lens` but allows for the fact that the lens may not always be able to produce a value:


~~~~ {.haskell}
newtype PartialLens a b = PLens (a -> Maybe (Store b a))
~~~~


However, it seems a bit awkward to use `partial-lens` at the moment. To use a normal lens with need to convert it to a partial lens using `totalLens`:


~~~~ {.haskell}
totalLens :: Lens a b -> PartialLens a b
~~~~


Additionally, `partial-lens` lacks the `MonadState` interaction that we will examine in the next section. But, hopefully these issues will be resolved in the future.

We can turn our `ixLens` into a partial lens like this:

> -- | note: `setPL` does not insert into an `IxSet` it only
> --   modifies a value if the key already exists in the map.
> ixPLens :: (Typeable key, Ord a, Typeable a, Indexable a) =>
>            key -> PartialLens (IxSet a) a
> ixPLens key = maybeLens . totalLens (ixLens key)

See the [haddock page](http://hackage.haskell.org/package/partial-lens) for `partial-lens` for more information. Using `partial-lens` is very similar to a normal lens.

%%% Using a setter and modifier with `IxSet` in an `acid-state` event

If we are using `IxSet` with `acid-state`, we can use a special version of the modifier operator that automatically does the `get`/`put` for us:

~~~~ {.haskell}
(%=) :: (MonadState a m) => Lens a b -> (b -> b) -> m b
~~~~

Note that this version of `%=` was imported from `Data.Lens` which comes from the `data-lens-fd` package. There are similar functions in `Data.Lens.Strict` and `Data.Lens.Lazy` but they do not have the right type.

We can now make `changeNick` into an `Update` event like this:



> changeNick' :: Update UserState (IxSet User)
> changeNick' =
>  users %= user (UserId 0)
>       ^%= fmap (name ^%= (nickName ^= "stepkut"))



All we did was change the first `^%=` to `%=`. This works because `Update` is an instance of `MonadState`.


`data-lens-fd` provides a few other functions that you can use to get, set, and modify the state in an `Update` or `Query` event. Check out the <a href='http://hackage.haskell.org/package/data-lens-fd'> haddock documentation for data-lens-fd</a>.

Source code for the app is [here](http://srclink/IxSetDataLens.hs).
