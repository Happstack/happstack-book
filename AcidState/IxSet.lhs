
IxSet: a set with multiple indexed keys
---------------------------------------

To use `IxSet` you will need to install the optional `ixset` package.

In the first `acid-state` example we stored a single value. But in real database we typically need to store a large collection of records. And we want to be able to efficiently search and update those records. For simple key/value pairs we can use `Data.Map`. However, in practice, we often want to have *multiple* keys. That is what `IxSet` set offers -- a set-like type which can be indexed by multiple keys.

Instead of having:

~~~~ {.haskell}
Set Foo
~~~~

we will have:

~~~~ {.haskell}
IxSet Foo
~~~~

with the ability to do queries based on the indices of `Foo`, which are defined using the `Indexable` type-class.

IxSet can be found [here on hackage](http://hackage.haskell.org/package/ixset).

In this example, we will use `IxSet` to create a mini-blog.

> {-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
>   RecordWildCards, TemplateHaskell, TypeFamilies,
>   OverloadedStrings #-}

> module Main where

> import Control.Applicative  ((<$>), optional)
> import Control.Exception    (bracket)
> import Control.Monad        (msum, mzero)
> import Control.Monad.Reader (ask)
> import Control.Monad.State  (get, put)
> import Control.Monad.Trans  (liftIO)
> import Data.Acid            ( AcidState, Update, Query
>                             , makeAcidic, openLocalState
>                             )
> import Data.Acid.Advanced   (update', query')
> import Data.Acid.Local      (createCheckpointAndClose)
> import Data.Data            (Data, Typeable)
> import Data.IxSet           ( Indexable(..), IxSet(..), (@=)
>                             , Proxy(..), getOne, ixFun, ixSet )
> import qualified Data.IxSet as IxSet
> import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
> import Data.Text            (Text)
> import Data.Text.Lazy       (toStrict)
> import qualified Data.Text  as Text
> import Data.Time            (UTCTime(..), getCurrentTime)
> import Happstack.Server
>     ( ServerPart, Method(POST, HEAD, GET), Response, decodeBody
>     , defaultBodyPolicy, dir, lookRead, lookText, method
>     , notFound, nullConf, nullDir, ok, seeOther, simpleHTTP
>     , toResponse)
> import           Text.Blaze.Html ((!), Html)
> import qualified Text.Blaze.Html4.Strict as H
> import qualified Text.Blaze.Html4.Strict.Attributes as A


The first thing we are going to need is a type to represent a blog post.

It is convenient to assign a unique id to each blog post so that it can be easily referenced in URLs and easily queried in the `IxSet`. In order to keep ourselves sane, we can create a `newtype` wrapper around an `Integer` instead of just using a nameless `Integer`.


> newtype PostId = PostId { unPostId :: Integer }
>     deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)


Note that in addition to deriving normal classes like `Eq` and `Ord`, we also derive an instance of `SafeCopy`. This is not required by `IxSet` itself, but since we want to store the our blog posts in `acid-state` we will need it there.

A blog post will be able to have two statuses 'draft' and 'published'. We could use a boolean value, but it is easier to understand what `Draft` and `Published` mean instead of trying to remember what `True` and `False` mean. Additionally, we can easily extend the type with additional statuses later.


> data Status =
>     Draft
>   | Published
>     deriving (Eq, Ord, Data, Typeable)
>
> $(deriveSafeCopy 0 'base ''Status)


And now we can create a simple record which represents a single blog post:

> data Post = Post
>     { postId  :: PostId
>     , title   :: Text
>     , author  :: Text
>     , body    :: Text
>     , date    :: UTCTime
>     , status  :: Status
>     , tags    :: [Text]
>     }
>     deriving (Eq, Ord, Data, Typeable)
>
> $(deriveSafeCopy 0 'base ''Post)



Each `IxSet` key needs to have a unique type. Looking at `Post` it seems like that could be trouble -- because we have multiple fields which all have the type `Text`. Fortunately, we can easily get around this by introducing some newtypes which are used for indexing.


> newtype Title     = Title Text
>     deriving (Eq, Ord, Data, Typeable, SafeCopy)
> newtype Author    = Author Text
>     deriving (Eq, Ord, Data, Typeable, SafeCopy)
> newtype Tag       = Tag Text
>     deriving (Eq, Ord, Data, Typeable, SafeCopy)
> newtype WordCount = WordCount Int
>     deriving (Eq, Ord, Data, Typeable, SafeCopy)


%%% Defining the indexing keys

We are now ready to create an instance of the `Indexable` class. This is the class that defines the keys for a `Post` so that we can store it in an `IxSet`:


> instance Indexable Post where
>   empty = ixSet
>     [ ixFun $ \bp -> [ postId bp ]
>     , ixFun $ \bp -> [ Title  $ title bp  ]
>     , ixFun $ \bp -> [ Author $ author bp ]
>     , ixFun $ \bp -> [ status bp ]
>     , ixFun $ \bp -> map Tag (tags bp)
>     , ixFun $ (:[]) . date  -- point-free, just for variety
>     , ixFun $ \bp -> [ WordCount (length $ Text.words $ body bp) ]
>     ]
>


In the `Indexable Post` instance we create a list of `Ix Post` values by using the `ixFun` helper function:


~~~~ {.haskell}
ixFun :: (Ord b, Typeable b) => (a -> [b]) -> Ix a
~~~~


We pass a key extraction function to `ixFun`. For example, in this line:


~~~~ {.haskell}
ixFun $ \bp -> [ postId bp ]
~~~~


we extract the `PostId` from a `Post`. Note that we return a list of keys values not just a single key. That is because a single entry might have several keys for a specific type. For example, a `Post` has a list of tags. But, we want to be able to search for posts that match a specific tag. So, we index each tag separately:


~~~~ {.haskell}
ixFun $ \bp -> map Tag (tags bp)
~~~~


Note that the keys do not have to directly correspond to a field in the record. We can perform calculations to create arbitrary keys. For example, the `WordCount` key calculates the number of words in a post:


~~~~ {.haskell}
ixFun $ \bp -> [ WordCount (length $ Text.words $ body bp) ]
~~~~


For the `Title` and `Author` keys we add the newtype wrapper.

Now we will create the record that we will use with acid-state to hold the `IxSet Post` and other state information.



> data Blog = Blog
>     { nextPostId :: PostId
>     , posts      :: IxSet Post
>     }
>     deriving (Data, Typeable)
>
> $(deriveSafeCopy 0 'base ''Blog)
>
> initialBlogState :: Blog
> initialBlogState =
>     Blog { nextPostId = PostId 1
>          , posts      = empty
>          }



`IxSet` does not (currently) provide any auto-increment functionality for indexes, so we have to keep track of what the next available `PostId` is ourselves. That is why we have the `nextPostId` field. (Feel free to submit a patch that adds an auto-increment feature to `IxSet`!).

Note that in `initialBlogState` the `nextPostId` is initialized to 1 not 0. Sometimes we want to create a `Post` that is not yet in the database, and hence does not have a valid `PostId`. I like to reserve `PostId 0` to mean uninitialized. If I ever see a `PostId 0` stored in the database, I know there is a bug in my code.

%%% Inserting a Record

Next we will create some update and query functions for our acid-state database.


> -- | create a new, empty post and add it to the database
> newPost :: UTCTime -> Update Blog Post
> newPost pubDate =
>     do b@Blog{..} <- get
>        let post = Post { postId = nextPostId
>                        , title  = Text.empty
>                        , author = Text.empty
>                        , body   = Text.empty
>                        , date   = pubDate
>                        , status = Draft
>                        , tags   = []
>                        }
>        put $ b { nextPostId = succ nextPostId
>                , posts      = IxSet.insert post posts
>                }
>        return post



Nothing in that function should be too surprising. We have to pass in `UTCTime`, because we can not do IO in the update function. Because `PostId` is an instance of `Enum` we can use `succ` to increment it. To add the new post to the `IxSet` we use `IxSet.insert`.


~~~~ {.haskell}
insert :: (Typeable a, Ord a, Indexable a) =>
          a -> IxSet a -> IxSet a
~~~~


%%% Updating a Record

Next we have a function that updates an existing `Post` in the database with a newer version:


> -- | update the post in the database (indexed by PostId)
> updatePost :: Post -> Update Blog ()
> updatePost updatedPost = do
>   b@Blog{..} <- get
>   put $ b { posts =
>              IxSet.updateIx (postId updatedPost) updatedPost posts
>           }



Note that instead of `insert` we use `updateIx`:


~~~~ {.haskell}
updateIx :: (Indexable a, Ord a, Typeable a, Typeable key) =>
            key
         -> a
         -> IxSet a
         -> IxSet a
~~~~


The first argument to `updateIx` is a key that maps to the post we want to updated in the database. The key must uniquely identify a single entry in the database. In this case we use our primary key, `PostId`.

%%% Looking up a value by its indexed key

Next we have some query functions.



> postById :: PostId -> Query Blog (Maybe Post)
> postById pid =
>      do Blog{..} <- ask
>         return $ getOne $ posts @= pid



`postById` is used to lookup a specific post by its `PostId`. This is our first example of querying an `IxSet`. Here we use the equals query operator:


~~~~ {.haskell}
(@=) :: (Typeable key, Ord a, Typeable a, Indexable a) =>
        IxSet a -> key -> IxSet a
~~~~


It takes an `IxSet` and filters it to produce a new `IxSet` which only contains values that match the specified key. In this case, we have specified the primary key (`PostId`), so we expect exactly zero or one values in the resulting `IxSet`. We can use `getOne` to turn the result into a simple `Maybe` value:


~~~~ {.haskell}
getOne :: Ord a => IxSet a -> Maybe a
~~~~


%%% Ordering the Results and the `Proxy` type

Here is a query function that gets all the posts with a specific status (`Published` vs `Draft`) and sorts them in reverse chronological order (aka, newest first):



> postsByStatus :: Status -> Query Blog [Post]
> postsByStatus status = do
>  Blog{..} <- ask
>  let posts' =
>        IxSet.toDescList (Proxy :: Proxy UTCTime) $
>          posts @= status
>  return posts'



We use the `@=` operator again to select just the posts which have the matching status. Since the publication date is a key (`UTCTime`) we can use `toDescList` to return a sorted list:


~~~~ {.haskell}
toDescList :: (Typeable k, Typeable a, Indexable a) =>
              Proxy k -> IxSet a -> [a]
~~~~


`toDescList` takes a funny argument `(Proxy :: Proxy UTCTime)`. While the `Post` type itself has an `Ord` instance -- we generally want to order by a specific key, which may have a different ordering. Since our keys are specified by type, we need a way to pass a type to `toDescList` so that it knows which key we want to order by. The `Proxy` type exists for that sole reason:


~~~~ {.haskell}
data Proxy a = Proxy
~~~~


It just gives us a place to stick a type signature that `toDescList` and other functions can use.

%%% Summary

You have now seen the basics of using `IxSet`. `IxSet` includes numerous other operations such as range-based queries, deleting records, converting to and from lists and Sets. See the <a href='http://hackage.haskell.org/package/ixset'>haddock docs</a> for a complete list of functions and their descriptions. You should have no difficulty understanding what they do based on what we have already seen.

%%% Rest of the Example Code

The remainder of the code in this section integrates the above code into a fully functioning example. In order to keep things simple I have just used `blaze-html`. In a real application I would use `reform` to deal with the form generation and validation. (I would probably also use `web-routes` to provide type-safe URLs, and `HSP` for the templates). But those topics are covered elsewhere. The remainder of the code in this section does not contain any new concepts that have not already been covered in previous sections of the crash course.

> $(makeAcidic ''Blog
>   [ 'newPost
>   , 'updatePost
>   , 'postById
>   , 'postsByStatus
>   ])

> -- | HTML template that we use to render all the
> --   pages on the site
> template :: Text -> [Html] -> Html -> Response
> template title headers body =
>   toResponse $
>     H.html $ do
>       H.head $ do
>         css
>         H.title (H.toHtml title)
>         H.meta ! A.httpEquiv "Content-Type"
>                ! A.content "text/html;charset=utf-8"
>         sequence_ headers
>       H.body $ do
>         H.ul ! A.id "menu" $ do
>          H.li $ H.a ! A.href "/" $ "home"
>          H.li $ H.a ! A.href "/drafts" $ "drafts"
>          H.li $ H.form ! A.enctype "multipart/form-data"
>                        ! A.method "POST"
>                        ! A.action "/new" $ H.button $ "new post"
>         body
>


> -- | CSS for our site
> --
> -- Normally this would live in an external .css file.
> -- It is included inline here to keep the example
> -- self-contained.
> css :: Html
> css =
>  let s = Text.concat
>       [ "body { color: #555; padding: 0; margin: 0; margin-left: 1em;}"
>       , "ul { list-style-type: none; }"
>       , "ol { list-style-type: none; }"
>       , "h1 { font-size: 1.5em; color: #555; margin: 0; }"
>       , ".author { color: #aaa; }"
>       , ".date { color: #aaa; }"
>       , ".tags { color: #aaa; }"
>       , ".post { border-bottom: 1px dotted #aaa; margin-top: 1em; }"
>       , ".bdy  { color: #555; margin-top: 1em; }"
>       , ".post-footer { margin-top: 1em; margin-bottom: 1em; }"
>       , "label { display: inline-block; width: 3em; }"
>       , "#menu { margin: 0; padding: 0; margin-left: -1em;"
>       ,         "border-bottom: 1px solid #aaa; }"
>       , "#menu li { display: inline; margin-left: 1em; }"
>       , "#menu form { display: inline; margin-left: 1em; }"
>       ]
>  in H.style ! A.type_ "text/css" $ H.toHtml s
>

> -- | edit an existing blog post
> edit :: AcidState Blog -> ServerPart Response
> edit acid = do
>  pid   <- PostId <$> lookRead "id"
>  mMsg  <- optional $ lookText "msg"
>  mPost <- query' acid (PostById pid)
>  case mPost of
>   Nothing ->
>    notFound $ template "no such post" [] $
>                 do "Could not find a post with id "
>                    H.toHtml (unPostId pid)
>   (Just p@(Post{..})) -> msum
>     [ do method GET
>          ok $ template "foo" [] $ do
>          case mMsg of
>            (Just msg) | msg == "saved" -> "Changes saved!"
>            _ -> ""
>          H.form ! A.enctype "multipart/form-data"
>                 ! A.method "POST"
>                 ! A.action (H.toValue $ "/edit?id=" ++
>                                  (show $ unPostId pid)) $ do
>            H.label "title" ! A.for "title"
>            H.input ! A.type_ "text"
>                    ! A.name "title"
>                    ! A.id "title"
>                    ! A.size "80"
>                    ! A.value (H.toValue title)
>            H.br
>            H.label "author" ! A.for "author"
>            H.input ! A.type_ "text"
>                    ! A.name "author"
>                    ! A.id "author"
>                    ! A.size "40"
>                    ! A.value (H.toValue author)
>            H.br
>            H.label "tags" ! A.for "tags"
>            H.input ! A.type_ "text"
>                    ! A.name "tags"
>                    ! A.id "tags"
>                    ! A.size "40"
>                    ! A.value (H.toValue $
>                                Text.intercalate ", " tags)
>            H.br
>            H.label "body" ! A.for "body"
>            H.br
>            H.textarea ! A.cols "80"
>                       ! A.rows "20"
>                       ! A.name "body" $ H.toHtml body
>            H.br
>            H.button ! A.name "status"
>                     ! A.value "publish" $ "publish"
>            H.button ! A.name "status"
>                     ! A.value "save"    $ "save as draft"
>     , do method POST
>          ttl   <- lookText' "title"
>          athr  <- lookText' "author"
>          tgs   <- lookText' "tags"
>
>          bdy   <- lookText' "body"
>          now   <- liftIO $ getCurrentTime
>          stts  <- do s <- lookText' "status"
>                      case s of
>                         "save"    -> return Draft
>                         "publish" -> return Published
>                         _         -> mzero
>          let updatedPost =
>                  p { title  = ttl
>                    , author = athr
>                    , body   = bdy
>                    , date   = now
>                    , status = stts
>                    , tags   =
>                        map Text.strip $ Text.splitOn "," tgs
>                    }
>          update' acid (UpdatePost updatedPost)
>          case status of
>            Published ->
>              seeOther ("/view?id=" ++ (show $ unPostId pid))
>                       (toResponse ())
>            Draft     ->
>              seeOther ("/edit?msg=saved&id=" ++
>                        (show $ unPostId pid))
>                       (toResponse ())
>                   ]
>
>       where lookText' = fmap toStrict . lookText





> -- | create a new blog post in the database,
> --   and then redirect to /edit
> new :: AcidState Blog -> ServerPart Response
> new acid = do
>   method POST
>   now <- liftIO $ getCurrentTime
>   post <- update' acid (NewPost now)
>   let url = "/edit?id=" ++ show (unPostId $ postId post)
>   seeOther url (toResponse ())

> -- | render a single blog post into an HTML fragment
> postHtml  :: Post -> Html
> postHtml (Post{..}) =
>   H.div ! A.class_ "post" $ do
>     H.h1 $ H.toHtml title
>     H.div ! A.class_ "author" $
>        do "author: "
>           H.toHtml author
>     H.div ! A.class_ "date"   $
>        do "published: "
>           H.toHtml (show date)
>     H.div ! A.class_ "tags"   $
>        do "tags: "
>           H.toHtml (Text.intercalate ", " tags)
>     H.div ! A.class_ "bdy" $ H.toHtml body
>     H.div ! A.class_ "post-footer" $ do
>      H.span $ H.a !
>        A.href (H.toValue $ "/view?id=" ++
>                 show (unPostId postId)) $ "permalink"
>      H.span $ " "
>      H.span $ H.a !
>        A.href (H.toValue $ "/edit?id=" ++
>                 show (unPostId postId)) $ "edit this post"

> -- | view a single blog post
> view :: AcidState Blog -> ServerPart Response
> view acid =
>     do pid <- PostId <$> lookRead "id"
>        mPost <- query' acid (PostById pid)
>        case mPost of
>          Nothing ->
>              notFound $ template "no such post" [] $
>                do "Could not find a post with id "
>                   H.toHtml (unPostId pid)
>          (Just p) ->
>              ok $ template (title p) [] $ do
>                  (postHtml p)

> -- | render all the Published posts (ordered newest to oldest)
> home :: AcidState Blog -> ServerPart Response
> home acid =
>     do published <- query' acid (PostsByStatus Published)
>        ok $ template "home" [] $ do
>          mapM_ postHtml published
>

> -- | show a list of all unpublished blog posts
> drafts :: AcidState Blog -> ServerPart Response
> drafts acid =
>     do drafts <- query' acid (PostsByStatus Draft)
>        case drafts of
>          [] -> ok $ template "drafts" [] $
>                "You have no unpublished posts at this time."
>          _ ->
>              ok $ template "home" [] $
>                  H.ol $ mapM_ editDraftLink drafts
>  where
>   editDraftLink Post{..} =
>     let url = (H.toValue $ "/edit?id=" ++ show (unPostId postId))
>     in H.a ! A.href url $ H.toHtml title

> -- | route incoming requests
> route :: AcidState Blog -> ServerPart Response
> route acid =
>     do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
>        msum [ dir "favicon.ico" $ notFound (toResponse ())
>             , dir "edit"        $ edit acid
>             , dir "new"         $ new acid
>             , dir "view"        $ view acid
>             , dir "drafts"      $ drafts acid
>             , nullDir          >> home acid
>             ]


> -- | start acid-state and the http server
> main :: IO ()
> main =
>     do bracket (openLocalState initialBlogState)
>                (createCheckpointAndClose)
>                (\acid ->
>                     simpleHTTP nullConf (route acid))


Source code for the app is [here](http://srclink/AcidState/IxSet.hs).

