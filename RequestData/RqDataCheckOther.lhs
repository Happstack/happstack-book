
Other uses of `checkRq`
-----------------------

Looking again at the type for `checkRq` we see that
function argument is fairly general -- it is not restricted to just
string input:

~~~~ {.haskell}
checkRq :: RqData a -> (a -> Either String b) -> RqData b
~~~~

So, `checkRq` is not limited to just parsing a `String` into a
value. We could use it, for example, to validate an existing value. In
the following example we use `lookRead "i"` to convert the value `i`
to an `Int`, and then we use `checkRq` to ensure that the value is
within range:


> module Main where
>
> import Control.Applicative ((<$>), (<*>))
> import Happstack.Server (ServerPart, badRequest, nullConf, ok, simpleHTTP)
> import Happstack.Server.RqData (RqData, checkRq, getDataFn, look, lookRead)
>
> data Vote = Yay | Nay deriving (Eq, Ord, Read, Show, Enum, Bounded)
>
> inRange :: (Show a, Ord a) => a -> a -> a -> Either String a
> inRange lower upper a
>     | lower <= a && a <= upper = Right a
>     | otherwise =
>         Left (show a ++ " is not between " ++ show lower ++ " and " ++ show upper)
>
> oneToTenPart :: ServerPart String
> oneToTenPart =
>     do r <- getDataFn (lookRead "i" `checkRq` (inRange (1 :: Int) 10))
>        case r of
>          (Left e) ->
>              badRequest $ unlines e
>          (Right i) ->
>              ok $ "You picked: " ++ show i
>
> main :: IO ()
> main = simpleHTTP nullConf $ oneToTenPart


Source code for the app is [here](http://srclink/RqDataCheckOther.hs).

Now if we visit ]http://localhost:8000/?i=10](http://localhost:8000/?i=10), we will get the message:


     $ curl http://localhost:8000/?i=10
    You picked: 10


But if we pick an out of range value [http://localhost:8000/?i=113](http://localhost:8000/?i=113), we will get the message:

     $ curl http://localhost:8000/?i=113
    113 is not between 1 and 10

