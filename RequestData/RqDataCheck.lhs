
Using `checkRq`
---------------

Sometimes the representation of a value as a request parameter will be
different from the representation required by `Read`. We can use
`checkRq` to lift a custom parsing function into `RqData`.

~~~~ {.haskell}
checkRq :: (Monad m, HasRqData m) => m a -> (a -> Either String b) -> m b
~~~~

In this example we create a type `Vote` with a custom parsing function:

> module Main where
>
> import Control.Applicative ((<$>), (<*>))
> import Happstack.Server
>     ( ServerPart, badRequest
>     , nullConf, ok, simpleHTTP)
> import Happstack.Server.RqData
>     ( RqData, checkRq
>     , getDataFn, look, lookRead)
>
> data Vote = Yay | Nay
>     deriving (Eq, Ord, Read, Show, Enum, Bounded)
>
> parseVote :: String -> Either String Vote
> parseVote "yay" = Right Yay
> parseVote "nay" = Right Nay
> parseVote str   =
>     Left $ "Expecting 'yay' or 'nay' but got: " ++ str
>
> votePart :: ServerPart String
> votePart =
>     do r <- getDataFn (look "vote" `checkRq` parseVote)
>        case r of
>          (Left e) ->
>              badRequest $ unlines e
>          (Right i) ->
>              ok $ "You voted: " ++ show i
>
> main :: IO ()
> main = simpleHTTP nullConf $ votePart

Source code for the app is [here](http://srclink/RequestData/RqDataCheck.hs).


Now if we visit [http://localhost:8000/?vote=yay](http://localhost:8000/?vote=yay), we will get the message:

    You voted: Yay

If we visit [http://localhost:8000/?vote=yes](http://localhost:8000/?vote=yes), we will get the error:

    Expecting 'yay' or 'nay' but got: yes

