
Using `Read`
------------

So far we have only tried to look up `String` values
using `look`. But the `RqData` module provides a
variety of ways to work with values besides `Strings`.


For some types, it is sufficient to use `Read` to parse the `String`
into a value. `RqData` provides functions such as `lookRead` to assist
with this. The advantage of using `lookRead` instead of calling `look`
and applying `read` yourself is that `lookRead` ties into the `RqData`
error handling system neatly.


~~~~ {.haskell}
lookRead :: (Functor m, Monad m, HasRqData m, Read a) => String -> m a
~~~~

Here is a trivial example where we create a `lookInt` function which looks for an `Int` parameter named `int`.


> module Main where
>
> import Control.Applicative ((<$>), (<*>))
> import Happstack.Server (ServerPart, badRequest, nullConf, ok, simpleHTTP)
> import Happstack.Server.RqData (RqData, lookRead, getDataFn)
>
> lookInt :: RqData Int
> lookInt = lookRead "int"
>
> intPart :: ServerPart String
> intPart =
>     do r <- getDataFn lookInt
>        case r of
>          (Left e) ->
>              badRequest $ unlines e
>          (Right i) ->
>              ok $ "Read the int: " ++ show i
>
> main :: IO ()
> main = simpleHTTP nullConf $ intPart


Source code for the app is [here](http://srclink/RqDataRead.hs).


Now if we visit [http://localhost:8000/?int=1](http://localhost:8000/?int=1), we will get the message:


    Read the int: 1


If we visit [http://localhost:8000/?int=apple](http://localhost:8000/?int=apple), we will get the error:

    Read failed while parsing: apple

