
Using Template Haskell
======================

Template Haskell is a GHC extension that makes it possible to generate new code at compile time. It is like a more powerful version of C macros, but a bit more restrictive than LISP macros. You can see the code that is being generated by passing the `-ddump-splices` flag to GHC.

There are only a few places in Happstack where you will encounter Template Haskell code. In each of those cases, it is used to generate some very boilerplate code. You are already familiar with one code generation mechanism in Haskell -- the `deriving (Eq, Ord, Read, Show, Data, Typeable)` clause. In Happstack, we use Template Haskell in a similar manner to derive instances of classes like `SafeCopy` and `IsAcidic`.

There are only a few simple things you will need to learn to use Template Haskell with Happstack.

To enable Template Haskell you will need to include `{-# LANGUAGE TemplateHaskell #-}` at the top of the file.

Here is an example of some Template Haskell that derives a `SafeCopy` instance:


~~~~ {.haskell}
$(deriveSafeCopy 0 'base ''CounterState)
~~~~


There are three new pieces of syntax you will see:


`$( )`

:    This syntax is used to indicate that the code inside is going to generate code. The `$(..)` will be replaced by the generated code, and then the module will be compiled. The use of `$( )` is optional (since GHC 6.12 or so).

`'`

:    The single quote in `'base` is syntax that returns the `Name` of a function or constructor. (Specificially, `Language.Haskell.TH.Syntax.Name`).

`''`

:    Note: that is two single ticks `''` <i>not</i> a double-quote `"`. It serves the same purpose as `'` except that it is used to get the `Name` of a type instead of a function or constructor.

Finally, you may occasionally run into some staging restrictions. In a
normal Haskell source file, it does not matter what order you declare
things. You can use a type in a type signature, and then define the
type later in the file. However, when using Template Haskell, you may
occasionally find that you need to order your types so that they are
declared before they are used. If the compiler complains that it can't
find a type that you have clearly defined in the file, try moving the
declaration up higher.

That is everything you should need to know to use Template Haskell in
Happstack. See the relevant section of the crash course for the
details of calling specific Template Haskell functions such as
`deriveSafeCopy`.