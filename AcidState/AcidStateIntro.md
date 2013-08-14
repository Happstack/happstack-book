
`acid-state`
============

`acid-state` is a NoSQL, RAM-cloud, persistent data store. One
attractive feature is that it's designed to store arbitrary Haskell
datatypes and queries are written using plain old Haskell code. This
means you do not have to learn a special query language, or figure out
how to turn your beautiful Haskell datastructures into some limited
set of ints and strings.

`acid-state` and `safecopy` are the successors to the old
`happstack-state` and `happstack-data` libraries. You can learn more
at the [acid-state
homepage](http://acid-state.seize.it/). `acid-state` is now completely
independent from Happstack and can be used with any web
framework. However, Happstack is still committed to the improvement
and promotion of `acid-state`.

Apps written using `happstack-state` can be migrated to use
`acid-state` relatively easily. Details on the process or documented
[here](http://code.google.com/p/happstack/wiki/HapstackStateToAcidState).

How `acid-state` works
----------------------

A very simple way to model a database in Haskell would be to create a
datatype to represent your data and then store that data in a mutable,
global variable, such as a global `IORef`. Then you could just write
normal Haskell functions to query that value and update it. No need to
learn a special query language. No need to marshal your types from
expressive Haskell datatypes to some limited set of types supported by
an external database.

That works great.. as long as your application is only
single-threaded, and as long as it never crashes, and never needs to
be restarted. For a web application, those requires are completely
unacceptable -- but the idea is still appealing. `acid-state` provides
a practical implementation of that idea which actually implements the
ACID guarantees that you may be familiar with from traditional
relational databases such as MySQL, postgres, etc.

In `acid-state` we start by defining a type that represents the state
we wish to store. Then we write a bunch of pure functions that query
that value or which return an updated value. However, we do not call
those functions directly. Instead we keep the value inside an
`AcidState` handle, and we call our functions indirectly by using the
`update` and `query` functions. This allows `acid-state` to
transparently log update events to disk, to ensure that update and
query events run automatically and in isolation, etc. It is allows us
to make remote API calls, and, eventually, replication and
multimaster.

Note that when we say `acid-state` is pure, we are referring
specifically to the fact that the functions we write to perform
updates and queries are pure. `acid-state` itself must do IO in order
to coordinate events from multiple threads, log events to disk,
perform remote queries, etc.

Now that you have a vague idea how `acid-state` works, let's clarify
it by looking at some examples.

