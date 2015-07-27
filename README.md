# flavors
LFE Flavors package

Implements the Lisp Machine flavors system in/and for LFE.

This is still very experimental and it includes different versions to
test different implementation methods.

NOTE: in these descriptions we will not describe the Lisp Machine
flavors. Check here for the [Lisp Machine
manual](http://bitsavers.trailing-edge.com/pdf/mit/cadr/chinual_6thEd_Jan84/),
our stuff is in chapter 21.

### v1

In this version the flavor instance is just a map which behaves just
like any normal map or other data structure. This means that an
instance is not a global object and when one is updated then a new
once created and the old one is there unchanged.

This means that this flavors version is like super records/elixir
structs on steroids and is probably not what most people would expect.

(Yes, **I know** it is written in Erlang but the real one won't be)

### v2

This version will be process based.
