# flavors

<img src="resources/images/flavors-logo.png" />

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
one is created and the old one is there unchanged.

This means that this flavors version is like super records/elixir
structs on steroids and is probably not what most people would expect.

(Yes, **I know** it is written in Erlang but the real one won't be)

### v2

This version uses processes to model flavor instances. The flavor
instance variables are kept in a map and the methods explicitly handle
this map. This make the instances behave more like is expected but the
implementation of the methods is still has a very traditional handling
of data.

Note that if you try to send a message to yourself then the instance
process will hang, you after all just sending a synchronous message to
yourself. This could be fixed for the special case of sending messages
to yourself but the general problem that sends are synchronous
messages communication remains.
