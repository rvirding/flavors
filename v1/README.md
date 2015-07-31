### v1

In this version a flavors instance is modelled with a map (it could
just as well be a tuple). This means that a flavor instance is not a
global object but behaves like a "normal" LFE term. If one is updated
then a new one is created and the old one is left unchanged.

This model has 2 modules for each flavor:

- *flav*-flavor-core which contains functions describing all the
  properties of the flavor *flav*. This module is built at compile
  time.

- *flav*-flavor which contains the access functions for which are
  called when sending messages to an instance of this flavor. It is
  built when the first instance of this flavor is made using
  ``make-instance`` or ``instantiate-flavor``.

The reason for having 2 modules per flavor is that access function
module will only be built for flavors which actually have instances of
them, so they won't be built for mixins. It also makes it easier to
modify flavors that are being used.

NOTE: no .lfe files are actually created. A *flav*-flavor-core.beam is
created but the *flav*-flavor module is compiled and directly loaded
into the system without being written into a file. This means that all
the flavor-core modules are generated at compile time and put in the
ebin directory when the application is built.

To access the macros do ``(include-file "include/flavors.lfe")`` or
``(include-lib "flavors/include/flavors.lfe")`` if you have the
flavors application in your search path.

In this simple test the map representing the instance is directly
visible and operations on it are explicitly done. There is no hiding
of the actual implementation. The following macros are available for
defining flavors:

```lisp
(defflavor <flavor-name> (<var1> <var2> ...) (<flav1> <flav2> ...) <opt1> <opt2> ...)
(defmethod (<flavor-name> <operation>) <lambda-list> <form1> <form2>)
(defmethod (<flavor-name> <method-type> <operation>) <lambda-list> <form1> <form2>)
(endflavor <flavor-name>)               ;Must be last after the methods
```

Currently we support the options:

- ``gettable-instance-variables`` completely supported
- ``settable-instance-variables``
- ``inittable-instance-variables``
- ``required-instance-variables`` not yet used
- ``required-methods``
- ``required-flavors``
- ``no-vanilla-flavor``
- ``abstract-flavor``

and the standard method types ``before`` and ``after`` for the
daemons.

For using the flavor definitions there is:

```lisp
(make-instance <flavor-name> <opt1> <value1> <opt2> <value2> ... )
(flavors:instantiate-flavor <flavor-name> <init-plist>)

(send <object> <operation> <arg1> ...)
```

A primary method **MUST** return the tuple ``#(<return-value>
<updated-instance-map>)`` and a daemon methods **MUST** return only
the updated instance map. See the example flavors ``f1`` and ``f2``.

When defining a flavor the component sequence is built as it should be
for the ``before`` and ``after`` daemons and there is a very (very)
rudimentary ``vanilla-flavor``.

You can define one or many flavors in an LFE file either with or
without an LFE module. Flavor definitions cannot be intermixed. If you
don't have any LFE module defined in the file then it can't be
compiled normally as it will generate a compiler/load error. This is
because there will be nothing left for the resultant LFE module and
the compiler can't handle this yet. From the LFE repl do ``(c "f1" '(to_exp return))``.

In the ``test`` directory there are two example flavors, ``f1`` and
``f2``, where ``f2`` is a component of ``f1``. Here is an example of
compiling and using them:

```lisp
> (c "test/f1" '(to_exp return))
((defmodule f1-flavor-core
   (export
    (name 0)
    (instance-variables 0)
    (components 0)
    (options 0)
    (methods 0)
    (daemons 1))
   (export (primary-method 3) (before-daemon 3) (after-daemon 3)))
...
 (defun methods () '(one two))
 (defun daemons (('before) '(set-a one two)) (('after) '(set-y one))))
#(module ())
> (c "test/f2" '(to_exp return))
((defmodule f2-flavor-core
   (export
    (name 0)
    (instance-variables 0)
    (components 0)
    (options 0)
    (methods 0)
    (daemons 1))
   (export (primary-method 3) (before-daemon 3) (after-daemon 3)))
...
 (defun methods () '(three))
 (defun daemons (('before) '(set-y)) (('after) '(set-y))))
#(module ())
> (run "include/flavors.lfe")      ;Import the macro definitions
()
> (set f1 (make-instance 'f1))
((defmodule f1-flavor
   (export
    (name 0)
    (instance-variables 0)
    (combined-methods 0)
    (combined-method 3)))
...
     #(set-b f2 () ())
     #(print-self vanilla-flavor () ())
     #(set vanilla-flavor () ()))))
#M(*flavor-module* f1-flavor a undefined b undefined x undefined
   y undefined z undefined)
> (send f1 'one 12 13 14)
f1 before one #M(*flavor-module* f1-flavor a undefined b undefined
                 x undefined y undefined z undefined)
f1 after one #M(*flavor-module* f1-flavor a undefined b undefined
                x undefined y undefined z undefined)
#(39
  #M(*flavor-module* f1-flavor a undefined b undefined x undefined
     y undefined z undefined))
> (send f1 'set-y 42)
f2 before set-y #M(*flavor-module* f1-flavor a undefined b undefined
                   x undefined y undefined z undefined)
f2 after set-y #M(*flavor-module* f1-flavor a undefined b undefined
                  x undefined y 42 z undefined)
f1 after set-y #M(*flavor-module* f1-flavor a undefined b undefined
                  x undefined y 42 z undefined)
#(ok
  #M(*flavor-module* f1-flavor a undefined b undefined x undefined
     y 42 z undefined))
```

Yes, this is still an experiment and the *-flavor-core* and *-flavor*
modules code are printed out even though the LFE files are never
written.
