# flavors

<img src="resources/images/flavors-logo.png" />

LFE Flavors package

Implements the Lisp Machine flavors system in/and for LFE.

NOTE: in these descriptions we will not describe the Lisp Machine
flavors. Check here for the [Lisp Machine manual]
(http://bitsavers.trailing-edge.com/pdf/mit/cadr/chinual_6thEd_Jan84/),
our stuff is in chapter 21.

In this version a flavor instance is modelled with a process which is
more LFEy. The instance variables are kept internally in a map. This
means that a flavor instance more like a global object.

Note that sending messages is a synchronous operation. Explicitly
sending a message to yourself is specially handled so it should not
block but sending messages which result in circular calls will
block. This is a general problem of synchronous messages communication
and it still remains.

This model generates 2 modules for each flavor:

- *flav*-flavor-core which contains functions describing all the
  properties of the flavor *flav*. This module is built at compile
  time.

- *flav*-flavor which contains the access functions for which are
  called when sending messages to an instance of this flavor. It is
  built when the first instance of this flavor is made using
  ``make-instance`` or ``flavors:instantiate-flavor``.

The reason for having 2 modules per flavor is that the access function
module will only be built for flavors which actually have instances of
them, so they won't be built for mixins. It also makes it easier to
modify flavors that are being used.

NOTE: no extra .lfe files are actually created. A
*flav*-flavor-core.beam is created directly from the original file
containing the flavor definition but the *flav*-flavor module is
compiled and directly loaded into the system without being written
into a file. This means that all the flavor-core modules are generated
at compile time and put in the ebin directory when the application is
built.

To access the macros do ``(include-file "include/flavors.lfe")`` or
``(include-lib "flavors/include/flavors.lfe")`` if you have the
flavors application in your search path.

The following macros are available for defining flavors and methods:

```lisp
(defflavor <flavor-name> (<var1> <var2> ...) (<flav1> <flav2> ...)
  <opt1> <opt2> ...)

(defmethod (<operation>) <lambda-list> <form1> <form2>)

(defmethod (<operation> <method-type>) <lambda-list> <form1> <form2>)

(endflavor <flavor-name>)               ;Must be last after the methods
```

Alternatively you can use compile time macros without including a
macro definition file:

```lisp
(flavors:defflavor <flavor-name> (<var1> <var2> ...) (<flav1> <flav2> ...)
  <opt1> <opt2> ...)

(flavors:defmethod (<operation>) <lambda-list> <form1> <form2>)

(flavors:defmethod (<operation> <method-type>) <lambda-list> <form1> <form2>)

(flavors:endflavor <flavor-name>)        ;Must be last after the methods
```

Note that the flavor name is no longer given when defining a method as
the method must be defined between the ``defflavor`` and the
``endflavor``, they **cannot** be spread out in the file and
mixed. Also note the order of the method name (operation) and the
method type in the method definition.

The currently supported the options:

- ``gettable-instance-variables``
- ``settable-instance-variables``
- ``inittable-instance-variables``
- ``init-keywords``
- ``required-instance-variables``
- ``required-methods`` (methods given as ``(name arity)``)
- ``required-flavors``
- ``no-vanilla-flavor``
- ``abstract-flavor``

and the standard method types ``before`` and ``after`` for the
daemons.

The ``init/1`` method is also supported and it is automatically called
together with its daemons with the ``init-plist`` as its argument when
an instance is created. There is a predefined method ``terminate/0``
which terminates the instance after calling the method and its before
and after daemons. The default methods for ``init/1`` and
``terminate/0`` do nothing and it is intended for those flavors which
need to use the methods will add daemons rather than redefining them.

For using the flavor definitions and accessing the instances there is:

```lisp
(make-instance <flavor-name> <opt1> <value1> <opt2> <value2> ... )
(flavors:instantiate-flavor <flavor-name> <init-plist>)

(send <object> <operation> <arg1> ...)
```

Alternatively with compile time macros:

```lisp
(flavors:make-instance <flavor-name> <opt1> <value1> <opt2> <value2> ... )

(flavors:instantiate-flavor <flavor-name> <init-plist>)

(flavors:send <object> <operation> <arg1> ...)
```

The variable ``self`` is automatically bound to the actual instance so
it can be passed around. A primary method must now only return the
actual return value which will be returned from sending the
method. The return value of a ``before`` and ``after`` daemon is
ignored. See the example flavors ``f1`` and ``f2``.

To access the instance variables there are two predefined function
``get/1`` and ``set/2``. Calling ``(get 'foo)`` will return the value
of the variable ``foo`` while ``(set 'bar 42)`` sets the value of the
variable ``bar`` to ``42``.

When defining a flavor the component sequence is built as it should be
for the ``before`` and ``after`` daemons. There are rudimentary
``vanilla`` and ``property-list-mixin`` flavors included in the
release.

You can now define many flavors in an LFE file together with other LFE
modules. Compiling the flavor definition file results in the
*flav*-flavor-core.beam file being generated. Local functions used by
the flavor methods can be defined in the same file but they **MUST**
come after the ``defflavor`` definition and before the ``endflavor``.

From the LFE repl do ``(c "f1" '(to-expand return))`` to see the
resultant code generated by the ``defflavor``, ``defmethod`` and
``endflavor`` macros. 

In the ``examples`` directory there is a ``shapes`` sub-directory containing a number of shapes which have the ``shapes`` flavor as a component. These are defined in separate files or in the file ``multi.lfe`` which contains them all. Compiling the file compiles all the shape flavors:

```lisp
> (c "examples/shapes/multi")
(#(module circle-flavor-core)
 #(module rectangle-flavor-core)
 #(module shape-flavor-core))
```

There is also an LFE script ``run-shapes.lfesh`` which can be run from the LFE repl. It creates some instances of the shapes ``rectangle`` and ``circle`` and sends them some messages:

```lisp
> (run "examples/shapes/run-shapes.lfesh")
Drawing rectangle at (10 20), width 5, height 6
Drawing rectangle at (110 120), width 5, height 6
Drawing circle at (15 25), radius 8
Drawing circle at (115 125), radius 8
Drawing rectangle at (0 0), width 30, height 15
#(flavor-instance rectangle rectangle-flavor <0.82.0>)
```

The ``test`` sub-directory contains more flavors to test. For example
the flavors ``f1`` and ``f2`` where ``f2`` is a component of ``f1``,
and the flavors ``foo`` and its components ``foo-base``, ``foo-mixin``
and ``bar-mixin``.

**NOTE**: Using the flavors now requires the latest version of the
compiler which can handle the module not being the same as the file
name. However the .beam still has the same name as the module as it
must.
