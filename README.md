# flavors
LFE Flavors package

Implements the Lisp Machine flavors system in/and for LFE.

This is still very experimental and it includes different versions to
test different implementation methods.

### v1

This model has 2 modules for each flavor:

- *flav*-flavor-core which contains functions describing all the
  properties of the flavor *flav*. This module is built at compile
  time.

- *flav*-flavor which contains the access functions for which are
  called when sending messages to an instance of this flavor. It is
  built when the first instance of this flavor is made.

The reason for having 2 modules per flavor is that access function
module will only be built for flavors which actually have instances of
them, so they won't be built for mixins. It also makes it easier to
modify flavors that are being used.

NOTE: no .lfe files are actually created. A *flav*-flavor-core.beam is
created but the *flav*-flavor module is compiled and directly loaded
into the system without being written into a file. This means that all
the flavor-core modules are generated at compile time and put in the
ebin directory when the application is built.
