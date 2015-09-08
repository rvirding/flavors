(include-file "include/flavors.lfe")

(defflavor foo (x)
           (foo-mixin foo-base)
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables)

(defmethod (foo before hack) ()
  (lfe_io:format "foo, hack before ~p\n" (list self)))

(defmethod (foo after hack) ()
  (lfe_io:format "foo, hack after ~p\n" (list self)))

(endflavor foo)

(defflavor foo-base ()
           ()
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables)

(defmethod (foo-base hack) ()
  (lfe_io:format "foo-base, hack ~p\n" (list self))
  'foo-base)

(defmethod (foo-base after hack) ()
  (lfe_io:format "foo-base, hack after ~p\n" (list self)))

(endflavor foo-base)

(defflavor foo-mixin ()
           (bar-mixin)
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables
  abstract-flavor)

(defmethod (foo-mixin before hack) ()
  (lfe_io:format "foo-mixin, hack before ~p\n" (list self)))

(defmethod (foo-mixin after hack) ()
  (lfe_io:format "foo-mixin, hack after ~p\n" (list self)))

(endflavor foo-mixin)

(defflavor bar-mixin ()
           ()
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables
  abstract-flavor)

(defmethod (bar-mixin before hack) ()
  (lfe_io:format "bar-mixin, hack before ~p\n" (list self)))

(defmethod (bar-mixin hack) ()
  (lfe_io:format "bar-mixin, hack ~p\n" (list self))
  'bar-mixin)

(endflavor bar-mixin)
