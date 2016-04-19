(include-file "include/flavors.lfe")

(defflavor foo (x)
           (foo-mixin foo-base)
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables)

(defmethod (hack before) ()
  (lfe_io:format "foo, hack before ~p\n" (list self)))

(defmethod (hack after) ()
  (lfe_io:format "foo, hack after ~p\n" (list self)))

(endflavor foo)

(defflavor foo-base ()
           ()
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables)

(defmethod (hack) ()
  (lfe_io:format "foo-base, hack ~p\n" (list self))
  'foo-base)

(defmethod (hack after) ()
  (lfe_io:format "foo-base, hack after ~p\n" (list self)))

(endflavor foo-base)

(defflavor foo-mixin ()
           (bar-mixin)
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables
  abstract-flavor)

(defmethod (hack before) ()
  (lfe_io:format "foo-mixin, hack before ~p\n" (list self)))

(defmethod (hack after) ()
  (lfe_io:format "foo-mixin, hack after ~p\n" (list self)))

(endflavor foo-mixin)

(defflavor bar-mixin ()
           ()
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables
  abstract-flavor)

(defmethod (hack before) ()
  (lfe_io:format "bar-mixin, hack before ~p\n" (list self)))

(defmethod (hack) ()
  (lfe_io:format "bar-mixin, hack ~p\n" (list self))
  'bar-mixin)

(endflavor bar-mixin)
