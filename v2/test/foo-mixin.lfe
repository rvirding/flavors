(include-file "include/flavors.lfe")

(defflavor foo-mixin ()
           (bar-mixin)
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables
  abstract-flavor)

(defmethod (foo-mixin before hack) ()
  (lfe_io:format "foo-mixin, hack before ~p\n" (list self))
  self)

(defmethod (foo-mixin after hack) ()
  (lfe_io:format "foo-mixin, hack after ~p\n" (list self))
  self)

(endflavor foo-mixin)
