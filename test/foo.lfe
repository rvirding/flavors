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
