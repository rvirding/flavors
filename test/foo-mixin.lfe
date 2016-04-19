(include-file "include/flavors.lfe")

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
