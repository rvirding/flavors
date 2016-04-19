(include-file "include/flavors.lfe")

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
