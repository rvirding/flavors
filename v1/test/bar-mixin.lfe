(include-file "include/flavors.lfe")

(defflavor bar-mixin ()
           ()
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables
  abstract-flavor)

(defmethod (bar-mixin before hack) ()
  (lfe_io:format "bar-mixin, hack before ~p\n" (list self))
  self)

(defmethod (bar-mixin hack) ()
  (lfe_io:format "bar-mixin, hack ~p\n" (list self))
  (tuple 'bar-mixin self))

(endflavor bar-mixin)
