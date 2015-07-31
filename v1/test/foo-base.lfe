(include-file "include/flavors.lfe")

(defflavor foo-base ()
           ()
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables)

(defmethod (foo-base hack) ()
  (lfe_io:format "foo-base, hack ~p\n" (list self))
  (tuple 'foo-base self))

(defmethod (foo-base after hack) ()
  (lfe_io:format "foo-base, hack after ~p\n" (list self))
  self)

(endflavor foo-base)
