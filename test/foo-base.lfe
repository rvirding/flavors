(include-file "include/flavors.lfe")

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
