(include-file "include/flavors.lfe")

(defflavor f2 (g q (time (now)) (share 'f2))
           ()
  gettable-instance-variables
  (settable-instance-variables g q)     ;share is not settable
  inittable-instance-variables
  (required-instance-variables x y)
  (required-methods set-y)
  abstract-flavor)

(defmethod (f2 before set-y) (v)
  (lfe_io:format "f2 before set-y ~p\n" (list self))
  self)

(defmethod (f2 after set-y) (v)
  (lfe_io:format "f2 after set-y ~p\n" (list self))
  self)

(defmethod (f2 three)
  (('x x) (tuple 'ok (mupd self 'x x)))
  (('y y) (tuple 'ok (mupd self 'y y))))

(endflavor f2)
