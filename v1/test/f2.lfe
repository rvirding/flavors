(include-file "include/flavors.lfe")

(defflavor f2 (a b)
  ()
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables)

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
