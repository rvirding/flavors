(include-file "include/flavors.lfe")

(defflavor f1 (x y z)
  (f2)
  gettable-instance-variables
  settable-instance-variables
  inittable-instance-variables)

(defmethod (f1 after set-y) (v)
  (lfe_io:format "f1 after set-y ~p\n" (list self))
  self)

(defmethod (f1 before set-a) (v)
  (lfe_io:format "f1 before set-a ~p\n" (list self))
  self)

(defmethod (f1 one) (x y z)
  (tuple (+ x y z) self))

(defmethod (f1 before one) (x y z)
  (lfe_io:format "f1 before one ~p\n" (list self))
  self)

(defmethod (f1 after one) (x y z)
  (lfe_io:format "f1 after one ~p\n" (list self))
  self)

(defmethod (f1 two) (x y z)
  (tuple (* x y z) self))

(defmethod (f1 before two) (x y z)
  (lfe_io:format "f1 before two ~p\n" (list self))
  self)

(endflavor f1)
