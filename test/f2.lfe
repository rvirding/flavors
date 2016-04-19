(include-file "include/flavors.lfe")

(defflavor f2 ((time (now)) (share 'f2) g q)
           ()
  gettable-instance-variables
  (settable-instance-variables q g)     ;share is not settable
  inittable-instance-variables
  (required-instance-variables y x)
  (required-methods (set-y 1) (set-a 1))
  abstract-flavor)

(defmethod (set-y before) (v)
  (lfe_io:format "f2 before set-y ~p\n" (list self)))

(defmethod (set-y after) (v)
  (lfe_io:format "f2 after set-y ~p\n" (list self)))

(defmethod (three)
  (('x x) (set 'x x))
  (('y y) (set 'y y)))

(endflavor f2)
