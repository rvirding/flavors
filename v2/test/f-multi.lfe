;; Simple test of defining multiple flavors in one file.
;; Doesn't work yet.

(include-file "include/flavors.lfe")

(defflavor f1 (a b c)
           (f2)
  settable-instance-variables)

(defmethod (f1 bert) (x)
  (f1-local x))

(defun f1-local (x)
  (tuple 'f1 x))

(endflavor f1)

(defflavor f2 (a x y)
           ()
  settable-instance-variables
  abstract-flavor)

(defmethod (f2 sune) (x)
  (f2-local x))

(defun f2-local (x)
  (tuple 'f2 x))

(endflavor f2)
