;; Simple test of defining multiple flavors in one file.
;; We use module macro interface instead of including the macro
;; definition file.

(flavors:defflavor f1 (a b c)
                   (f2)
   settable-instance-variables)

(flavors:defmethod (bert) (x)
  (f1-local x))

(defun f1-local (x)
  (tuple 'f1 x))

(flavors:endflavor f1)

(flavors:defflavor f2 (a x y)
                   ()
  settable-instance-variables
  abstract-flavor)

(flavors:defmethod (sune) (x)
  (f2-local x))

(defun f2-local (x)
  (tuple 'f2 x))

(flavors:endflavor f2)
