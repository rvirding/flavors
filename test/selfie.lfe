;; Testing selfie's
;;
;; This requires a version of the compiler which can handle multiple
;; modules in one file.

(include-file "include/flavors.lfe")

(defflavor self-1 (x a b)
           (self-2)
  settable-instance-variables
  )

(endflavor self-1)

(defflavor self-2 (y x)
           ()
  (required-instance-variables a b)
  )

(defmethod (selfie) (x)                 ;We send to ourselves a lot
  (let ((a (send self 'a))
        (b (send self 'b)))
    (send self 'set-x (+ a b x))
    (send self 'x)))

(endflavor self-2)
