;; Testing user defined methods shadowing of predefined methods.
;;
;; This requires a version of the compiler which can handle multiple
;; modules in one file.

(include-file "include/flavors.lfe")

(defflavor flav-1 (x a (b 49))
           (flav-2)
  (gettable-instance-variables x a b)
  (settable-instance-variables b)
  )

(defmethod (set-b) (x)                  ;This should shadow the settable
  (set 'b (tuple 'flav-1 x)))

(defmethod (a) ()                       ;This should shadow the gettable
  (tuple 'flav-1 (get 'a)))

(defmethod (x) (m n)                    ;This should not shadow the gettable
  (tuple 'flav-1 (get 'x) m n))

(endflavor flav-1)

(defflavor flav-2 (c b)
           ()
  (settable-instance-variables b)
  )

(defmethod (b) ()
  '42)

(endflavor flav-2)
