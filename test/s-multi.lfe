;; Testing which get/set methods are called.

(include-file "include/flavors.lfe")

(defflavor s1 (a b (c 84) d x)
           (s2)
  (init-keywords k-2 k-4)
  (settable-instance-variables a b d)
  (required-instance-variables y)
  )

(defmethod (s1 set-d) (v)	        ;Setting settable in this flavor
  (set 'd (tuple 's1 v)))

(defmethod (s1 set-y) (v)		;Setting variable in other flavor
  (set 'y (tuple 's1 v)))

(defmethod (s1 set-c) (v)		;Setting settable in other flavor
  (set 'c (tuple 's1 v)))

(endflavor s1)

(defflavor s2 (a (b 42) c y)
           ()
  (init-keywords k-1 k-2 k-3)
  (settable-instance-variables b c)
  (required-instance-variables x)
  abstract-flavor)

(defmethod (s2 set-a) (v)		;Setting settable in other flavor
  (set 'a (tuple 's2 v)))

(defmethod (s2 set-b) (v)	        ;Setting settable in this flavor
  (set 'b (tuple 's2 v)))

(defmethod (s2 set-x) (v)		;Setting variable in other flavor
  (set 'x (tuple 's2 v)))

(defmethod (s2 init) (v)		;Redefining init
  (tuple 's2 v))

(endflavor s2)
