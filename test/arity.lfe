;; Testing multiple arity methods
;;
;; This requires a version of the compiler which can handle multiple
;; modules in one file.

(include-file "include/flavors.lfe")

(defflavor arity (c b a)
	   ()
  settable-instance-variables
  )

(defmethod (arity set-b) (x y z)
  (set 'b (tuple x y z)))

(defmethod (arity set-c)
  (['up x y] (set 'c (+ (get 'c) x y)))
  (['down x y] (set 'c (- (get 'c) x y))))

(defmethod (arity before set-c) (x y z)
  (lfe_io:format "before set-c/3 ~p\n" (list (list x y z))))

(defmethod (arity after set-b) (v)
  (lfe_io:format "after set-b/1 ~p\n" (list (list v))))

(defmethod (arity after set-b) (x y z)
  (lfe_io:format "after set-b/3 ~p\n" (list (list x y z))))

(defmethod (arity before set-c) (v)
  (lfe_io:format "before set-c/1 ~p\n" (list (list v))))

(endflavor arity)
