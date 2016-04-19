;; Testing multiple arity methods

(include-file "include/flavors.lfe")

(defflavor arity (c b a)
	   ()
  settable-instance-variables
  )

(defmethod (set-b) (x y z)
  (set 'b (tuple x y z)))

(defmethod (set-c)
  (['up x y] (set 'c (+ (get 'c) x y)))
  (['down x y] (set 'c (- (get 'c) x y))))

(defmethod (set-c before) (x y z)
  (lfe_io:format "before set-c/3 ~p\n" (list (list x y z))))

(defmethod (set-b after) (v)
  (lfe_io:format "after set-b/1 ~p\n" (list (list v))))

(defmethod (set-b after) (x y z)
  (lfe_io:format "after set-b/3 ~p\n" (list (list x y z))))

(defmethod (set-c before) (v)
  (lfe_io:format "before set-c/1 ~p\n" (list (list v))))

(endflavor arity)
