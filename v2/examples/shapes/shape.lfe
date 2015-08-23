(include-file "include/flavors.lfe")

;; Define the shape flavor.
(defflavor shape (x y)
	   ()
  ;; Settables are also gettable and inittable.
  (settable-instance-variables x y))

(defmethod (shape move-to) (new-x new-y)
  (tuple 'ok (mupd self 'x new-x 'y new-y)))

(defmethod (shape r-move-to) (delta-x delta-y)
  (let ((x (mref self 'x))
	(y (mref self 'y)))
    (tuple 'ok (mupd self 'x (+ x delta-x) 'y (+ y delta-y)))))

(defmethod (shape draw) ()
  (lfe_io:format "Drawing shape at (~p,~p)~n"
		 (list (mref self 'x) (mref self 'y)))
  (tuple 'ok self))

(endflavor shape)
