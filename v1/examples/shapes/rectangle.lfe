(include-file "include/flavors.lfe")

;; Define the rectangle flavor.
(defflavor rectangle (width height)
	   (shape)
  ;; Settables are also gettable and inittable.
  (settable-instance-variables width height))

(defmethod (rectangle draw) ()
  (lfe_io:format "Drawing rectangle at (~p ~p), width ~p, height ~p~n"
		 (list (mref self 'x) (mref self 'y)
		       (mref self 'width) (mref self 'height)))
  (tuple 'ok self))

(endflavor rectangle)
