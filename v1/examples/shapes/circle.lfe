(include-file "include/flavors.lfe")

;; Define the circle flavor.
(defflavor circle (radius)
	   (shape)
  ;; Settables are also gettable and inittable.
  (settable-instance-variables radius))

(defmethod (circle draw) ()
  (lfe_io:format "Drawing circle at (~p ~p), radius ~p~n"
		 (list (mref self 'x) (mref self 'y) (mref self 'radius)))
  (tuple 'ok self))

(endflavor radius)
