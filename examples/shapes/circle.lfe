(include-file "include/flavors.lfe")

;; Define the circle flavor.
(defflavor circle (radius)
           (shape)
  ;; Settables are also gettable and inittable.
  (settable-instance-variables radius))

(defmethod (circle draw) ()
  (lfe_io:format "Drawing circle at (~p ~p), radius ~p~n"
                 (list (get 'x) (get 'y) (get 'radius))))

(endflavor circle)
