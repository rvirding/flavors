(include-file "include/flavors.lfe")

;; Define the shape flavor.
(defflavor shape (x y)
           ()
  ;; Settables are also gettable and inittable.
  (settable-instance-variables x y))

(defmethod (move-to) (new-x new-y)
  (set 'x new-x)
  (set 'y new-y)
  'ok)

(defmethod (r-move-to) (delta-x delta-y)
  (let ((x (get 'x))
        (y (get 'y)))
    (set 'x (+ x delta-x))
    (set 'y (+ y delta-y))
    'ok))

(defmethod (draw) ()
  (lfe_io:format "Drawing shape at (~p,~p)~n"
                 (list (get 'x) (get 'y))))

(endflavor shape)
