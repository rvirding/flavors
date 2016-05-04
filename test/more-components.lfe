;; Test for ordering flavor components.
;; The ordering should be:
;;
;; pastry -> pastry cinnamon spice apple fruit food
;; pie -> pie pastry cinnamon spice apple fruit food

(include-file "include/flavors.lfe")

(defflavor pie () (pastry apple cinnamon))
(endflavor pie)

(defflavor pastry () (cinnamon apple))
(endflavor pastry)

(defflavor apple () (fruit))
(endflavor apple)

(defflavor cinnamon () (spice))
(endflavor cinnamon)

(defflavor fruit () (food))
(endflavor fruit)

(defflavor spice () (food))
(endflavor spice)

(defflavor food () ())
(endflavor food)

;; Circular ordering

(defflavor a () (b c))
(endflavor a)

(defflavor b () (c))
(endflavor b)

(defflavor c () (a b))
(endflavor c)
