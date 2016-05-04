;; Testing component sequences. These flavors don't *do*
;; anything. Note there is a loop here flav-2 -> flav-5 -> flav-2.
;;
;; flav-1 -> flav-1 flav-2 flav-4 flav-5 flav-3 vanilla-flavor
;;
;; This requires a version of the compiler which can handle multiple
;; modules in one file.

(include-file "include/flavors.lfe")

(defflavor flav-1 () (flav-2 flav-3))
(endflavor flav-1)

(defflavor flav-2 () (flav-4 flav-5))
(endflavor flav-2)

(defflavor flav-3 () (flav-4))
(endflavor flav-3)

(defflavor flav-4 () ())
(endflavor flav-4)

(defflavor flav-5 () (flav-2))
(endflavor flav-5)
