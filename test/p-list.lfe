;; Testing the property-list-mixin.

(include-file "include/flavors.lfe")

(defflavor pl1 ()
	   (property-list-mixin)
  )

(endflavor pl1)
