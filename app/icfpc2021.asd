(asdf:defsystem :icfpc2021
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :pathname #p"./"
  :depends-on (:icfpc2021/main
	       :icfpc2021/visualizer))

(register-system-packages :aserve '(:net.aserve :net.html.generator))
(register-system-packages :spatial-trees '(:rectangles))
