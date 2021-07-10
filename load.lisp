(unless *load-truename*
  (error "This file should be loaded"))

(load (make-pathname :name "setup"
                     :type "lisp"
                     :directory (append (pathname-directory *load-truename*)
                                        (list "lib"))))

(labels ((%add-local-dir (name)
           (push (make-pathname :directory (append (pathname-directory *load-truename*)
                                                   (list name)))
                 ql:*local-project-directories*)))
  (%add-local-dir "lib")
  (%add-local-dir "app"))

(ql:quickload :aserve)
(ql:quickload :icfpc2021)

;; https://www.urbandictionary.com/define.php?term=locked+and+loaded+and+good+to+go
(format t "Locked and loaded and good to go!~%")
