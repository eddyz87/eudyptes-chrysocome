(uiop:define-package :icfpc2021/visualizer
    (:use #:cl
	  #:net.html.generator
	  #:net.aserve
	  #:icfpc2021/svg-drawer
	  #:icfpc2021/parse
	  #:icfpc2021/problem-defs
	  )
  (:import-from #:cl-svg)
  (:import-from #:metabang-bind
		#:bind)
  (:import-from #:trivia
		#:ematch)
  (:export #:visualizer-main))

(in-package :icfpc2021/visualizer)

;; (net.aserve:publish-file
;;    :path "/"
;;    :file (asdf:system-relative-pathname 'icfpc2021 "visualizer/index.html")
;;    :content-type "text/html")

(defun visualizer-main (&key (port 8888))
  (publish :path "/" 
	   :content-type "text/html"
	   :function #'problems-page)
  (net.aserve:start :port port))

(defmacro problems-table ()
  (let ((problems-dir (asdf:system-relative-pathname 'icfpc2021 "../problems/")))
    `(html ,@(mapcar #'html-row-for-problem
		     (uiop:directory-files problems-dir)))))

(defun problems-page (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html ((:table :bgcolor "white"
		     :bordercolor "black"
		     :border "1" :cellpadding "5"
		     :cellspacing "1"
		     :text-align "center")
	     (problems-table))))))

(defun html-row-for-problem (problem-file)
  (let* ((problem (parse-json-file problem-file))
	 )
    (ematch problem
      ((problem :hole hole :figure figure)
       `(:tr (:td
	      (:princ ,(hole->svg-string hole)))
	     (:td
	      (:princ ,(figure->svg-string figure))))))))

(defun hole->svg-string (hole)
  (shape->svg-string hole #'icfpc2021/svg-drawer::draw-hole))

(defun figure->svg-string (figure)
  (shape->svg-string figure #'icfpc2021/svg-drawer::draw-figure))

(defmacro with-svg-to-stream ((svg &rest svg-attributes)
                              (stream)
                              &body body)
  `(let ((,svg (cl-svg:make-svg-toplevel ,@svg-attributes)))
     ,@body
     (cl-svg:stream-out ,stream ,svg)))

(defun shape->svg-string (shape draw-func)
  (with-output-to-string (str-stream)
    (bind (((max-width . max-height) (icfpc2021/svg-drawer::get-dimentions shape)))
      (with-svg-to-stream
	  (scene 'cl-svg:svg-1.2-toplevel
		 :height (+ max-height 5)
		 :width (+ max-width 5))
	  (str-stream)
	(funcall draw-func scene shape)))))



