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

(defun hole->svg-string (hole)
  (shape->svg-string hole #'icfpc2021/svg-drawer::draw-hole))

(defun figure->svg-string (figure)
  (shape->svg-string figure #'icfpc2021/svg-drawer::draw-figure))

(defun solution->svg-string (solution)
  (shape->svg-string
   solution
   (lambda (scene solution)
     (ematch solution
       ((problem :hole hole :figure figure)
	(icfpc2021/svg-drawer::draw-hole scene hole)
	(icfpc2021/svg-drawer::draw-figure scene figure))))))

;; (net.aserve:publish-file
;;    :path "/"
;;    :file (asdf:system-relative-pathname 'icfpc2021 "visualizer/index.html")
;;    :content-type "text/html")

(defun row-data-for-problem (problem-file)
  (ematch (parse-json-file problem-file)
    ((problem :hole hole :figure figure)
     (multiple-value-bind (solution dislikes massacred-problem)
	 (ignore-errors
	  (icfpc2021/solver::solve-file problem-file
					:max-iters 1000))
       
       (let ((message (if solution
			  (format nil "~A<br/>Dislikes: ~A~%" problem-file dislikes)
			  (format nil "~A<br/>~%Failed~%" problem-file))))
	 ;;(format t "~A~%" message)
	 (values (hole->svg-string hole)
		 (figure->svg-string figure)
		 (when solution (solution->svg-string massacred-problem))
		 message))))))

(defun html-row-for-problem (problem-file)
  (multiple-value-bind (hole-str figure-str solution-str message)
      (row-data-for-problem problem-file)
    (html (:tr (:td (:princ hole-str))
	       (:td (:princ figure-str))
	       (:td (:princ solution-str))
	       (:td (:princ message))))))

(defun problems-table (problems-dir)
  (html ((:table :bgcolor "white"
		 :bordercolor "black"
		 :border "1" :cellpadding "5"
		 :cellspacing "1"
		 :text-align "center")
	 (mapcar #'html-row-for-problem
		 (uiop:directory-files problems-dir)))))

(defun visualizer-main (&key (port 8888))
  (publish
   :path "/" 
   :content-type "text/html"
   :function (lambda (req ent)
	       (with-http-response (req ent)
		 (with-http-body (req ent)
		   (problems-table (asdf:system-relative-pathname 'icfpc2021 "../problems/"))))))
  (net.aserve:start :port port))

