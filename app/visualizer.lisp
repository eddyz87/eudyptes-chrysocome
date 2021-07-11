(uiop:define-package :icfpc2021/visualizer
    (:use #:cl
	  #:net.html.generator
	  #:net.aserve
	  #:icfpc2021/svg-drawer
	  #:icfpc2021/parse
	  #:icfpc2021/problem-defs
	  #:icfpc2021/utils)
  (:import-from #:icfpc2021/model
                #:solution->parsed-problem)
  (:import-from #:icfpc2021/solver)
  (:import-from #:icfpc2021/main)
  (:import-from #:icfpc2021/mcts-solver)
  (:import-from #:cl-svg)
  (:import-from #:cl-ppcre)
  (:import-from #:metabang-bind
		#:bind)
  (:import-from #:trivia
		#:ematch)
  (:export #:visualizer-main))

(in-package :icfpc2021/visualizer)

(defparameter *svg-dir* (asdf:system-relative-pathname :icfpc2021 "../svg/"))

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

(defun html-row-for-solution (solution)
  (destructuring-bind (hole-str figure-str solution-str message) solution
    (html (:tr (:td (:princ hole-str))
               (:td (:princ figure-str))
               (:td (:princ solution-str))
               (:td (:princ message))))))

(defun problems-table (solutions)
  (html ((:table :bgcolor "white"
		 :bordercolor "black"
		 :border "1" :cellpadding "5"
		 :cellspacing "1"
		 :text-align "center")
         (mapcar #'html-row-for-solution solutions))))

(defun sort-problems (files)
  (loop :for path :in files
        :for num := (problem-id-from-filename path)
        :if num
          :collect (cons num path) :into num-problems
        :else
          :collect path :into non-num-problems
        :finally (return (append (mapcar #'cdr (sort num-problems #'< :key #'car))
                                 (sort non-num-problems #'string< :key #'namestring)))))

(defun solve-all (solutions &key (hook-iters 100))
  (loop :for problem-file :in (sort-problems (uiop:directory-files (asdf:system-relative-pathname :icfpc2021 "../problems/")))
	:collect (ematch (parse-json-file problem-file)
                   ((problem :hole hole :figure figure)
                    (multiple-value-bind (solution dislikes massacred-problem)
			(ignore-errors
			 ;; (let ((iter 0))
                         ;;   (icfpc2021/solver::solve-file
			 ;;    problem-file
                         ;;    :max-iters 1000
                         ;;    :hook (lambda (problem solution)
                         ;;            (when (zerop (mod (incf iter) hook-iters))
                         ;;              (push (solution->parsed-problem problem solution)
                         ;;                    (gethash (problem-id-from-filename problem-file) solutions))))))
			 ;;
			 (format t "solving ~A~%" problem-file)
			 (icfpc2021/main::solve-file #'icfpc2021/mcts-solver::mcts-solve problem-file))
		      (let ((message (if solution
					 (format nil "~A<br/>Dislikes: ~A~%" problem-file dislikes)
					 (format nil "~A<br/>~%Failed ~A~%" problem-file dislikes))))
			(format t "~A~%" message)
			(list (hole->svg-string hole)
			      (figure->svg-string figure)
			      (if solution
				  (solution->svg-string massacred-problem)
				  (format nil "<a href=\"/~A\">Run</a>" (problem-id-from-filename problem-file)))
			      message)))))))

(defun visualizer-main (&key (port 8888) (hook-iters 100))
  (let ((solutions (make-hash-table)))
    (publish
     :path "/" 
     :content-type "text/html"
     :function (lambda (req ent)
                 (with-http-response (req ent)
                   (with-http-body (req ent)
                     (problems-table (solve-all solutions :hook-iters hook-iters))))))
    (publish-solutions (length (uiop:directory-files (asdf:system-relative-pathname :icfpc2021 "../problems/"))) solutions))
  (net.aserve:start :port port))

(defun publish-solutions (problems solutions)
  (loop :for id :from 1 :to problems
     :do (publish
          :path (format nil "/~A" id)
          :content-type "text/html"
          :function (lambda (req ent)
                      (with-http-response (req ent)
                        (with-http-body (req ent)
                          (loop :for sol :in (reverse (gethash id solutions))
                               :do (html (:div (solution->svg-string sol))))))))))
