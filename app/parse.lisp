(uiop:define-package :icfpc2021/parse
    (:use #:cl #:anaphora #:icfpc2021/problem-defs)
  (:import-from :yason)
  (:import-from :uiop)
  (:export #:parse-json-string
           #:parse-json-file
	   #:load-saved-solution))

(in-package :icfpc2021/parse)

(defun parse-json-file (file-name)
  (parse-json-string (uiop:read-file-string file-name)))

(defun parse-json-string (str)
  (let* ((ht (yason:parse str))
         (holes-list (gethash "hole" ht))
         (figure-ht (gethash "figure" ht))
         (figure-edges (gethash "edges" figure-ht))
         (figure-vertices (gethash "vertices" figure-ht))
         (epsilon (gethash "epsilon" ht)))
    (make-problem :hole (make-hole :vertices holes-list)
                  :figure (make-figure :edges figure-edges
                                       :vertices figure-vertices)
                  :epsilon epsilon)))

(defun load-saved-solution (file-name)
  (when (probe-file file-name)
    (let ((ht (yason:parse (uiop:read-file-string file-name))))
      (make-saved-solution
       :vertices (gethash "vertices" ht)
       :dislikes (gethash "dislikes" ht)
       :solver-info (gethash "solver-info" ht)))))
