(uiop:define-package :icfpc2021/problem-defs
    (:use #:cl)
  (:import-from #:alexandria)
  (:export #:problem
           #:make-problem
           #:problem-figure
           #:problem-hole
           #:problem-epsilon
           #:figure
           #:make-figure
           #:figure-edges
           #:figure-vertices
           #:hole
           #:make-hole
           #:hole-vertices
	   #:make-saved-solution
	   #:saved-solution-vertices
	   #:saved-solution-dislikes
	   #:saved-solution-solver-info
	   #:saved-solution->ht))

(in-package :icfpc2021/problem-defs)

(defstruct problem
  hole
  figure
  epsilon)

(defstruct hole
  vertices)

(defstruct figure
  edges
  vertices)

(defstruct saved-solution
  vertices
  dislikes
  solver-info)

(defun saved-solution->ht (ss)
  (alexandria:plist-hash-table
   (list "vertices" (saved-solution-vertices ss)
	 "dislikes" (saved-solution-dislikes ss)
	 "solver-info" (saved-solution-solver-info ss))))
