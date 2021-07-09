(uiop:define-package :icfpc2021/problem-defs
    (:use #:cl)
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
           #:hole-vertices))

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
