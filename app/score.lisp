(uiop:define-package :icfpc2021/score
    (:use :cl)
  (:export #:dislikes
           #:distance))

(in-package :icfpc2021/score)

(defun distance (a b)
  (+ (* (- (first a) (first b))
        (- (first a) (first b)))
     (* (- (second a) (second b))
        (- (second a) (second b)))))

(defun dislikes (pose hole)
  (loop :for h :in hole
     :sum (loop :for v :in pose
               :minimizing (distance h v))))
