(uiop:define-package :icfpc2021/circle
    (:use #:cl #:icfpc2021/model)
  (:export #:make-ring-hash-table
           #:vectors-in-ring
           #:points-in-ring))

(in-package :icfpc2021/circle)

(defun sum-sqr (x y)
  (+ (* x x)
     (* y y)))

(defun build-matrix (r)
  "Accepts radius of the biggest circle possible"
  (let* ((r (1+ r))
         (m (make-array (list r r) :element-type '(unsigned-byte 64))))
    (loop :for x :from 0 :below r
       :do (loop :for y :from 0 :below r
              :do (setf (aref m x y) (sum-sqr x y))))
    m))

(defun matrix->hash-table (m)
  (let ((ht (make-hash-table)))
    (loop :for x :below (array-dimension m 0)
          :do (loop :for y :below (array-dimension m 1)
                    :for r2 := (aref m x y)
                    :do (progn
                          (push (cons x y) (gethash r2 ht))
                          (unless (= y 0)
                            (push (cons x (- y)) (gethash r2 ht)))
                          (unless (= x 0)
                            (push (cons (- x) y) (gethash r2 ht)))
                          (when (and (/= x 0) (/= y 0))
                            (push (cons (- x) (- y)) (gethash r2 ht))))))
    ht))

(defun make-ring-hash-table (r)
  "Accepts radius of the biggest circle possible"
  (matrix->hash-table (build-matrix r)))

(defun vectors-in-ring (r2 eps &optional ht)
  "Accepts R^2 of the middle circle"
  (let* ((min-r2 (ceiling (* r2 (- 1 eps))))
         (max-r2 (floor (* r2 (+ 1 eps))))
         (ht (or ht (make-ring-hash-table (ceiling (sqrt max-r2))))))
    (loop :for r :from min-r2 :to max-r2
          :append (gethash r ht))))

;; TODO: filter by min/max?
(defun points-in-ring (p r2 eps &optional ht)
  (loop :for (x . y) :in (vectors-in-ring r2 eps ht)
     :collect (make-point :x (+ (p-x p) x)
                          :y (+ (p-y p) y))))
