(uiop:define-package :icfpc2021/solver
    (:use #:cl #:icfpc2021/model)
    (:import-from :icfpc2021/problem-defs)
    (:import-from :icfpc2021/parse)
    (:import-from :icfpc2021/score)
    (:import-from :icfpc2021/polygon)
    (:import-from :icfpc2021/svg-drawer)
    (:import-from :icfpc2021/http)
    (:import-from :cl-ppcre))

(in-package :icfpc2021/solver)

(defparameter *edge-coef* 0.2)
(defparameter *hole-coef* 0.05)

(defun delta-along-vec (delta p1 p2)
  (let* ((dist (sqrt (dist-square p1 p2))))
    (when (zerop dist)
      (return-from delta-along-vec
        (make-vec :x 0 :y 0)))
    (let ((dx (/ (- (p-x p2) (p-x p1))
                 dist))
          (dy (/ (- (p-y p2) (p-y p1))
                 dist)))
      (make-vec :x (* delta dx)
                :y (* delta dy)))))


(defun add-delta (vertex-vec num-vertex v)
  (setf (aref vertex-vec num-vertex)
        (vec-add (aref vertex-vec num-vertex)
                 v)))

(defun edge-force (p1 p2 orig-dist-square epsilon)
  (let* ((dist-square (dist-square p1 p2))
         (ratio (abs (- (/ dist-square orig-dist-square) 1))))
    (if (<= (* ratio 1000000) epsilon)
        0
        (let ((dist (sqrt dist-square))
              (orig-dist (sqrt orig-dist-square)))
          (* *edge-coef* (- dist orig-dist))))))

(defun points-force (p1 p2 coef)
  (let ((dist (sqrt (dist-square p1 p2))))
    (* coef dist)))

(defun closest-point (p points-vec)
  (loop :with min-i := 0
        :with min-d := (dist-square p (aref points-vec 0))
        :for i :from 1 :below (length points-vec)
        :for dist-square := (dist-square p (aref points-vec i))
        :do (when (< dist-square min-d)
              (setf min-d dist-square)
              (setf min-i i))
        :finally (return min-i)))

(defun iteration (problem solution)
  (let ((forces (make-array (length solution)
                            :initial-element (make-vec :x 0 :y 0))))
    (loop :for i :below (length solution)
          :for p := (aref solution i)
          :for edges := (aref (problem-edges problem) i)
          :do (loop :for edge :in edges
                    :for target := (aref solution (edge-vertex edge))
                    :do (let ((f (edge-force p
                                             target
                                             (edge-len-square edge)
                                             (problem-epsilon problem))))
                          (add-delta forces i (delta-along-vec f p target)))))
    (loop :for hp :across (problem-hole problem)
          :for i := (closest-point hp solution)
          :for f := (points-force hp (aref solution i) *hole-coef*)
          :do (add-delta forces i (delta-along-vec f (aref solution i) hp)))
    (let ((new-solution (make-array (length solution))))
      (loop :for i :below (length solution)
            :do (setf (aref new-solution i)
                      (pv-add (aref solution i)
                              (aref forces i))))
      new-solution)))

(defun solution-dist (sol1 sol2)
  (loop :for i :below (length sol1)
        :sum (dist-square (aref sol1 i) (aref sol2 i))))

(defun round-solution (sol)
  (map 'vector
       (lambda (p)
         (make-point :x (round (p-x p))
                     :y (round (p-y p))))
       sol))

(defun solve (problem &key max-iters svg-prefix (svg-freq 100))
  (let ((solution (problem-init-pos problem)))
    (loop :for iter :from 0
          :for new-solution := (iteration problem solution)
          :do (when (and svg-prefix
                         (= 0 (mod iter svg-freq)))
                (icfpc2021/svg-drawer:problem->svg
                 (solution->parsed-problem problem new-solution)
                 (format nil "~A_~5,'0D.svg" svg-prefix iter)))
              (if (or (< (solution-dist solution new-solution)
                         0.001)
                      (and max-iters
                           (>= iter max-iters)))
                  (return (round-solution new-solution))
                  (setf solution new-solution)))))

(defun solve-file (json-file &key max-iters svg-prefix (svg-freq 100))
  (let* ((p (parsed-problem->problem
             (icfpc2021/parse::parse-json-file json-file)))
         (solution
           (solve p :max-iters max-iters
                    :svg-prefix svg-prefix
                    :svg-freq svg-freq))
         (out-solution (solution->parsed-problem
                        p
                        solution)))
    (assert (icfpc2021/polygon:proper-solution? p solution))
    (values (icfpc2021/problem-defs:figure-vertices
             (icfpc2021/problem-defs:problem-figure out-solution))
            (icfpc2021/score:dislikes
             (icfpc2021/problem-defs:figure-vertices
              (icfpc2021/problem-defs:problem-figure out-solution))
             (icfpc2021/problem-defs:hole-vertices
              (icfpc2021/problem-defs:problem-hole out-solution)))
	    out-solution)))

(defun try-solve-all (dir)
  (loop :for file :in (uiop:directory-files dir)
        :do (multiple-value-bind (solution dislikes)
                (ignore-errors (solve-file file :max-iters 1000))
              (if solution
                  (format t "~A: solved, dislikes: ~A~%" file dislikes)
                  (format t "~A: failed~%" file)))))

(defun file-problem-id (file)
  (cl-ppcre:scan-to-strings "[0-9]+" (pathname-name file)))

(defun try-solve-and-post-all (dir)
  (loop :for file :in (directory dir)
        :do (multiple-value-bind (solution dislikes)
                (ignore-errors (solve-file file :max-iters 1000))
              (when solution
                (format t "Solution found for ~A: ~A~%" (file-problem-id file)
                        dislikes)
                (icfpc2021/http:post-solution (file-problem-id file) solution)))))
