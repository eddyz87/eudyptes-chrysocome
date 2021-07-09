(uiop:define-package :icfpc2021/solver
    (:use #:cl)
    (:import-from :icfpc2021/problem-defs)
    (:import-from :icfpc2021/parse)
    (:import-from :icfpc2021/svg-drawer))

(in-package :icfpc2021/solver)

(defparameter *edge-coef* 0.2)
(defparameter *hole-coef* 0.01)

(defstruct (point (:conc-name p-))
  (x 0)
  (y 0))

(defstruct (vec (:conc-name v-))
  (x 0)
  (y 0))

(defstruct problem
  hole
  edges
  init-pos
  epsilon)

(defstruct edge
  vertex
  len-square)

(defun make-point-vec (list)
  (map 'vector
       (lambda (p)
         (make-point :x (first p) :y (second p)))
       list))

(defun point-vec->list (point-vec)
  (map 'list
       (lambda (p)
         (list (p-x p) (p-y p)))
       point-vec))

(defun dist-square (p1 p2)
  (labels ((%square (x) (* x x)))
    (+ (%square
        (- (p-x p1) (p-x p2)))
       (%square
        (- (p-y p1) (p-y p2))))))

(defun parsed-problem->problem (problem)
  (let* ((hole (make-point-vec
                (icfpc2021/problem-defs:hole-vertices
                 (icfpc2021/problem-defs:problem-hole problem))))
         (epsilon (icfpc2021/problem-defs:problem-epsilon problem))
         (init-pos (make-point-vec
                    (icfpc2021/problem-defs:figure-vertices
                     (icfpc2021/problem-defs:problem-figure problem))))
         (edges (make-array (length init-pos) :initial-element nil)))
    (loop :for (edge-start edge-end) :in (icfpc2021/problem-defs:figure-edges
                                          (icfpc2021/problem-defs:problem-figure problem))
          :for dist-square := (dist-square (aref init-pos edge-start)
                                           (aref init-pos edge-end))
          :do
             (push (make-edge :vertex edge-end :len-square dist-square)
                   (aref edges edge-start))
             (push (make-edge :vertex edge-start :len-square dist-square)
                   (aref edges edge-end)))
    (make-problem :hole hole
                  :edges edges
                  :init-pos init-pos
                  :epsilon epsilon)))

(defun solution->parsed-problem (problem vertex-pos)
  (icfpc2021/problem-defs:make-problem
   :hole (icfpc2021/problem-defs:make-hole
          :vertices (point-vec->list (problem-hole problem)))
   :figure (icfpc2021/problem-defs:make-figure
            :edges (loop :for es :across (problem-edges problem)
                         :for ind :from 0
                         :appending (loop :for e :in es
                                          :when (> (edge-vertex e)
                                                   ind)
                                            :collect (list ind (edge-vertex e))))
            :vertices (point-vec->list vertex-pos))
   :epsilon (problem-epsilon problem)))

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

(defun vec-add (v1 v2)
  (make-vec :x (+ (v-x v1) (v-x v2))
            :y (+ (v-y v1) (v-y v2))))

(defun pv-add (p v)
  (make-point :x (+ (p-x p) (v-x v))
              :y (+ (p-y p) (v-y v))))

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
                 (format nil "~A_~5,'0D.svg" svg-prefix iter))
                (if (or (< (solution-dist solution new-solution)
                           0.01)
                        (and max-iters
                             (>= iter max-iters)))
                    (return (round-solution new-solution))
                    (setf solution new-solution))))))

(defun solve-file (json-file &key max-iters svg-prefix (svg-freq 100))
  (let ((p (parsed-problem->problem
            (icfpc2021/parse::parse-json-file json-file))))
    (solve p :max-iters max-iters
             :svg-prefix svg-prefix
             :svg-freq svg-freq)))
