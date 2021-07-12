(uiop:define-package :icfpc2021/model
    (:use :cl)
  (:import-from #:icfpc2021/problem-defs)
  (:export #:point
           #:make-point
           #:p-x
           #:p-y

           #:vec
           #:make-vec
           #:v-x
           #:v-y
           #:vec-add
           #:pv-add

           #:problem
           #:make-problem
           #:problem-hole
           #:problem-edges
           #:problem-init-pos
           #:problem-epsilon
           
           #:edge
           #:make-edge
           #:edge-vertex
           #:edge-len-square

           #:dist-square
           #:make-point-vec
           #:make-vec->list
           #:solution->parsed-problem
           #:parsed-problem->problem

           #:make-segment
           #:segment-a
           #:segment-b
           #:segment

           #:mapc-edges
           #:line
           #:line*
           #:p=
           ))

(in-package :icfpc2021/model)

(defstruct (point (:conc-name p-))
  (x 0)
  (y 0))

(defun point (x y)
  (make-point :x x :y y))

(defmethod print-object ((p point) stream)
  (format stream "<~A, ~A>" (p-x p) (p-y p)))

(defstruct (vec (:conc-name v-))
  (x 0)
  (y 0))

(defstruct problem
  hole     ;; (array point)       -- all vertices of the hole
  edges    ;; (array (list edge)) -- edges[w] = (list (edge :vertex u), (edge :vertex v)) meaning:
           ;;                        there are figure edges (w, u), (w, v)
  init-pos ;; (array point)       -- initial coordinates for figure vertexes like u,v,w
  epsilon)

(defstruct edge
  vertex ;; destination figure vertex index
  len-square)

(defstruct segment
  (a (make-point) :type point)
  (b (make-point) :type point))

(defun line (p1 p2)
  (make-segment :a p1
                :b p2))

(defun line* (x1 y1 x2 y2)
  (line (point x1 y1) (point x2 y2)))

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
  (labels ((%square (x)
             (or (ignore-errors (* x x))
                 0)))
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

(defun vec-add (v1 v2)
  (make-vec :x (+ (v-x v1) (v-x v2))
            :y (+ (v-y v1) (v-y v2))))

(defun pv-add (p v)
  (make-point :x (+ (p-x p) (v-x v))
              :y (+ (p-y p) (v-y v))))

(defun p= (p1 p2)
  (and (= (p-x p1) (p-x p2))
       (= (p-y p1) (p-y p2))))

(defun mapc-edges (vertices edges-list-array func)
  (loop :for i :below (length vertices)
        :for p := (aref vertices i)
        :for edges := (aref edges-list-array i)
        :do (loop :for edge :in edges
                  :do (funcall func
                               i
                               p
                               (edge-vertex edge)
                               (aref vertices (edge-vertex edge))
                               (edge-len-square edge)))))
