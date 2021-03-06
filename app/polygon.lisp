(uiop:define-package :icfpc2021/polygon
    (:use :cl
          #:icfpc2021/model
          #:trivia
          #:spatial-trees)
  (:import-from #:rectangles
                #:make-rectangle)
  (:import-from :alexandria
                #:rcurry)
  (:import-from #:metabang-bind
                #:bind)
  (:shadowing-import-from #:spatial-trees
                          #:delete
                          #:search)
  (:export #:lines-intersect?
           #:point-in-polygon?
           #:pose-in-polygon?
           #:check-solution
           #:poly->tree
           #:line-intersect?
           #:rasterize-problem
           #:rasterize-polygon
           #:rasterize-polygon*
           #:visualize-poly
           #:visualize-solution
           #:poly-inner-dir))

(in-package :icfpc2021/polygon)

(defun lines-intersect? (s1 s2)
  (ematch (list s1 s2)
    ((list (segment :a (point (p-x x1) (p-y y1))
                    :b (point (p-x x2) (p-y y2)))
           (segment :a (point (p-x x3) (p-y y3))
                    :b (point (p-x x4) (p-y y4))))
     (handler-case
         (let* ((a1 (- x2 x1))
                (a2 (- y2 y1))
                (b1 (- x4 x3))
                (b2 (- y4 y3))
                (c1 (- x3 x1))
                (c2 (- y3 y1))
                (d (- (* a1 b2)
                      (* a2 b1)))
                (val1 (/ (- (* c1 b2)
                            (* c2 b1))
                         d))
                (val2 (/ (- (* a1 c2)
                            (* a2 c1))
                         (- d))))
           (values
            (and (<= 0 val1 1)
                 (<= 0 val2 1))
            (when (or (= 0 val1)
                      (= 1 val1))
              val1)
            (when (or (= 0 val2)
                      (= 1 val2))
              val2)))
       (arithmetic-error () nil)))))

(deftype poly () '(vector point))

(defun point-in-polygon? (point poly)
  (ematch point
    ((point (p-x px) (p-y py))
     (loop :with c := nil
           :for i :below (length poly)
           :for p1 := (aref poly i)
           :for p2 := (aref poly (mod (1+ i) (length poly)))
           :for ix := (p-x p1)
           :for iy := (p-y p1)
           :for jx := (p-x p2)
           :for jy := (p-y p2)
           :when (and (= px ix)
                      (= py iy))
             :return t
           :do (if (and (>= py (min iy jy))
                        (<  py (max iy jy)))
                   (let ((slope (- (* (- px ix)
                                      (- jy iy))
                                   (* (- jx ix)
                                      (- py iy)))))
                     (cond
                       ((zerop slope)
                        (return t))
                       ((not (eq (< slope 0)
                                 (< jy iy)))
                        (setf c (not c)))))
                   ;; same side of Y or same Y
                   (when (and (= iy jy py)
                              (<= (min ix jx)
                                  px
                                  (max ix jx)))
                     (return t)))
           :finally (return c)))))

(defun point-in-polygon?-test ()
  (let* ((poly (make-point-vec '((00 20) (10 20) (20 10) (30 20) (20 00))))
         (points '(( 00 00 nil)
                   ( 20 00 t)
                   ( 00 20 t)
                   ;;(-10 20 nil)
                   ( 20 07 t)
                   ( 05 20 t)
                   ( 20 20 nil)))
         (raster (rasterize-polygon* poly)))
    (loop :for p :across poly
       :unless (= 1 (aref raster (p-x p) (p-y p)))
       :do (format t "Bad answer for ~A ~A~%" (p-x p) (p-y p)))
    (loop
      :with (mx my) := (array-dimensions raster)
      :for y :from (1- my) :downto 0
      :do (loop :for x :below mx
                :do (format t "~A" (if (= 1 (aref raster x y)) "x" ".")))
      :do (format t "~%"))
    (loop
      :with (mx my) := (array-dimensions raster)
      :for y :below my
      :do (loop :for x :below mx
                :for raster-result := (= 1 (aref raster x y))
                :for analytic-result := (point-in-polygon? (make-point :x x :y y) poly)
                :unless (eq raster-result analytic-result)
                  :do (format t "Divergence at ~A, ~A: ~A vs ~A~%"
                              x y raster-result analytic-result)))
    (loop
      :for test-case :in points
      :for (x y expected) := test-case
      :for result := (point-in-polygon? (make-point :x x :y y) poly)
      :for raster-result := (= 1 (aref raster x y))
      :unless (eq expected result)
        :do (format t "Bad answer for ~A: ~A~%" test-case result)
      :unless (eq expected raster-result)
        :do (format t "Bad raster answer for ~A: ~A~%" test-case raster-result))))

(defun pose-in-polygon? (vertices edges-list-array tree raster)
  (and (loop :for v :across vertices
          :always (= 1 (aref raster (p-x v) (p-y v))))
       (loop :for edges-list :across edges-list-array
          :for ind :from 0
          :do (loop :for edge :in edges-list
                 :when (and (> (edge-vertex edge)
                               ind)
                            (line-intersect?
                             (make-segment
                              :a (aref vertices ind)
                              :b (aref vertices (edge-vertex edge)))
                             tree
                             raster))
                 :do (return-from pose-in-polygon? nil))
          :finally (return t))))

(defun same-edge-lengths? (vertices edge-list-array epsilon)
  (loop :for i :below (length vertices)
        :for p := (aref vertices i)
        :for edges := (aref edge-list-array i)
        :always (loop :for edge :in edges
                      :for target := (aref vertices (edge-vertex edge))
                      :always (let* ((dist-square (dist-square p target))
                                     (orig-dist-square (edge-len-square edge))
                                     (ratio (abs (- (/ dist-square orig-dist-square) 1))))
                                ;; (format t "Edge ~A,~A (~A, ~A), dist-square = ~A, orig = ~A, ratio = ~A~%"
                                ;;         i (edge-vertex edge)
                                ;;         p target
                                ;;         dist-square orig-dist-square (float ratio))
                                (<= (* ratio 1000000) epsilon)))))

(defun check-solution (problem vertices &key
                                          (tree (poly->tree (problem-hole problem)))
                                          (raster (rasterize-problem problem)))
  (cond
    ((null (same-edge-lengths? vertices (problem-edges problem) (problem-epsilon problem)))
     :length-missmatch)
    ((null (pose-in-polygon? vertices
                             (problem-edges problem)
                             tree
                             raster))
     :pose-outside-poly)
    (t :ok)))

(defun rasterize-problem (problem)
  (rasterize-polygon* (problem-hole problem)))

(defun rasterize-polygon* (poly)
  (loop :for p :across poly
     :maximizing (p-x p) :into mx
     :maximizing (p-y p) :into my
     :finally (return (rasterize-polygon (1+ mx) (1+ my) poly))))

(defun rasterize-polygon (max-x max-y poly)
  (rasterize-fast max-x max-y poly))

(defun rasterize-correct (max-x max-y poly)
  (loop :with arr := (make-array (list max-x max-y) :element-type 'bit)
     :for x :from 0 :below max-x
     :do (loop :for y :from 0 :below max-y
            :when (point-in-polygon? (make-point :x x :y y) poly)
            :do (setf (aref arr x y) 1))
     :finally (return arr)))

(defun rasterize-fast (max-x max-y poly)
  (loop
     :with arr := (make-array (list max-x max-y) :element-type 'bit)
     :for y :below max-y
     :for intersections
       := (loop
             :with intersections
             :for i :below (length poly)
             :for a := (aref poly i)
             :for b := (aref poly (mod (1+ i) (length poly)))
             :do (bind (((:structure p- (ax x) (ay y)) a)
                        ((:structure p- (bx x) (by y)) b)
                        (dx (- bx ax))
                        (dy (- by ay))
                        (ind (cond
                               ((= y (min ay by)) :min)
                               ((= y (max ay by)) :max)
                               (t :middle))))
                   (when (<= (min ay by) y (max ay by))
                     (cond ((= dx 0)
                            (push (cons ax ind) intersections))
                           ((= dy 0)
                            ;; no intersection
                            )
                           (t (let ((x (+ ax (* (/ dx dy) (- y ay)))))
                                (push (cons x ind) intersections))))))
             :finally (return (unify-intersections intersections)))
     :do (loop :for (l r) :on intersections :by #'cddr
            :do (loop :for x :from (ceiling l) :to (floor r)
                   :do (setf (aref arr x y) 1)))
     :finally (return arr)))

(defun unify-intersections (intersections)
  (labels ((%unify (intersections paint)
             (ematch intersections
               ((list (cons lx _))
                (list lx))
               ((list* (cons lx :middle) rest)
                (cons lx (%unify rest (not paint))))
               ((guard (list* (cons lx li) (cons rx ri) rest)
                       (eq li ri))
                (if paint
                    (%unify rest paint)
                    (list* lx rx (%unify rest paint))))
               ((list* (cons lx _) (cons rx _) rest)
                (if paint
                    (cons rx (%unify rest (not paint)))
                    (cons lx (%unify rest (not paint)))))
               (nil nil))))
    (%unify (sort intersections #'< :key #'car) nil)))

(defstruct hole-segment
  line
  prev
  next)

(defun poly-hole-segment (poly i)
  (make-hole-segment
   :line (poly-segment poly i)
   :prev (poly-segment poly (1- i))
   :next (poly-segment poly (1+ i))))

(defun hole-segment->bound (hs-or-seg)
  (ematch (if (hole-segment-p hs-or-seg)
              (hole-segment-line hs-or-seg)
              hs-or-seg)
    ((segment :a (point (p-x x1) (p-y y1))
              :b (point (p-x x2) (p-y y2)))
     (make-rectangle :lows (list (min x1 x2)
                                 (min y1 y2))
                     :highs (list (max x1 x2)
                                  (max y1 y2))))))

(defun point-line-side (line point)
  (ematch (cons point line)
    ((cons
      (point (p-x x) (p-y y))
      (segment :a (point (p-x x1) (p-y y1))
               :b (point (p-x x2) (p-y y2))))
     (signum (- (* (- x2 x1) (- y y1))
                (* (- y2 y1) (- x x1)))))))

(defun outer? (side)
  (= side -1))

(defun inner? (side)
  (= side 1))

(defun two-segments-outer-point? (seg1 seg2 p)
  (let ((side1 (point-line-side seg1 p))
        (side2 (point-line-side seg2 p)))
    (cond
      ((or (= 0 side1) (= 0 side2))
       nil)
      ((= side1 side2)
       (outer? side1))
      (t
       (let ((seg2-side (point-line-side seg1 (segment-b seg2))))
         (inner? seg2-side))))))

(defun poly-segment (poly i)
  (line (aref poly (mod i (length poly)))
        (aref poly (mod (1+ i) (length poly)))))

(defun poly->tree (poly)
  (let ((tree (make-spatial-tree :r :rectfun #'hole-segment->bound)))
    (loop :for i :below (length poly)
          :do (insert (poly-hole-segment poly i) tree))
    tree))

(defun line-intersect? (line poly-tree raster)
  (declare (ignorable raster))
  (some (lambda (holy-segment)
          (multiple-value-bind (intersect? line-point? segment-point?)
              (lines-intersect? line (hole-segment-line holy-segment))
            (when intersect?
              (cond (segment-point?
                     (multiple-value-bind (seg1 seg2)
                         (if (= segment-point? 0)
                             (values (hole-segment-prev holy-segment)
                                     (hole-segment-line holy-segment))
                             (values (hole-segment-line holy-segment)
                                     (hole-segment-next holy-segment)))
                       (or (two-segments-outer-point? seg1 seg2 (segment-a line))
                           (two-segments-outer-point? seg1 seg2 (segment-b line)))))
                    (line-point?
                     (or (outer? (point-line-side (hole-segment-line holy-segment)
                                                  (segment-a line)))
                         (outer? (point-line-side (hole-segment-line holy-segment)
                                                  (segment-b line)))))
                    (t t)))
            ;; (if (or line-point? segment-point?)
            ;;     (null (check-line line raster))
            ;;     intersect?)
            ;; (and (not (or line-point? segment-point?))
            ;;      intersect?)
            ))
        (search line poly-tree)))

(defun check-line (line raster)
  (ematch line
    ((segment :a (point (p-x x1) (p-y y1))
              :b (point (p-x x2) (p-y y2)))
     (let ((dx (1+ (abs (- x2 x1))))
           (dy (1+ (abs (- y2 y1)))))
       (cond
         ((= dx 1)
          (loop :for y :from (min y1 y2) :to (max y1 y2)
             :always (= 1 (aref raster x1 y))))
         ((>= dx dy)
          (loop :with sx := (signum (- x2 x1))
             :with sy := (signum (- y2 y1))
             :with y := y1
             :with error := 0
             :for x := x1 :then (+ x sx)
             :while (/= x x2)
             :always (= 1 (aref raster x y))
             :do (incf error dy)
             :when (>= error dx)
             :do (progn
                   (incf y sy)
                   (decf error dx))))
         (t (loop :with sx := (signum (- x2 x1))
               :with sy := (signum (- y2 y1))
               :with x := x1
               :with error := 0
               :for y := y1 :then (+ y sy)
               :while (/= y y2)
               :always (= 1 (aref raster x y))
               :do (incf error dx)
               :when (>= error dy)
               :do (progn
                     (incf x sx)
                     (decf error dy)))))))))

(defun check-mid-point (line raster)
  (ematch line
    ((segment :a (point (p-x x1) (p-y y1))
              :b (point (p-x x2) (p-y y2)))
     (= 0 (aref raster
                (round (/ (+ x1 x2) 2))
                (round (/ (+ y1 y2) 2)))))))

(defun poly-inner-dir (poly)
  ;; (let ((p (point 0 0)))
  ;;   (loop
  ;;     :for i :below (length poly)
  ;;     :for seg := (poly-segment poly i)
  ;;     :for side := (point-line-side seg p)
  ;;     :unless (zerop side)
  ;;       :do (let ((in? (point-in-polygon? p poly)))
  ;;             (return (if in? side (- side))))))
  (let ((max-y (loop :for i :below (length poly)
                     :maximizing (p-y (aref poly i)))))
    (loop :for i :below (length poly)
          :for p := (aref poly i)
          :for next-p := (aref poly (mod (1+ i) (length poly)))
          :for prev-p := (aref poly (mod (1- i) (length poly)))
          :do (when (= (p-y p) max-y)
                (return (point-line-side (line prev-p p) next-p))))))

(defun line-intersect?-test ()
  (let* ((lines '((00 00 00 19 nil)
                  (00 00 00 20 t)
                  (01 19 20 08 nil)
                  (01 19 29 19 t)
                  (10 19 29 19 t)
                  (19 11 21 11 t)
                  (20 00 20 20 t)
                  (00 20 20 00 nil)
                  (01 20 20 00 nil)
                  (00 19 20 00 t)))
         (poly (make-point-vec (reverse '((00 20) (10 20) (20 10) (30 20) (20 00)))))
         (raster (rasterize-polygon 32 32 poly))
         (tree (poly->tree poly)))
    (assert (lines-intersect? (make-segment :a (make-point :x 20 :y 0)
                                            :b (make-point :x 20 :y 20))
                              (make-segment :a (make-point :x 20 :y 10)
                                            :b (make-point :x 30 :y 20))))
    (loop :for i :from 0 :below (length poly)
       :for j := (mod (1+ i) (length poly))
       :unless (or (check-line (make-segment :a (aref poly i)
                                             :b (aref poly j))
                               raster)
                   (check-line (make-segment :a (aref poly j)
                                             :b (aref poly i))
                               raster))
       :do (format t "Bad answer for check-line for ~A~%"
                   (make-segment :a (aref poly i)
                                 :b (aref poly j)))
         (let ((lr (make-array '(32 32) :element-type 'bit))
               (lr2 (make-array '(32 32) :element-type 'bit)))
           (visualize-raster raster)
           (rasterize-line (make-segment :a (aref poly i)
                                         :b (aref poly j))
                           lr)
           (rasterize-line (make-segment :a (aref poly j)
                                         :b (aref poly i))
                           lr2)
           (visualize-raster lr)
           (visualize-raster lr2)))


    (loop :for (x1 y1 x2 y2 expected) :in lines
       :for i :from 0
       :for result := (line-intersect?
                       (make-segment :a (make-point :x x1 :y y1)
                                     :b (make-point :x x2 :y y2))
                       tree
                       raster)
       :unless (eq expected result)
       :do (format t "Bad answer: ~A~%" (nth i lines)))))

(defun visualize-poly (poly)
  (visualize-raster (rasterize-polygon* poly)))

(defun visualize-raster (raster)
  (terpri)
  (loop :with (mx my) := (array-dimensions raster)
     :for y :from 0 :below my
     :do (loop :for x :from 0 :below mx
            :do (princ (if (= 1 (aref raster x y))
                           "x"
                           ".")))
       (terpri)))

(defun visualize-solution (problem solution)
  (let* ((poly (rasterize-polygon* (problem-hole problem)))
         (edges (rasterize-solution (problem-edges problem) solution poly)))
    (terpri)
    (loop :with (mx my) := (array-dimensions poly)
       :for y :from 0 :below my
       :do (loop :for x :from 0 :below mx
              :do (princ (cond
                           ((and (= 1 (aref edges x y))
                                 (= 1 (aref poly x y)))
                            "o")
                           ((= 1 (aref edges x y))
                            "?")
                           ((= 1 (aref poly x y))
                            ".")
                           (t " "))))
         (terpri))))

(defun rasterize-solution (edges-list-array vertices raster)
  (let ((raster (make-array (array-dimensions raster) :element-type 'bit)))
    (loop :for edges-list :across edges-list-array
       :for ind :from 0
       :do (loop :for edge :in edges-list
              :when (> (edge-vertex edge)
                       ind)
              :do (rasterize-line (make-segment
                                   :a (aref vertices ind)
                                   :b (aref vertices (edge-vertex edge)))
                                  raster)))
    raster))

(defun rasterize-line (line raster)
  (ematch line
    ((segment :a (point (p-x x1) (p-y y1))
              :b (point (p-x x2) (p-y y2)))
     (let ((dx (1+ (abs (- x2 x1))))
           (dy (1+ (abs (- y2 y1)))))
       (cond
         ((= x1 x2)
          (loop :for y :from (min y1 y2) :to (max y1 y2)
             :do (setf (aref raster x1 y) 1)))
         ((>= dx dy)
          (loop :with sx := (signum (- x2 x1))
             :with sy := (signum (- y2 y1))
             :with y := y1
             :with error := 0
             :for x := x1 :then (+ x sx)
             :while (/= x x2)
             :do (setf (aref raster x y) 1)
             :do (incf error dy)
             :when (>= error dx)
             :do (progn
                   (incf y sy)
                   (decf error dx))))
         (t (loop :with sx := (signum (- x2 x1))
               :with sy := (signum (- y2 y1))
               :with x := x1
               :with error := 0
               :for y := y1 :then (+ y sy)
               :while (/= y y2)
               :do (setf (aref raster x y) 1)
               :do (incf error dx)
               :when (>= error dy)
               :do (progn
                     (incf x sx)
                     (decf error dy)))))))))
