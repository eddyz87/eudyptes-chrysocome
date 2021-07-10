(uiop:define-package :icfpc2021/polygon
    (:use :cl
          #:icfpc2021/model
          #:trivia)
  (:import-from :alexandria
                #:rcurry)
  (:import-from #:metabang-bind
		#:bind)
  (:export #:lines-intersect?
           #:point-in-polygon?
           #:pose-in-polygon?
           #:check-solution
           #:rasterize-polygon))

(in-package :icfpc2021/polygon)

(defun lines-intersect? (s1 s2)
  (ematch (list s1 s2)
    ((list (segment :a (point (p-x x1) (p-y y1))
                    :b (point (p-x x2) (p-y y2)))
           (segment :a (point (p-x x3) (p-y y3))
                    :b (point (p-x x4) (p-y y4))))
     (let* ((a1 (- x2 x1))
            (a2 (- y2 y1))
            (b1 (- x4 x3))
            (b2 (- y4 y3))
            (c1 (- x3 x1))
            (c2 (- y3 y1))
            (d (- (* a1 b2)
                  (* a2 b1))))
       (handler-case
           (and (< 0
                   (/ (- (* c1 b2)
                         (* c2 b1))
                      d)
                   1)
                (< 0
                   (/ (- (* a1 c2)
                         (* a2 c1))
                      (- d))
                   1))
         (arithmetic-error () nil))))))

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
         (raster (rasterize-polygon 32 32 poly)))
    (loop
      :with (mx my) := (array-dimensions raster)
      :for y :from (1- my) :downto 0
      :do (loop :for x :below mx
                :do (format t "~A" (if (= 1 (aref raster x y)) "x" ".")))
      :do (format t "~%"))
    (loop
      :for test-case :in points
      :for (x y expected) := test-case
      :for result := (point-in-polygon? (make-point :x x :y y) poly)
      :for raster-result := (= 1 (aref raster x y))
      :unless (eq expected result)
        :do (format t "Bad answer for ~A: ~A~%" test-case result)
      :unless (eq expected raster-result)
        :do (format t "Bad raster answer for ~A: ~A~%" test-case raster-result))))

(defun pose-in-polygon? (vertices edges-list-array poly)
  (and (loop :for v :across vertices
             :always (point-in-polygon? v poly))
       (loop :for edges-list :across edges-list-array
             :for ind :from 0
             :do (loop :for edge :in edges-list
                       :when (> (edge-vertex edge)
                                ind)
                         :do (loop :for i :below (length poly)
                                   :for p1 := (aref poly i)
                                   :for p2 := (aref poly (mod (1+ i) (length poly)))
                                   :when (lines-intersect?
                                          (make-segment
                                           :a (aref vertices ind)
                                           :b (aref vertices (edge-vertex edge)))
                                          (make-segment :a p1 :b p2))
                                     :do (return-from pose-in-polygon? nil)))
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

(defun check-solution (problem vertices)
  (cond
    ((null (same-edge-lengths? vertices (problem-edges problem) (problem-epsilon problem)))
     :length-missmatch)
    ((null (pose-in-polygon? vertices
                              (problem-edges problem)
                              (problem-hole problem)))
     :pose-outside-poly)
    (t :ok)))

(defun rasterize-polygon (max-x max-y poly)
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
                      (dy (- by ay)))
                 (when (<= (min ay by) y (max ay by))
                   (cond ((= dx 0)
                          (push ax intersections))
                         ((= dy 0)
                          ;; no intersection
                          )
                         (t (let ((x (+ ax (* (/ dx dy) (- y ay)))))
                              (push x intersections))))))
           :finally (return (sort intersections #'<)))
    :do (loop :for (l r) :on intersections :by #'cddr
              :when (and l r)
                :do (loop :for x :from (ceiling l) :to (floor r)
                          :do (setf (aref arr x y) 1)))
    :finally (return arr)))
