(uiop:define-package :icfpc2021/polygon
    (:use :cl
          :icfpc2021/problem-defs)
  (:import-from :alexandria
                #:rcurry)
  (:export #:lines-intersect?
           #:point-in-polygon?
           #:pose-in-polygon?))

(in-package :icfpc2021/polygon)

(defun lines-intersect? (a b)
  (destructuring-bind (((x1 y1) (x2 y2))
                       ((x3 y3) (x4 y4)))
      (list a b)
    (let* ((a1 (- x2 x1))
           (a2 (- y2 y1))
           (b1 (- x4 x3))
           (b2 (- y4 y3))
           (c1 (- x3 x1))
           (c2 (- y3 y1))
           (d (- (* a1 b2)
                 (* a2 b1))))
      (unless (= d 0)
        (and (< 0
                (/ (- (* c1 b2)
                      (* c2 b1))
                   d)
                1)
             (< 0
                (/ (- (* a1 c2)
                      (* a2 c1))
                   (- d))
                1))))))

(defun add-last (poly)
  (nconc (copy-list (last poly)) poly))

(defun point-in-polygon? (point poly)
  (destructuring-bind (px py) point
    (loop :with c := nil
       :for ((ix iy) (jx jy) . nil) :on (add-last poly)
         :while jx
       :when (and (= px ix)
                  (= py iy))
       :return t
       :do (unless (eq (> iy py) (> jy py))
             (let ((slope (- (* (- px ix)
                                (- jy iy))
                             (* (- jx ix)
                                (- py iy)))))
               (cond
                 ((zerop slope)
                  (return t))
                 ((not (eq (< slope 0)
                           (< jy iy)))
                  (setf c (not c))))))
         :finally (return c))))

(defun pose-in-polygon? (pose poly)
  (and (some (rcurry #'point-in-polygon? poly) pose)
       (loop :with loop-poly := (add-last poly)
          :for (p1 p2 . nil) :on (add-last pose)
          :while p2
          :for line := (list p1 p2)
          :when (loop :for (p3 p4 . nil) :on loop-poly
                   :while p4
                   :when (lines-intersect? line (list p3 p4))
                   :return t)
          :return nil
          :finally (return t))))
