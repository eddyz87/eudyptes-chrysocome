(uiop:define-package :icfpc2021/svg-drawer
    (:use #:cl #:cl-svg #:icfpc2021/problem-defs)
  (:import-from :metabang-bind
                #:bind)
  (:export #:problem->svg
           #:hole->svg
           #:figure->svg))

(in-package :icfpc2021/svg-drawer)

(defun get-dimentions-from-vertices-list (vertices-list)
  (loop :with max-x := 0
        :with max-y := 0
        :for vertex :in vertices-list
        :for x := (first vertex)
        :for y := (second vertex)
        :when (> x max-x)
          :do (setf max-x x)
        :when (> y max-y)
          :do (setf max-y y)
        :finally (return (cons max-x max-y))))

(defgeneric get-dimentions (obj)
  (:method ((obj problem))
    (bind (((max-x-1 . max-y-1) (get-dimentions (problem-hole obj)))
           ((max-x-2 . max-y-2) (get-dimentions (problem-figure obj))))
      (cons (max max-x-1 max-x-2)
            (max max-y-1 max-y-2))))
  (:method ((obj hole))
    (get-dimentions-from-vertices-list (hole-vertices obj)))
  (:method ((obj figure))
    (get-dimentions-from-vertices-list (figure-vertices obj))))

(defun problem->svg (problem filename)
  (bind (((max-width . max-height) (get-dimentions problem)))
    (with-svg-to-file
        (scene 'svg-1.1-toplevel :height max-height :width max-width)
        (filename :if-exists :supersede)
      (draw-hole scene (problem-hole problem))
      (draw-figure scene (problem-figure problem)))))

(defun hole->svg (hole filename)
  (bind (((max-width . max-height) (get-dimentions hole)))
    (with-svg-to-file
        (scene 'svg-1.1-toplevel :height max-height :width max-width)
        (filename :if-exists :supersede)
      (draw-hole scene hole))))

(defun figure->svg (figure filename)
  (bind (((max-width . max-height) (get-dimentions figure)))
    (with-svg-to-file
        (scene 'svg-1.1-toplevel :height max-height :width max-width)
        (filename :if-exists :supersede)
      (draw-figure scene figure))))

(defun draw-hole (scene hole)
  (loop :for (start end) :on (hole-vertices hole) :by #'cdr
        :do (draw-line-segment scene start end)))

(defun draw-figure (scene figure)
  (loop :for edge :in (figure-edges figure)
        :for start := (nth (first edge) (figure-vertices figure))
        :for end := (nth (second edge) (figure-vertices figure))
        :do (draw-line-segment scene start end :stroke "red")))

(defun draw-line-segment (scene start end &key (stroke "black") (stroke-width 1))
  (let ((group
          (make-group scene (:stroke stroke
                             ;;:fill color :opacity alpha
                             :stroke-width stroke-width
                             ;;:fill-opacity (* alpha 0.6)
                             ;;:stroke-linecap "round"
                             ))))
    (draw group (:line :x1 (first start)
                       :y1 (second start)
                       :x2 (first end)
                       :y2 (second end)))
    (draw group (:circle :cx (first start) :cy (second start) :r 1))
    (draw group (:circle :cx (first end) :cy (second end) :r 1))))

;; (defun draw-text (scene text point)
;;   (text scene (:x (get-x (x point)) :y (get-y (y point))) text))

;; (defun draw-point (scene point)
;;   (draw scene (:circle :cx (get-x (x point)) :cy (get-y (y point)) :r 2)))
 
