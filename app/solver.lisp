(uiop:define-package :icfpc2021/solver
  (:use #:cl
        #:icfpc2021/model
        #:icfpc2021/polygon)
  (:import-from :icfpc2021/problem-defs)
  (:import-from :icfpc2021/parse)
  (:import-from :icfpc2021/score)
  (:import-from :icfpc2021/polygon)
  (:import-from :icfpc2021/svg-drawer)
  (:import-from :icfpc2021/http)
  (:import-from :cl-ppcre)
  (:import-from :alexandria
		#:plist-hash-table))

(in-package :icfpc2021/solver)

(defun delta-along-points (delta p1 p2)
  (let* ((dist (sqrt (dist-square p1 p2))))
    (let ((dx (ignore-errors (/ (- (p-x p2) (p-x p1))
                                dist)))
          (dy (ignore-errors (/ (- (p-y p2) (p-y p1))
                                dist))))
      (if (and dx dy)
          (make-vec :x (* delta dx)
                    :y (* delta dy))
          (make-vec :x 0 :y 0)))))

(defun delta-along-vec (delta vec)
  (let* ((dist (sqrt (+ (* (v-x vec) (v-x vec))
                        (* (v-y vec) (v-y vec))))))
    (let ((dx (ignore-errors (/ (v-x vec) dist)))
          (dy (ignore-errors (/ (v-y vec) dist))))
      (if (and dx dy)
          (make-vec :x (* delta dx)
                    :y (* delta dy))
          (make-vec :x 0 :y 0)))))

(defun add-delta (vertex-vec num-vertex v)
  (setf (aref vertex-vec num-vertex)
        (vec-add (aref vertex-vec num-vertex)
                 v)))

(defun edge-force (p1 p2 &key coef orig-dist-square epsilon)
  (let* ((dist-square (dist-square (round-point p1) (round-point p2)))
         (ratio (abs (- (/ dist-square orig-dist-square) 1))))
    (if (<= (* ratio 1000000) epsilon)
        0
        (let ((dist (sqrt dist-square))
              (orig-dist (sqrt orig-dist-square)))
          (* coef (- dist orig-dist))))))

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

(defun mapc-poly-edges (vertices func)
  (loop
    :for i :below (length vertices)
    :for p1 := (aref vertices i)
    :for p2 := (aref vertices (mod (1+ i) (length vertices)))
    :do (funcall func p1 p2)))

(defun iteration (problem solution forces
                  &key
                    edge-force-coef
                    hole-force-coef
                    to-inner-force)
  ;; (format t "Iteration~%")
  ;; Edge forces
  (mapc-edges
   solution (problem-edges problem)
   (lambda (i1 p1 i2 p2 edge-len-square)
     (declare (ignore i2))
     ;; (format t "Edge ~A, ~A: (~A, ~A)~%" i1 i2 p1 p2)
     (let ((f (edge-force p1
                          p2
                          :orig-dist-square edge-len-square
                          :epsilon (problem-epsilon problem)
                          :coef edge-force-coef)))
       (unless (zerop f)
         ;; (format t "Edge force for ~A: ~A~%" i1 f)
         (add-delta forces i1 (delta-along-points f p1 p2))))))
  ;; Forces tying to hole vertices
  (loop :for hp :across (problem-hole problem)
        :for i := (closest-point hp solution)
        :for f := (points-force hp (aref solution i) hole-force-coef)
        :do (add-delta forces i (delta-along-points f (aref solution i) hp)))
  ;; Forces putting figure inside the hole
  (mapc-edges
   solution (problem-edges problem)
   (lambda (i1 p1 i2 p2 edge-len-square)
     (declare (ignore edge-len-square))
     (mapc-poly-edges
      (problem-hole problem)
      (lambda (hole-p1 hole-p2)
        (when (lines-intersect? (make-segment :a p1 :b p2)
                                (make-segment :a hole-p1 :b hole-p2))
          (let ((dir (make-vec
                      :x (- (p-y hole-p1) (p-y hole-p2))
                      :y (- (p-x hole-p2) (p-x hole-p1)))))
            ;; (format t "Inner force~%")
            (add-delta forces i1 (delta-along-vec to-inner-force dir))
            (add-delta forces i2 (delta-along-vec to-inner-force dir))))))))
  ;; Apply forces
  (let ((new-solution (make-array (length solution))))
    (loop :for i :below (length solution)
          :do (setf (aref new-solution i)
                    (pv-add (aref solution i)
                            (aref forces i))))
    (fix-solution new-solution)
    new-solution))

(defun solution-dist (sol1 sol2)
  (loop :for i :below (length sol1)
        :sum (dist-square (aref sol1 i) (aref sol2 i))))

(defun round-point (p)
  (make-point :x (round (p-x p))
              :y (round (p-y p))))

(defun round-solution (sol)
  (map 'vector
       #'round-point
       sol))

(defun fix-solution (sol)
  (map-into sol
            #'fix-point
            sol))

(defun fix-point (p)
  (if (or (< (abs (p-x p)) 0.00001)
          (< (abs (p-y p)) 0.00001))
      (make-point :x (if (< (abs (p-x p)) 0.00001)
                         0
                         (p-x p))
                  :y (if (< (abs (p-y p)) 0.00001)
                         0
                         (p-y p)))
      p))

(defun solve-phase (problem solution
                    &key
                      hook
                      max-iters svg-prefix (svg-freq 100)
                      edge-force-coef hole-force-coef to-inner-force)
  (loop :for iter :from 0
        :for forces := (make-array (length solution)
                                   :initial-element (make-vec :x 0 :y 0))
        :for new-solution := (iteration problem solution forces
                                        :edge-force-coef edge-force-coef
                                        :hole-force-coef hole-force-coef
                                        :to-inner-force to-inner-force)
        :do (when (and svg-prefix
                       (= 0 (mod iter svg-freq)))
              (icfpc2021/svg-drawer:problem->svg
               (solution->parsed-problem problem new-solution)
               (format nil "~A_~5,'0D.svg" svg-prefix iter)))
            (when hook (funcall hook problem new-solution))
            (if (or (< (solution-dist solution new-solution)
                       0.001)
                    (and max-iters
                         (>= iter max-iters)))
                (return (round-solution new-solution))
                (setf solution new-solution))))

(defun solve-phases (problem &key max-iters svg-prefix (svg-freq 100) phases hook)
  (let ((solution (problem-init-pos problem)))
    (loop :for phase-spec :in phases
          :for phase-num :from 0
          :do (setf solution
                    (apply
                     #'solve-phase problem solution
                     :max-iters max-iters
                     :svg-prefix (when svg-prefix
                                   (format nil "~A_~A" svg-prefix phase-num))
                     :hook hook
                     :svg-freq svg-freq
                     phase-spec)))
    solution))

(defvar *phases-spec*
  (list (list :edge-force-coef 0.05
              :hole-force-coef 0.2
              :to-inner-force 0.5)
        (list :edge-force-coef 0.2
              :hole-force-coef 0
              :to-inner-force 0.5)))

(defvar *max-iters* nil)

(defun solve (problem &key max-iters svg-prefix (svg-freq 100) hook)
  (setf *max-iters* max-iters)
  (solve-phases problem
                :max-iters max-iters
                :svg-prefix svg-prefix
                :svg-freq svg-freq
                :hook hook
                :phases *phases-spec*))

(defun combinations (variants-list)
  (if (cdr variants-list)
      (let ((rest-variants (combinations (cdr variants-list))))
        (alexandria:mappend
         (lambda (elem)
           (mapcar
            (lambda (variant)
              (cons elem variant))
            rest-variants))
         (car variants-list)))
      (mapcar #'list (car variants-list))))

(defun best-solution (problem &key max-iters svg-prefix (svg-freq 100) coef-variants hook)
  (loop :with best-dislikes := nil
        :with best-solution := nil
        :for coefs :in (combinations coef-variants)
        :for iter :from 0
        :do (destructuring-bind (ec1 hc1 ic1 ec2 hc2 ic2)
                coefs
              (let ((solution (ignore-errors
                               (solve-phases
                                problem
                                :max-iters max-iters
                                :svg-prefix (when svg-prefix
                                              (format nil "~A_~A" svg-prefix iter))
                                :svg-freq svg-freq
                                :hook hook
                                :phases (list (list :edge-force-coef ec1
                                                    :hole-force-coef hc1
                                                    :to-inner-force ic1)
                                              (list :edge-force-coef ec2
                                                    :hole-force-coef hc2
                                                    :to-inner-force ic2))))))
                (when (and solution
                           (eq :ok (icfpc2021/polygon:check-solution problem solution)))
                  (let* ((out-solution (solution->parsed-problem
                                        problem
                                        solution))
                         (dislikes (icfpc2021/score:dislikes
                                    (icfpc2021/problem-defs:figure-vertices
                                     (icfpc2021/problem-defs:problem-figure out-solution))
                                    (icfpc2021/problem-defs:hole-vertices
                                     (icfpc2021/problem-defs:problem-hole out-solution)))))
                    (when (or (null best-dislikes)
                              (< dislikes best-dislikes))
                      (setf best-solution solution)
                      (setf best-dislikes dislikes))))))
        :finally (return best-solution)))

(defun solve-file (json-file &key max-iters svg-prefix (svg-freq 100) hook)
  (let* ((p (parsed-problem->problem
             (icfpc2021/parse::parse-json-file json-file)))
         (solution
           ;; (solve p :max-iters max-iters
           ;;          :svg-prefix svg-prefix
           ;;          :svg-freq svg-freq)
           (best-solution p
                          :max-iters max-iters
                          :svg-prefix svg-prefix
                          :svg-freq svg-freq
                          :hook hook
                          :coef-variants '((0.05) (0.2 0.5) (0.5 0.7)
                                           (0.1 0.2 0.3) (0 0.01) (0.5 0.7))))
         (out-solution (when solution
                         (solution->parsed-problem
                          p
                          solution))))
    (unless solution
      (error "Solution not found"))
    (ecase (icfpc2021/polygon:check-solution p solution)
      (:ok nil)
      (:length-missmatch (error "Length missmatch"))
      (:pose-outside-poly (error "Pose outside polygon")))
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
                (ignore-errors (solve-file file :max-iters 100))
              (if solution
                  (format t "~A: solved, dislikes: ~A~%" file dislikes)
                  (format t "~A: failed: ~A~%" file dislikes)))))

(defun file-problem-id (file)
  (cl-ppcre:scan-to-strings "[0-9]+" (pathname-name file)))

(defun try-solve-and-post-all (dir)
  (loop :for file :in (directory dir)
        :do (multiple-value-bind (solution dislikes)
                (ignore-errors (solve-file file :max-iters 100))
              (when solution
                (format t "Solution found for ~A: ~A~%" (file-problem-id file)
                        dislikes)
                (icfpc2021/http:post-solution (file-problem-id file) solution)))))

(defun get-solver-info ()
  (plist-hash-table
   (list "type" "spring"
	 "max-iters" *max-iters*
	 "phases-spec" (format nil "~A" *phases-spec*))))

