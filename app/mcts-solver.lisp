(uiop:define-package :icfpc2021/mcts-solver
  (:use #:cl
        #:icfpc2021/model
        #:icfpc2021/polygon
        #:icfpc2021/circle
        #:icfpc2021/shortest-paths
        #:icfpc2021/mcts)
  (:import-from #:alexandria
		#:plist-hash-table)
  (:import-from #:icfpc2021/utils
		#:dir-pathname)
  (:import-from #:icfpc2021/svg-drawer)
  (:import-from :metabang-bind
                #:bind)
  (:export #:mcts-solve))

(in-package :icfpc2021/mcts-solver)

;; Algorithm:
;; - compute shortest path matrix
;; - Vf := random figure vertex
;; - Vh := random hole vertex
;; - initial state Vf = Vh
;; - state:
;;   - set of already fixed vertexes
;;   - frontier
;;   - current score
;;   - estimate
;; - move:
;;   - pick / pop? a vertex from frontier
;;   - possible locations (L):
;;     1. a full circle -> how many integer coordinates are there?
;;     2. intersection of circles for connected fixed points
;;     Subject of the following constraints:
;;       - for every fixed point F:
;;           euclidian_distance(F, L) <=  graph_shortest_path(F, L)
;;       - L is inside of the polygon

(defvar *problem*)
(defvar *ring-table*)
(defvar *holy-raster*)
(defvar *holy-tree*)
(defvar *distance-matrix*)

(defvar *timeout-in-seconds* 60)
(defvar *exploration-coef* (sqrt 0.5))

(defun mcts-solve (problem &key print-tree)
  (let* ((max-coord (loop :for p :across (problem-hole problem)
                          :maximizing (max (p-x p) (p-y p))))
         (max-r (ceiling (* (sqrt 2) max-coord)))
         (figure-vertices-num (length (problem-init-pos problem)))
         (*problem* problem)
         (*ring-table* (make-ring-hash-table max-r))
         (*holy-raster* (rasterize-polygon max-r max-r (problem-hole problem)))
         (*holy-tree* (poly->tree (problem-hole problem)))
         (*distance-matrix* (make-shortest-paths-matrix problem))
	 (root-state (make-initial-state))
	 (mcts-root
           (nth-value 1 (select-next-move :root-state root-state
                                          :root-player 0
                                          :players-number 1
                                          :max-iters (* 1000 1000)
                                          :timeout-in-seconds *timeout-in-seconds*
                                          :max-selection-depth figure-vertices-num
                                          :exploration-coefficient *exploration-coef*)))
         (best-score/solution nil))

    (when print-tree
      (let ((icfpc2021/mcts::*state->svg-func* #'mcts-state->svg))
	(with-open-file (stream (format nil "~A/mcts.dot" (dir-pathname ".."))
				:direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
	  (print-decision-tree stream mcts-root root-state
			       :exploration-coefficient *exploration-coef*))))

    (mapc-children-actions
     mcts-root
     (lambda (reverse-actions)
       (let ((state (make-initial-state)))
         (when (= (length reverse-actions) figure-vertices-num)
           (loop :for action :in reverse-actions
                 :do (next-state state action))
           (let ((score (aref (estimate-state-rewards state nil) 0)))
             (when (or (null best-score/solution)
                       (< (car best-score/solution) score))
               (setf best-score/solution
                     (cons score (state-fixed-vertices state)))))))))
    (cdr best-score/solution)))

(defstruct state
  fixed-vertices ;; (array (or null point))
  frontier ;; list of indexes
  initial?
  nearest-figure-vertice-dist ;; (array (or nil dist^2))
  )

(defun edges (vertex)
  (aref (problem-edges *problem*) vertex))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *reward-expr*
    `(/ (+
	 ;; share of fixed vertices 0 -> 1
	 (/ (count-if-not #'null (state-fixed-vertices s))
            (length (state-fixed-vertices s)))
	 (/ 1 (1+ dislikes)))
	2)))

(defmethod estimate-state-rewards (s _)
  (declare (ignore _))
  ;; added dist square to the neares
  (let ((dislikes (reduce #'+ (state-nearest-figure-vertice-dist s)
                          :initial-value 0)))
    (macrolet ((%compute () `,*reward-expr*))
	(vector (%compute)
	     
	     ;; (/ (+
	     ;;     ;; share of fixed vertices 0 -> 1
	     ;;     (/ (count-if-not #'null (state-fixed-vertices s))
	     ;;        (length (state-fixed-vertices s)))
	     ;;     (/ 1
	     ;;        (1+ dislikes)))
	     ;;    2)
	     ))))

(defun make-initial-state ()
  (make-state
   :fixed-vertices (make-array (length (problem-init-pos *problem*))
                               :initial-element nil)
   :frontier nil
   :initial? t
   :nearest-figure-vertice-dist (make-array (length (problem-hole *problem*))
                                            :initial-element nil)))

(defmethod next-state (s action)
  (destructuring-bind (vertex . position) action
    (with-slots (fixed-vertices frontier initial? nearest-figure-vertice-dist) s
      (assert (null (aref fixed-vertices vertex)))
      (setf initial? nil)
      (setf (aref fixed-vertices vertex) position)
      (setf frontier (remove-duplicates
                      (remove-if (lambda (i) (aref fixed-vertices i))
                                 (append (mapcar #'edge-vertex (edges vertex))
                                         frontier))))
      (loop
        :for hole-index :from 0
        :for hole-point :across (problem-hole *problem*)
        :for vertex-to-hole-distance := (dist-square hole-point position)
        :when (or (null (aref nearest-figure-vertice-dist hole-index))
                  (< vertex-to-hole-distance
                     (aref nearest-figure-vertice-dist hole-index)))
          :do (setf (aref nearest-figure-vertice-dist hole-index)
                    vertex-to-hole-distance))))
  s)

(defmethod clone-state (s)
  (let ((new-s (copy-state s)))
    (setf (state-fixed-vertices new-s)
          (subseq (state-fixed-vertices s) 0))
    (setf (state-nearest-figure-vertice-dist s)
          (subseq (state-nearest-figure-vertice-dist s) 0))
    new-s))

(defmethod possible-actions (s _)
  (declare (ignore _))
  (if (state-initial? s)
      (vertice-for-holes)
      ;; (select-initial-vertices)
      (with-slots (fixed-vertices frontier) s
        ;; (when frontier
        ;;   (let* ((free-vertex (car frontier))
        ;;          (actions
        ;;            (loop :for position :in (possible-positions fixed-vertices free-vertex)
        ;;                  :collect (cons free-vertex position))))
        ;;     (format t "State ~A: ~A~%" fixed-vertices actions)
        ;;     actions))
        (let* ((vertex-actions
                 (loop
                   :for free-vertex :in frontier
                   :collect (cons free-vertex
                                  (loop :for position :in (possible-positions fixed-vertices free-vertex)
                                        :collect position))))
               (best-action (alexandria:extremum vertex-actions
                                                 #'<
                                                 :key (lambda (action)
                                                        (length (cdr action))))))
          ;; (format t "State ~A: ~A~%" fixed-vertices best-action)
          (mapcar (lambda (act)
                    (cons (car best-action) act))
                  (cdr best-action))))))

(defun same-distance? (d1 d2)
  (<= (* (abs (1- (/ d1 d2)))
         1000000)
      (problem-epsilon *problem*)))

(defun vertice-for-holes ()
  (loop :for hole-point :across (problem-hole *problem*)
     :collect (cons 0 hole-point)))

(defun hole-for-vertices ()
  (loop :with hole-point := (aref (problem-hole *problem*) 0)
     :for vertex :across (problem-init-pos *problem*)
     :collect (cons vertex hole-point)))

(defun select-initial-vertices ()
  (or (loop :with hole := (problem-hole *problem*)
         :for i :from 0 :below (length hole)
         :for hole-i := (aref hole i)
         :for hole-j := (aref hole (mod (1+ i) (length hole)))
         :for dist := (dist-square hole-i hole-j)
         :append (loop :with edges := (problem-edges *problem*)
                    :for edge-i :from 0 :below (length edges)
                    :append (loop :for edge :in (aref edges edge-i)
                               :when (same-distance? dist (edge-len-square edge))
                               :collect (cons edge-i hole-i))))
      (hole-for-vertices)))

(defun possible-positions (fixed-vertices free-vertex)
  (let* ((all-edges (edges free-vertex))
         (edges-to-fixed (remove-if-not (lambda (e) (aref fixed-vertices (edge-vertex e)))
                                        all-edges))
         (fixed-circles (mapcar (lambda (e)
                                  (remove-if-not
                                   (lambda (point)
                                     (or
                                      ;; filter by hole
                                      (and (>= (p-x point) 0)
                                           (>= (p-y point) 0)
                                           (< (p-x point) (array-dimension *holy-raster* 0))
                                           (< (p-y point) (array-dimension *holy-raster* 1))
                                           (= 1 (aref *holy-raster* (p-x point) (p-y point))))
                                      ;; filter by hole intersection
                                      (null (line-intersect? (make-segment :a point
                                                                           :b (aref fixed-vertices (edge-vertex e)))
                                                             *holy-tree*
                                                             *holy-raster*))))
                                   (points-in-ring (aref fixed-vertices (edge-vertex e))
                                                   (edge-len-square e)
                                                   (/ (problem-epsilon *problem*) (* 1000 1000))
                                                   *ring-table*)))
                                edges-to-fixed))
         (possible-locations (intersect-point-sets fixed-circles)))
    (loop
      :with L-index := free-vertex
      :for L :in possible-locations
      :when (loop
              :for F-index :below (length fixed-vertices)
              :for F := (aref fixed-vertices F-index)
              :always (or (null F)
                          (<= (dist-square L F)
                              (square (shortest-path *distance-matrix* L-index F-index)))))
        :collect L)))

(defun square (x)
  (* x x))

(defun intersect-point-sets (sets-as-lists)
  (let ((possible-locations (make-hash-table :test #'equalp))
        (sets-number (length sets-as-lists))
        (result nil))
    (loop :for points :in sets-as-lists
          :do (loop :for p :in points
                    :do (incf (gethash p possible-locations 0))))
    (maphash (lambda (point count)
               (when (= count sets-number)
                 (push point result)))
             possible-locations)
    result))

(defun get-solver-info ()
  (plist-hash-table
   (list "type" "mcts"
	 "timeout" *timeout-in-seconds*
	 "exploration" *exploration-coef*
	 "reward-expr" (format nil "~A" *reward-expr*))))

(defun mcts-state->svg (state filename)
  (labels ((%point->list (p)
	     (when p (list (p-x p) (p-y p)))))
    (let* ((problem (solution->parsed-problem *problem* (problem-init-pos *problem*)))
	   (hole (icfpc2021/problem-defs:problem-hole problem))
	   (vertices (remove nil (map 'list #'%point->list
				      (icfpc2021/mcts-solver::state-fixed-vertices state)))))
      (bind (((max-width . max-height)
	      (bind (((max-x-1 . max-y-1) (icfpc2021/svg-drawer::get-dimentions hole))
		     ((max-x-2 . max-y-2) (icfpc2021/svg-drawer::get-dimentions-from-vertices-list
					   vertices)))
		(cons (max max-x-1 max-x-2)
		      (max max-y-1 max-y-2)))))
	(icfpc2021/svg-drawer::with-svg-to-file
	    (scene 'cl-svg:svg-1.2-toplevel
		   :height (+ max-height 5)
		   :width (+ max-width 5))
	    (filename :if-exists :supersede)
	  (icfpc2021/svg-drawer::draw-hole scene hole)
	  (loop :for edge :in (icfpc2021/problem-defs:figure-edges
			       (icfpc2021/problem-defs:problem-figure problem))
		:for start := (nth (first edge) vertices)
		:for end := (nth (second edge) vertices)
		:when (and start end)
		  :do (icfpc2021/svg-drawer::draw-line-segment scene start end :stroke "red")))))))
