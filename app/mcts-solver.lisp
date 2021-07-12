(uiop:define-package :icfpc2021/mcts-solver
  (:use #:cl
        #:icfpc2021/model
        #:icfpc2021/polygon
        #:icfpc2021/circle
        #:icfpc2021/shortest-paths
        #:icfpc2021/mcts
        #:alexandria)
  (:import-from #:alexandria
		#:plist-hash-table)
  (:import-from #:icfpc2021/utils
		#:dir-pathname)
  (:import-from #:icfpc2021/svg-drawer)
  (:import-from :metabang-bind
                #:bind)
  (:import-from #:icfpc2021/a-star)
  (:export #:mcts-solve
           #:a-star-solve
           #:a-star/mcts-solve
           #:a-star/mcts-solver-info
	   #:*a-star-exhaustive?*
           #:a-star-anneal-solve))

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
(defvar *complete/total-estimates*)

(defparameter *timeout-in-seconds* 300)
(defparameter *exploration-coef* (sqrt 0.5))

(defparameter *a-star-exhaustive?* t)

(defun exec-with-problem-context (problem fn)
  (let* ((max-coord (loop :for p :across (problem-hole problem)
                          :maximizing (max (p-x p) (p-y p))))
         (max-r (ceiling (* (sqrt 2) max-coord)))
         (*problem* problem)
         (*ring-table* (make-ring-hash-table max-r))
         (*holy-raster* (rasterize-polygon* (problem-hole problem)))
         (*holy-tree* (poly->tree (problem-hole problem)))
         (*distance-matrix* (make-shortest-paths-matrix problem))
         (*complete/total-estimates* (cons 0 0)))
    (funcall fn)))

(defmacro with-problem-context (problem &body body)
  `(exec-with-problem-context
    ,problem
    (lambda () ,@body)))

(defun mcts-solve (problem &key print-tree)
  (with-problem-context problem
    (mcts-solve-aux
     :figure-vertices-num (length (problem-init-pos problem))
     :root-state (make-initial-state)
     :print-tree print-tree)))

(defun mcts-solve-aux (&key
                         figure-vertices-num
                         root-state
                         print-tree
                         early-exit-treshold)
  (let* ((mcts-root
           (nth-value 1 (select-next-move :root-state root-state
                                          :root-player 0
                                          :players-number 1
                                          :max-iters (* 1000 1000)
                                          :timeout-in-seconds *timeout-in-seconds*
                                          :max-selection-depth figure-vertices-num
                                          :exploration-coefficient *exploration-coef*
                                          :expand-node-exit-treshold early-exit-treshold)))
         (best-score/solution nil))

    (when print-tree
      (let ((icfpc2021/mcts::*state->svg-func* #'mcts-state->svg))
        (with-open-file (stream (format nil "~A/mcts.dot" (dir-pathname ".."))
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (print-decision-tree stream mcts-root root-state
                               :exploration-coefficient *exploration-coef*))))
    ;; (format t "Need solution with ~A steps~%" figure-vertices-num)
    ;; (format t "Complete estimates ratio: ~1,3f~%"
    ;;         (if (/= (cdr *complete/total-estimates*) 0)
    ;;             (/ (car *complete/total-estimates*)
    ;;                (cdr *complete/total-estimates*))
    ;;             0))
    ;; (print-mcts-tree-stats mcts-root)
    (mapc-children-actions
     mcts-root
     (lambda (reverse-actions)
       (let ((state (clone-state root-state)))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *reward-expr* nil))

(defmacro capture-reward-expr (&body body)
  (setf *reward-expr* body)
  `(progn ,@body))

(defun state-dislikes (s)
  (reduce #'+ (state-nearest-figure-vertice-dist s)
          :initial-value 0))

(defmethod estimate-state-rewards (s _)
  (declare (ignore _))
  (capture-reward-expr
    (let ((s (clone-state s)))
      (loop :for actions := (possible-actions s 0)
            :while actions
            :do (next-state s (nth (random (length actions))
                                   actions)))
      (let ((complete? (notany #'null (state-fixed-vertices s)))
            (dislikes (state-dislikes s)))
        (when complete?
          (incf (car *complete/total-estimates*)))
        (incf (cdr *complete/total-estimates*))
        (vector
         (if complete?
             (/ 10 (1+ dislikes))
             0))))))

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
        ;; (loop
        ;;   :for free-vertex :in frontier
        ;;   :append (loop :for position :in (possible-positions fixed-vertices free-vertex)
        ;;                 :collect (cons free-vertex position)))
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
                  (cdr best-action)))
        )))

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
                                     (and
                                      ;; filter by hole
                                      (>= (p-x point) 0)
                                      (>= (p-y point) 0)
                                      (< (p-x point) (array-dimension *holy-raster* 0))
                                      (< (p-y point) (array-dimension *holy-raster* 1))
                                      (= 1 (aref *holy-raster* (p-x point) (p-y point)))))
                                   (points-in-ring (aref fixed-vertices (edge-vertex e))
                                                   (edge-len-square e)
                                                   (/ (problem-epsilon *problem*) (* 1000 1000))
                                                   *ring-table*)))
                                edges-to-fixed))
         (fixed-circles-intersections (intersect-point-sets fixed-circles))
         (possible-locations (loop :for point :in fixed-circles-intersections
                                   :when (loop :for edge :in edges-to-fixed
                                               :for connected-point := (aref fixed-vertices (edge-vertex edge))
                                               :never (line-intersect? (make-segment :a connected-point
                                                                                     :b point)
                                                                       *holy-tree*
                                                                       *holy-raster*))
                                     :collect point))
         (result (loop
                   :with L-index := free-vertex
                   :for L :in possible-locations
                   :when (loop
                           :for F-index :below (length fixed-vertices)
                           :for F := (aref fixed-vertices F-index)
                           :always (or (null F)
                                       (<= (dist-square L F)
                                           (round (square (shortest-path *distance-matrix* L-index F-index))))))
                     :collect L)))
    ;; (format t "Fixed circles: ~A~%" fixed-circles)
    ;; (format t "Fixed circles int: ~A~%" fixed-circles-intersections)
    ;; (format t "Lines: ~A~%" possible-locations)
    ;; (format t "After shortest: ~A~%" result)
    result))

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
   (list :type "mcts"
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

;; A-star stuff

(defstruct a-star-state
  orig-state
  dislikes)

(defmethod icfpc2021/a-star:next-states ((state a-star-state))
  (labels ((%next (action)
             (let ((next-orig-state (next-state
                                     (clone-state
                                      (a-star-state-orig-state state))
                                     action)))
               (make-a-star-state
                :orig-state next-orig-state
                :dislikes (state-dislikes next-orig-state)))))
    (let ((actions (possible-actions (a-star-state-orig-state state) 0)))
      (mapcar #'%next actions))))

(defmethod icfpc2021/a-star:final-state? ((state a-star-state))
  (zerop (count-if #'null (state-fixed-vertices (a-star-state-orig-state state)))))

(defparameter *min-dislikes* (expt 2 31))
(defparameter *max-dislikes* 0)

(defparameter *vertex/dislikes* 0)
(defparameter *sample-count* 0)

(defparameter *with-her* nil)

(defun her (a-star-state)
  (let* ((fixed-vertices (state-fixed-vertices
			  (a-star-state-orig-state a-star-state)))
	 (dislikes (a-star-state-dislikes a-star-state))
	 (n-fixed 0)
	 (n-free 0)
	 (n (length fixed-vertices)))
    
    (loop :for v :across fixed-vertices :do
      (if v
	  (incf n-fixed)
	  (incf n-free)))

    (when (/= 0 n-fixed)
      (setf *min-dislikes* (min *min-dislikes* dislikes))
      (setf *max-dislikes* (max *max-dislikes* dislikes))
      (incf *sample-count*))
    
    
    (cond ((= 0 n-fixed) 0)
	  ((= 0 n-free) 1.0)
	  ((= n n-fixed) 1.0)
	  ;; encourage early exploration
	  ;; by returning randomly high her-coef
	  ;; while n-fixed is small
	  ((< n-fixed (round (/ n 5)))
	   (1+ (random (/ 10.0 n-fixed))))
	  (t
	   ;; basic idea is her-coef changing depending on n-fixed like this
	   ;; https://www.wolframalpha.com/input/?i=plot+%2825-x*0.99%29%2F%2825-x%29+from+1+to+24
	   ;;
	   ;; if a-star became too slow (too many samples visited)
	   ;; then decrease the her-coef so that it approaches -> 1.0
	   (let* ((her-coef (float (/ (- n (* n-fixed 0.965))
				      (- n n-fixed))))
		  (decrease-start-count (* 4 n (expt 10 5)))
		  (intolerable-count (* 2 decrease-start-count)))
	     (cond ((< *sample-count* decrease-start-count)
		    her-coef)
		   ((< *sample-count* intolerable-count)
		    (1+ (* (- her-coef 1)
			   (- 1 (/ (- *sample-count* decrease-start-count)
				   (- intolerable-count decrease-start-count))))))
		   (t 1.0)))))
    ))

(defmethod icfpc2021/a-star:state-estimate ((state a-star-state))
  (round (* (a-star-state-dislikes state)
	    (if *with-her* (her state) 1.0))))

(defun a-star-solve (problem)
  (with-problem-context problem
    (a-star-solve-aux)))

(defun a-star-solve-aux (&key (init-state (make-a-star-state
                                           :orig-state (make-initial-state)
                                           :dislikes most-positive-fixnum)))
  (let* ((solved-state (icfpc2021/a-star:a-star
                        init-state
                        :timeout-in-seconds *timeout-in-seconds*
                        :exhaustive? *a-star-exhaustive?*))
	 (*vertex/dislikes* 0)
	 (*sample-count* 0)
	 (*min-dislikes* (expt 2 31))
	 (*max-dislikes* 0))
    (when solved-state
      (state-fixed-vertices (a-star-state-orig-state solved-state)))))

(defun a-star-solver-info ()
  (plist-hash-table
   (list
    :type "a-star"
    :estimate "dislikes"
    :exhaustive *a-star-exhaustive?*
    :her *with-her*)))

(defun compute-figure-vertex-costs (figure-points hole-points)
  (loop
    :with figure-vertex-costs := (make-array (length figure-points))
    :for hole-point :across hole-points
    :do (loop
          :with min-dist := nil
          :with min-figure-index := nil
          :for figure-index :from 0
          :for figure-point :across figure-points
          :for dist := (dist-square hole-point figure-point)
          :when (or (null min-dist)
                    (< dist min-dist))
            :do (setf min-dist dist
                      min-figure-index figure-index)
          :finally (incf (aref figure-vertex-costs min-figure-index)
                         min-dist))
    :finally (return figure-vertex-costs)))

(defun compute-hole-distances (figure-points hole-points)
  (loop
    :with hole-distances := (make-array (length hole-points) :initial-element nil)
    :for hole-index :from 0
    :for hole-point :across hole-points
    :do (setf (aref hole-distances hole-index)
              (loop
                :for figure-point :across figure-points
                :when figure-point
                  :minimizing (dist-square hole-point figure-point)))
    :finally (return hole-distances)))

(defun collect-vertices-from-point (index edges-array N)
  (loop
    :with queue := (list index)
    :with result := (make-hash-table)
    :while (and queue
                (< (hash-table-count result) N))
    :for i := (pop queue)
    :do (unless (gethash i result)
          (setf (gethash i result) t)
          (setf queue (append queue (mapcar #'edge-vertex
                                            (aref edges-array i)))))
    :finally (return (hash-table-keys result))))

(defun maximal-element-index (array &key (filter (constantly t)))
  (loop
    :with max-index := nil
    :for index :from 0
    :for elt :across array
    :when (and (funcall filter elt index)
               (or (null max-index)
                   (> (aref array index)
                      (aref array max-index))))
      :do (setf max-index index)
    :finally (return max-index)))

(defun collect-frontier (fixed-vertices edges-array)
  (loop
    :with frontier := (make-hash-table)
    :for i :from 0
    :for f :across fixed-vertices
    :for edges := (aref edges-array i)
    :when f
      :do (loop :for e :in edges
                :for ev := (edge-vertex e)
                :unless (aref fixed-vertices ev)
                  :do (setf (gethash ev frontier) t))
    :finally (return (hash-table-keys frontier))))

(defparameter *a-star/mcts-refinement-group-size* 3)

(defun a-star/mcts-solver-info ()
  (plist-hash-table
   (list
    :type "a-star/mcts"
    :a-star-estimate "dislikes"
    :mcts-group-size *a-star/mcts-refinement-group-size*
    :exhaustive *a-star-exhaustive?*
    :her *with-her*)))

(defun compute-solution-cost (fixed-vertices hole)
  (loop :for c :across (compute-hole-distances fixed-vertices hole)
        :when c
          :summing c))

(defun copy-array-set-nils (array nil-indexes)
  (loop :with new-vertices := (subseq array 0)
        :for index :in nil-indexes
        :do (setf (aref new-vertices index) nil)
        :finally (return new-vertices)))

(defun a-star/mcts-solve (problem &key (debug-stream t))
  (let ((figure-vertices-num (length (problem-init-pos problem)))
        (start-time (get-internal-run-time)))
    (with-problem-context problem
      (labels
          ((%refine-once (already-refined fixed-vertices)
             (let* ((old-cost (compute-solution-cost fixed-vertices (problem-hole problem)))
                    (_1 (format debug-stream "Trying to refine solution from ~A~%" old-cost))
                    (vertice-costs (compute-figure-vertex-costs fixed-vertices (problem-hole problem)))
                    (first-refinement-vertice (or (maximal-element-index
                                                   vertice-costs
                                                   :filter (lambda (elt idx)
                                                             (and (not (gethash idx already-refined))
                                                                  (/= 0 elt))))
                                                  (progn
                                                    (format debug-stream "No more refinement points~%")
                                                    (return-from %refine-once fixed-vertices))))
                    (refinement-vertices (collect-vertices-from-point first-refinement-vertice
                                                                      (problem-edges problem)
                                                                      *a-star/mcts-refinement-group-size*))
                    (mcts-fixed-vertices (copy-array-set-nils fixed-vertices refinement-vertices))
                    (_2 (format debug-stream "going to refine vertices: ~A~%" refinement-vertices))
                    (_3 (format debug-stream "frontier: ~A~%" (collect-frontier mcts-fixed-vertices
                                                                     (problem-edges problem))))
                    (mcts-state
                      (make-state
                       :fixed-vertices mcts-fixed-vertices
                       :frontier (collect-frontier mcts-fixed-vertices (problem-edges problem))
                       :nearest-figure-vertice-dist (compute-hole-distances mcts-fixed-vertices
                                                                            (problem-hole problem))))
                    (mcts-solution
                      (let ((*timeout-in-seconds* (max 0
                                                       (- *timeout-in-seconds*
                                                          (floor
                                                           (/ (- (get-internal-run-time)
                                                                 start-time)
                                                              internal-time-units-per-second))))))
                        (format debug-stream "Using MCTS timeout ~A~%" *timeout-in-seconds*)
                        (mcts-solve-aux
                         :figure-vertices-num (length refinement-vertices)
                         :root-state mcts-state
                         :early-exit-treshold 100))))
               (declare (ignore _1 _2 _3))
               (setf (gethash first-refinement-vertice already-refined) t)
               (if mcts-solution
                   (let ((new-cost (compute-solution-cost mcts-solution (problem-hole problem))))
                     (if (< new-cost old-cost)
                         (progn
                           (format debug-stream "MCTS refined to ~A~%" new-cost)
                           mcts-solution)
                         (progn
                           (format debug-stream "Discarding MCTS solution with worse score ~A~%" new-cost)
                           fixed-vertices)))
                   (progn
                     (format debug-stream "MCTS failed to refine~%")
                     fixed-vertices)))))
        (loop
          :with already-refined := (make-hash-table)
          :with fixed-vertices := (or (let ((*timeout-in-seconds* (* *timeout-in-seconds* 0.8)))
                                        (a-star-solve-aux))
                                      (return-from a-star/mcts-solve nil))
          :for i :below figure-vertices-num ;; TODO ???
          :do (setf fixed-vertices (%refine-once already-refined fixed-vertices))
          :finally (return fixed-vertices))))))

;; A-star annealer

(defun sigmoid (x)
  (/ 1
     (1+ (exp (- (* 7 x) 5)))))

(defun a-star-anneal-solve (problem)
  (with-problem-context problem
    (format t "Figure vertices num : ~5A~%" (length (problem-init-pos problem)))
    (format t "Hole vertices num   : ~5A~%" (length (problem-hole problem)))
    (loop
      :with figure-vertices-num := (length (problem-init-pos problem))
      :with edges-array := (problem-edges problem)
      :with hole := (problem-hole problem)
      :with fixed-vertices := (or (a-star-solve-aux)
                                  (return-from a-star-anneal-solve))
      :with current-cost := (compute-solution-cost fixed-vertices hole)
      :with all-time-best := fixed-vertices
      :with all-time-best-cost := current-cost
      :with start-time := (get-internal-run-time)
      :for i :from 0
      :for elapsed-time := (- (get-internal-run-time) start-time)
      :for time-budget-elapsed := (/ elapsed-time
                                     internal-time-units-per-second
                                     *timeout-in-seconds*)
      :while (< time-budget-elapsed 1)
      ;; (expt (cos (/ (* pi time-budget-elapsed) 2)) 1/7)
      :for N := (ceiling (* 0.75 figure-vertices-num
                            (sigmoid time-budget-elapsed)))
      ;; TODO: prioritize costly vertices?
      :for first-refinement-vertice := (random (length fixed-vertices))
      :for refinement-vertices := (collect-vertices-from-point first-refinement-vertice
                                                               (problem-edges problem)
                                                               N)
      :for partially-fixed-vertices := (copy-array-set-nils fixed-vertices refinement-vertices)
      :for partial-state
        := (make-state
            :fixed-vertices partially-fixed-vertices
            :frontier (collect-frontier partially-fixed-vertices edges-array)
            :nearest-figure-vertice-dist (compute-hole-distances partially-fixed-vertices hole))
      :for step-start-time := (get-internal-run-time)
      :for new-fixed-vertices
        := (let ((*timeout-in-seconds* (ceiling
                                        (min (/ *timeout-in-seconds* 20)
                                             (* *timeout-in-seconds*
                                                (- 1 time-budget-elapsed))))))
             (a-star-solve-aux
              :init-state (make-a-star-state
                           :orig-state partial-state
                           :dislikes (compute-solution-cost partially-fixed-vertices hole))))
      :for step-stop-time := (get-internal-run-time)
      :for new-fixed-vertices-cost := (if new-fixed-vertices
                                          (compute-solution-cost new-fixed-vertices hole)
                                          most-positive-fixnum)
      :for allow-backwards-move := (< (random 100)
                                      (* 25 (- 1 time-budget-elapsed)))
      :for time-improved? := (<= new-fixed-vertices-cost
                                 current-cost)
      :do (format t "i: ~5A N: ~5A v: ~5A old cost: ~10A new cost: ~10A elapsed: ~5,2fs  time left: ~5,2f ~A~%"
                  i N first-refinement-vertice current-cost
                  (when new-fixed-vertices
                    new-fixed-vertices-cost)
                  (/ (- step-stop-time step-start-time)
                     internal-time-units-per-second)
                  (- 1 time-budget-elapsed)
                  (if (and (not time-improved?)
                           allow-backwards-move)
                      "*" ""))
      :do (when (and new-fixed-vertices
                     (or time-improved?
                         allow-backwards-move))
            (setf fixed-vertices  new-fixed-vertices)
            (setf current-cost    new-fixed-vertices-cost))
      :do (when (< new-fixed-vertices-cost
                   all-time-best-cost)
            (setf all-time-best       new-fixed-vertices)
            (setf all-time-best-cost  new-fixed-vertices-cost))
      :finally (return all-time-best))))
