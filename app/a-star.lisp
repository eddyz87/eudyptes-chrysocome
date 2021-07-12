(defpackage :icfpc2021/a-star
  (:use #:cl)
  (:import-from #:pileup)
  (:export #:next-states
           #:state-estimate
           #:final-state?
           #:a-star))

(in-package :icfpc2021/a-star)

(defgeneric next-states (state))

(defgeneric state-estimate (state))

(defgeneric final-state? (state))

(defgeneric state-hash-object (state))

(defmethod state-hash-object (state)
  state)

(defun a-star (initial-state &key
                               (predicate #'<)
                               hash-state?
                               (state-hash-test #'eql)
                               timeout-in-seconds
                               exhaustive?)
  (let ((queue (pileup:make-heap predicate :key #'state-estimate))
        (visited (when hash-state?
                   (make-hash-table :test state-hash-test)))
        (stop-time (when timeout-in-seconds
                     (+ (get-internal-run-time)
                        (* timeout-in-seconds
                           internal-time-units-per-second))))
        (best-solution nil)
        (best-estimate nil)
	    (last-n-fixed 0))
    (pileup:heap-insert initial-state queue)
    (loop :for state := (pileup:heap-pop queue)
          :while (and state
                      (or (null stop-time)
                          (< (get-internal-run-time) stop-time)))
          :do (let ()
                ;; ((n-fixed (loop :for v :across
                ;; 			 (icfpc2021/mcts-solver::state-fixed-vertices
                ;; 			  (icfpc2021/mcts-solver::a-star-state-orig-state state))
                ;; 		  :when v :count 1)))
                ;; (when (< last-n-fixed n-fixed)
                ;;   (setf last-n-fixed n-fixed)
                ;;   (format t "f(state) = ~A; her(state) = ~A; dislikes = ~A; n-fixed = ~A; n-free = ~A; samples = ~A~%"
                ;; 	  (state-estimate state)
                ;; 	  (icfpc2021/mcts-solver::her state)
                ;; 	  (icfpc2021/mcts-solver::a-star-state-dislikes state)
                ;; 	  n-fixed
                ;; 	  (- (length (icfpc2021/mcts-solver::state-fixed-vertices
                ;; 		      (icfpc2021/mcts-solver::a-star-state-orig-state state)))
                ;; 	     n-fixed)
                ;; 	  icfpc2021/mcts-solver::*sample-count*))
		        (if (final-state? state)
		            (progn
		              (when (or (null best-solution)
				                (funcall predicate
					                     (state-estimate state)
					                     best-estimate))
			            (setf best-estimate (state-estimate state)
			                  best-solution state))
		              (unless exhaustive?
			            (return-from a-star state)))
                    (loop :for next-state :in (next-states state)
			              :do (unless (and hash-state?
					                       (gethash (state-hash-object next-state)
                                                    visited))
				                (when hash-state?
				                  (setf (gethash (state-hash-object next-state)
						                         visited)
					                    t))
				                (pileup:heap-insert next-state queue))))))
    best-solution))


;; Test

(defstruct test-state
  a
  b
  target)

(defmethod next-states ((state test-state))
  ;; (format t "Next states for ~A~%" state)
  (with-slots (a b target) state
    (let ((a1 (1+ a))
          (b1 (1+ b)))
      (remove nil
              (list
               (when (<= (* a b1) target)
                 (make-test-state :a a :b b1 :target target))
               (when (<= (* a1 b) target)
                 (make-test-state :a a1 :b b :target target)))))))

(defmethod state-estimate ((state test-state))
  (with-slots (a b target) state
    (- target (* a b))))

(defmethod final-state? ((state test-state))
  (with-slots (a b target) state
    (= target (* a b))))
