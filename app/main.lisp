(uiop:define-package :icfpc2021/main
    (:use #:cl
          #:icfpc2021/model
          #:icfpc2021/mcts-solver
	  #:icfpc2021/parse
	  #:icfpc2021/utils)
  (:import-from #:icfpc2021/problem-defs
		#:make-saved-solution
		#:saved-solution-dislikes
		#:saved-solution-solver-info
		#:saved-solution->ht)
  (:import-from #:icfpc2021/polygon)
  (:import-from #:icfpc2021/http)
  (:import-from #:icfpc2021/score)
  (:import-from #:icfpc2021/solver)
  (:import-from #:alexandria
		#:plist-hash-table
		#:hash-table-plist))

(in-package :icfpc2021/main)

(defvar *problems-dir*)
(defvar *solutions-dir*)

(defvar *mcts-solver-func* #'icfpc2021/mcts-solver:mcts-solve)
(defvar *spring-solver-func*
  (lambda (problem)
    (icfpc2021/solver::solve problem :max-iters 50)))

(defun main (&key problems-dir solutions-dir (solver :all))
  (let ((*problems-dir* (or problems-dir (dir-pathname "../problems/")))
	(*solutions-dir* (or solutions-dir (dir-pathname "../solutions/")))
	(solvers (ecase solver
		   (:all (list *mcts-solver-func*
			       *spring-solver-func*))
		   (:mcts (list *mcts-solver-func*))
		   (:spring (list *spring-solver-func*)))))
    (format t "Solving problems to find the best solution...~%~%")
    (loop :for problem-file :in (uiop:directory-files *problems-dir*)
          :do (process-problem problem-file solvers))))

(defun solver-info (solver)
  (if (eq solver *mcts-solver-func*)
      (icfpc2021/mcts-solver::get-solver-info)
      (icfpc2021/solver::get-solver-info)))

(defun process-problem (problem-file solvers)
  (let* ((problem-id (problem-id-from-filename problem-file))
	 (solution-file (solution-file-name problem-id *solutions-dir*))
	 (saved-solution (load-saved-solution solution-file)))
    (when saved-solution
      (format t "Loaded saved solution for problem ~A: dislikes = ~A solver-info = ~A~%"
	      problem-id
	      (saved-solution-dislikes saved-solution)
	      (hash-table-plist (saved-solution-solver-info saved-solution))))
    (loop :for solver :in solvers
	  :do (format t "Solving ~A with solver: ~A ~%"
		      problem-id
		      (hash-table-plist (solver-info solver)))
	      (multiple-value-bind (solution dislikes)
		  (ignore-errors (solve-file solver problem-file))
		(if solution
		    (progn
		      (format t "Solution computed for problem ~A: dislikes = ~A~%"
			      problem-id dislikes)
		      (when (or (null saved-solution)
				(< dislikes (saved-solution-dislikes saved-solution)))
			(save-json (saved-solution->ht
				    (make-saved-solution
				     :vertices solution
				     :dislikes dislikes
				     :solver-info (solver-info solver)))
				   solution-file)))
		    (format t "Solution not found~%"))
		))))
			; (icfpc2021/http:post-solution (problem-id-from-filename file) solution)

(defun solve-file (solver json-file)
  (let* ((p (parsed-problem->problem
             (icfpc2021/parse::parse-json-file json-file)))
         (solution (funcall solver p))
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

(defun try-solve-all (solver dir)
  (loop :for file :in (uiop:directory-files dir)
        :do (multiple-value-bind (solution dislikes)
                (ignore-errors (solve-file solver file))
              (if solution
                  (format t "~A: solved, dislikes: ~A~%" file dislikes)
                  (format t "~A: failed: ~A~%" file dislikes)))))

(defun try-solve-and-post-all (solver dir)
  (loop :for file :in (directory dir)
        :do (multiple-value-bind (solution dislikes)
                (ignore-errors (solve-file solver file))
              (when solution
                (format t "Solution found for ~A: ~A~%" (problem-id-from-filename file)
                        dislikes)
                (icfpc2021/http:post-solution (problem-id-from-filename file) solution)))))
