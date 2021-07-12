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
		#:saved-solution-vertices
		#:saved-solution->ht)
  (:import-from #:icfpc2021/polygon)
  (:import-from #:icfpc2021/http)
  (:import-from #:icfpc2021/score)
  (:import-from #:icfpc2021/solver)
  (:import-from #:alexandria
                #:plist-hash-table
                #:hash-table-plist)
  (:import-from #:cl-threadpool)
  (:export #:main
           #:post-saved-solutions))

(in-package :icfpc2021/main)

(defvar *problems-dir*)
(defvar *solutions-dir*)

(defparameter *mcts-solver-func*
  (lambda (problem)
    (icfpc2021/mcts-solver:mcts-solve problem)))
(defparameter *a-star-solver-func*
  (lambda (problem)
    (icfpc2021/mcts-solver:a-star-solve problem)))
(defparameter *spring-solver-func*
  (lambda (problem)
    (icfpc2021/solver::solve problem :max-iters 100)))

(defparameter *a-star/mcts-solver-func*
  (lambda (problem)
    (icfpc2021/mcts-solver:a-star/mcts-solve problem :debug-stream nil)))

(defparameter *a-star/mcts-with-her-solver-func*
  (lambda (problem)
    (let ((icfpc2021/mcts-solver::*with-her* t))
      (icfpc2021/mcts-solver:a-star/mcts-solve problem :debug-stream nil))))


(defun main (&key problems-dir solutions-dir (solver :all) (n-threads 6) (timeout 60))
  (let ((problems-dir (or problems-dir (dir-pathname "../problems/")))
	(solvers (ecase solver
		   (:all (list
			  
			  ;; *spring-solver-func*
			  ;; *a-star-solver-func*
			  
			  *a-star/mcts-with-her-solver-func*
			  *a-star/mcts-solver-func*
			  *mcts-solver-func*
			  ))
		   (:mcts (list *mcts-solver-func*))
		   (:spring (list *spring-solver-func*))
                   (:a-star (list *a-star-solver-func*))
                   (:a-star/mcts (list *a-star/mcts-solver-func*)))))
    (format t "Solving problems to find the best solution...~%~%")
    (let ((threadpool (cl-threadpool:make-threadpool n-threads))
	  (jobs (mapcar
		 (lambda (problem-file)
		   (lambda ()
		     (let ((icfpc2021/mcts-solver:*a-star-exhaustive?* t)
			   (*problems-dir* problems-dir)
			   (*solutions-dir* (or solutions-dir
						(dir-pathname "../solutions/")))
			   (icfpc2021/mcts-solver::*timeout-in-seconds* timeout))
		       (process-problem problem-file solvers))))
		 (uiop:directory-files problems-dir))))
      (if (<= n-threads 1)
	  (loop :for job :in jobs :do (funcall job))
	  (cl-threadpool:run-jobs threadpool jobs))
      (cl-threadpool:stop threadpool)
      (format t "Threadpool stopped!"))))

(defun solver-info (solver)
  (cond ((eq solver *mcts-solver-func*)
         (icfpc2021/mcts-solver::get-solver-info))
        ((eq solver *spring-solver-func*)
         (icfpc2021/solver::get-solver-info))
        ((eq solver *a-star-solver-func*)
         (icfpc2021/mcts-solver::a-star-solver-info))
        ((eq solver *a-star/mcts-solver-func*)
         (icfpc2021/mcts-solver::a-star/mcts-solver-info))
	((eq solver *a-star/mcts-with-her-solver-func*)
         (let ((ht (icfpc2021/mcts-solver::a-star/mcts-solver-info)))
	   (setf (gethash :type ht)
		 "a-star/mcts-with-her")
	   ht))
        (t (error "Unknown solver ~A~%" solver))))

(defun process-problem (problem-file solvers)
  (let* ((problem-id (problem-id-from-filename problem-file))
	 (solution-file (solution-file-name problem-id *solutions-dir*))
	 (saved-solution (load-saved-solution solution-file))
	 )
    ;; (when saved-solution
    ;;   (format t "Loaded saved solution for problem ~A: dislikes = ~A~%"
    ;; 	      problem-id
    ;; 	      (saved-solution-dislikes saved-solution)
    ;; 	      ;; (hash-table-plist (saved-solution-solver-info saved-solution))
    ;; 	      ))
    (when (and saved-solution
	       (= 0 (saved-solution-dislikes saved-solution)))
      (format t "0 dislikes for problem ~A in saved solution! Nothing to do.~%" problem-id)
      (return-from process-problem saved-solution))
    (loop :for solver :in solvers
	  :do (let ((solver-type (gethash :type (solver-info solver))))
		(format t "Solving ~A with solver: ~A ~%"
			problem-id
			solver-type)
		(multiple-value-bind (solution dislikes)
		    (ignore-errors (solve-file solver problem-file))
		  (if solution
		      (progn
			(format t "Solution computed by solver ~A for problem ~A: dislikes = ~A (saved ~A)~%"
				solver-type problem-id dislikes
				(when saved-solution (saved-solution-dislikes saved-solution)))
			(when (or (null saved-solution)
				  (< dislikes (saved-solution-dislikes saved-solution)))
			  (save-json (saved-solution->ht
				      (make-saved-solution
				       :vertices solution
				       :dislikes dislikes
				       :solver-info (solver-info solver)))
				     solution-file)))
		      (format t "Solution not found for ~A by ~A~%"
			      problem-id
			      solver-type))
		  )))))
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
    ;; (icfpc2021/polygon:visualize-poly (problem-hole p))
    ;; (terpri)
    ;; (icfpc2021/polygon:visualize-solution p solution)
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

(defun post-saved-solutions (&optional (dir (asdf:system-relative-pathname :icfpc2021 "../solutions/")))
  (loop :for file :in (uiop:directory-files dir)
     :for solution := (load-saved-solution file)
     :when solution
     :do (icfpc2021/http:post-solution
          (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" (pathname-name file)))
          (saved-solution-vertices solution))))
