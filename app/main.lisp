(uiop:define-package :icfpc2021/main
    (:use #:cl
          #:icfpc2021/model
          #:icfpc2021/mcts-solver)
  (:import-from #:icfpc2021/problem-defs)
  (:import-from #:icfpc2021/polygon)
  (:import-from #:icfpc2021/http)
  (:import-from #:icfpc2021/score)
  (:import-from #:icfpc2021/parse))

(in-package :icfpc2021/main)

(defun main ()
  (format t "Under construction, nothing to see here yet"))

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

(defun file-problem-id (file)
  (cl-ppcre:scan-to-strings "[0-9]+" (pathname-name file)))

(defun try-solve-and-post-all (solver dir)
  (loop :for file :in (directory dir)
        :do (multiple-value-bind (solution dislikes)
                (ignore-errors (solve-file solver file))
              (when solution
                (format t "Solution found for ~A: ~A~%" (file-problem-id file)
                        dislikes)
                (icfpc2021/http:post-solution (file-problem-id file) solution)))))
