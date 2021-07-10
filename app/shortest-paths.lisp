(uiop:define-package :icfpc2021/shortest-paths
  (:use #:cl
        #:icfpc2021/model
        #:icfpc2021/polygon)
  (:import-from #:icfpc2021/problem-defs)
  (:export #:make-shortest-paths-matrix
           #:shortest-path))

(in-package :icfpc2021/shortest-paths)

;; Floyd–Warshall algorithm w/o path reconstruction
;; https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
(defun make-shortest-paths-matrix (problem)
  (let* ((num-vertices (length (problem-init-pos problem)))
         (result (make-array (list num-vertices num-vertices)
                             :initial-element nil)))
    (mapc-edges (problem-init-pos problem)
                (problem-edges problem)
                (lambda (i1 p1 i2 p2 edge-len-square)
                  (declare (ignore p1 p2))
                  (setf (aref result i1 i2)
                        (sqrt edge-len-square))))
    (loop :for i :below num-vertices
          :do (setf (aref result i i) 0))
    (loop :for k :below num-vertices
          :do (loop :for i :below num-vertices
                    :do (loop :for j :below num-vertices
                              :do (when (and (aref result i k)
                                             (aref result k j)
                                             (or (null (aref result i j))
                                                 (> (aref result i j)
                                                    (+ (aref result i k)
                                                       (aref result k j)))))
                                    (setf (aref result i j)
                                          (+ (aref result i k)
                                             (aref result k j)))))))
    result))

(defun shortest-path (matrix i j)
  (aref matrix i j))

(defun test-it ()
  (let* ((problem (parsed-problem->problem
                   (icfpc2021/problem-defs:make-problem
                    :hole (icfpc2021/problem-defs:make-hole)
                    :figure (icfpc2021/problem-defs:make-figure
                             :vertices '((0 0) (0 10) (10 10))
                             :edges '((0 1) (1 2)))
                    :epsilon 10000)))
         (paths (make-shortest-paths-matrix problem)))
    (assert (= (shortest-path paths 0 1) 10))
    (assert (= (shortest-path paths 1 2) 10))
    (assert (= (shortest-path paths 0 2) 20))))
