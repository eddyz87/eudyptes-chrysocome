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

(defun a-star (initial-state &key (predicate #'<) hash-state? (state-hash-test #'eql))
  (let ((queue (pileup:make-heap predicate :key #'state-estimate))
        (visited (when hash-state?
                   (make-hash-table :test state-hash-test))))
    (pileup:heap-insert initial-state queue)
    (loop :for state := (pileup:heap-pop queue)
          :while state
          :do (if (final-state? state)
                  (return-from a-star state)
                  (loop :for next-state :in (next-states state)
                        :do (unless (and hash-state?
                                         (gethash (state-hash-object next-state)
                                                  visited))
                              (when hash-state?
                                (setf (gethash (state-hash-object next-state)
                                               visited)
                                      t))
                              (pileup:heap-insert next-state queue)))))
    nil))


;; Test

(defstruct test-state
  a
  b
  target)

(defmethod next-states ((state test-state))
  (format t "Next states for ~A~%" state)
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
