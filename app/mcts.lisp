;; As described in:
;;   http://www.cameronius.com/cv/mcts-survey-master.pdf
;; A Survey of Monte Carlo Tree Search Methods

(defpackage :icfpc2021/mcts
  (:use :common-lisp)
  (:export #:select-next-move
           #:possible-actions
           #:next-state
           #:clone-state
           #:show-state
           #:estimate-state-rewards
           #:print-decision-tree
           #:count-nodes
           #:mapc-children-actions)
  (:import-from :cl-ppcre))

(in-package :icfpc2021/mcts)

(defvar *exploration-coefficient*)
(defvar *max-selection-depth*)
(defvar *players-number*)
(defvar *node-dot-ids*)
(defvar *node-dot-ids-counter*)

;; TODO: what about state sharing?
(defclass node ()
  ((parent :reader parent
           :initarg :parent
           :type (or null node))
   (player :reader player
           :initarg :player
           :type fixnum
           :documentation "number of player whos turn it is now")
   ;; Q
   (simulation-rewards :type simple-vector
                       :initform (make-array *players-number*
                                             :element-type 'number
                                             :initial-element 0)
                       :accessor simulation-rewards
                       :initarg :simulation-rewards
                       :documentation "estimated rewards for each player, vector of numbers")
   ;; N
   (visit-count :type integer
                :initform 0
                :accessor visit-count
                :initarg :visit-count)
   (unexplored-actions :type list
                       :accessor unexplored-actions
                       :initarg :unexplored-actions)
   (children :type list
             :initform nil
             :accessor children
             :initarg :children
             :documentation "list of pairs (node . action) where
   'action' applied to state for 'this node' leads to 'node'") ))

(defgeneric possible-actions (state player)
  (:documentation "return the list of possible actions for specified player"))

(defgeneric next-state (state action)
  (:documentation "apply action to the state and return new state"))

(defgeneric clone-state (state)
  (:documentation "return copy of the state that could be modified locally"))

(defgeneric estimate-state-rewards (state player)
  (:documentation "return vector of reward values one per each player"))

(defgeneric show-state (state)
  (:documentation "convert state to string for debugging purpose"))

(defgeneric show-action (action)
  (:documentation "convert action to string for debugging purpose"))

(defmethod show-action ((a t))
  (format nil "~A" a))

(defun get-best-move (root)
  (let* ((*exploration-coefficient* 0))
    (cdr (select-best-child root))))

(defun select-next-move (&key
                           root-state
                           root-player
                           max-iters
                           timeout-in-seconds
                           (exploration-coefficient 1)
                           (max-selection-depth 1000)
                           players-number)
  (assert (or max-iters timeout-in-seconds))
  (assert players-number)
  (assert root-state)
  (assert root-player)
  (let* ((stop-time (when timeout-in-seconds
                      (+ (get-internal-run-time)
                         (* timeout-in-seconds
                            internal-time-units-per-second))))
         (*exploration-coefficient* exploration-coefficient)
         (*max-selection-depth* max-selection-depth)
         (*players-number* players-number)
         (root (make-node-for-state root-state
                                    :parent nil
                                    :player root-player))
         (i 0))
    (loop while (and (< i max-iters)
                     (or (null stop-time)
                         (< (get-internal-run-time) stop-time)))
       do 
         (multiple-value-bind (node state)
             (selection-loop root (clone-state root-state) 0)
           (incf i)
           (backup node
                   (estimate-state-rewards state (player node)))))
    (values (when (children root)
              (get-best-move root))
            root)))

(defun selection-loop (node state depth)
  (with-slots (children unexplored-actions player) node
    (cond
      ;; no moves or out of budget
      ((or (and (null children)
                (null unexplored-actions))
           (> depth *max-selection-depth*))
       (values node state))
      ;; stop the loop and pop first unexplored
      (unexplored-actions
       (expand-node node state))
      ;; select best child and repeat
      (children
       (destructuring-bind (best-child . action)
           (select-best-child node)
         (selection-loop best-child
                         (next-state state action)
                         (1+ depth))))
      (t (error "Should never reach this state")))))

(defun expand-node (node state)
  (let* ((action (pop (unexplored-actions node)))
         (child-state (next-state state action))
         (child-node (make-node-for-state child-state
                                          :parent node
                                          :player (next-player node))))
    (push (cons child-node action) (children node))
    (values child-node child-state)))

(defun next-player (node)
  (mod (1+ (player node))
       *players-number*))

(defun select-best-child (node)
  (assert (not (null (children node))))
  (let ((2-ln-node-visits (* 2 (log (visit-count node))))
        (player (player node)))
    (labels ((%ucb1 (child-node)
               (+ (/ (aref (simulation-rewards child-node) player)
                     (visit-count child-node))
                  (* *exploration-coefficient*
                     (sqrt (/ 2-ln-node-visits
                              (visit-count child-node)))))))
      ;; argmax loop
      (let* ((best-child/action (first (children node)))
             (best-child-score (%ucb1 (car best-child/action))))
        (dolist (child/action (rest (children node)))
          (let ((child-score (%ucb1 (car child/action))))
            (when (> child-score best-child-score)
              (setf best-child/action child/action
                    best-child-score child-score))))
        ;; return
        best-child/action))))

(defun backup (bottom-node reward-vector)
  (loop
     for node = bottom-node then (parent node)
     while node
     do
       (incf (visit-count node))
       (let ((node-reward-vector (simulation-rewards node))
             (i (player node)))
         (incf (aref node-reward-vector i)
               (aref reward-vector i))
         ;; pairwise (simulation-reward node) + reward-vector
         ;; (loop for i below (length node-reward-vector) do
         ;;      (incf (aref node-reward-vector i)
         ;;            (aref reward-vector i)))
         )))

(defun make-node-for-state (state &key parent player)
  (assert player)
  (make-instance
   'node
   :unexplored-actions (possible-actions state player)
   :parent parent
   :player player))

(defun print-decision-tree (stream root root-state &key (exploration-coefficient 1))
  (let ((*node-dot-ids* (make-hash-table :test #'eq))
        (*node-dot-ids-counter* 0)
        (*exploration-coefficient* exploration-coefficient))
    (format stream "digraph G {~%")
    (format stream "
  graph [fontname = \"monospace\"];
  node [fontname = \"monospace\"];
  edge [fontname = \"monospace\"];")
    (node-to-dot stream root root-state t)
    (format stream "}~%")))

(defun node-dot-id (node)
  (or (gethash node *node-dot-ids*)
      (setf (gethash node *node-dot-ids*)
            (incf *node-dot-ids-counter*))))

(defun node-to-dot (stream node state best?)
  (let ((id (node-dot-id node))
        (best-child (when (children node)
                      (car (select-best-child node)))))
    (format stream "  ~A [label=\"~A\", shape=box ~A];~%"
            id
            (node-dot-label node state)
            (if best? ", fillcolor=lightgray, style=filled" ""))
    (loop for (child . action) in (children node) do
         (format stream "  ~A -> ~A [label = \"~A\"];~%"
                 id
                 (node-dot-id child)
                 (escape-for-dot
                  (show-action action)))
         (node-to-dot stream
                      child
                      (next-state (clone-state state) action)
                      (eq child best-child)))))

(defun node-dot-label (node state)
  (escape-for-dot
   (with-output-to-string (stream)
     (format stream "N: ~A Q: ~A P: ~A~A~%"
             (visit-count node)
             (simulation-rewards node)
             (player node)
             (if (unexplored-actions node)
                 (format nil " A: ~A~%" (length (unexplored-actions node)))
                 ""))
     (format stream "~A" (show-state state)))))

(defun escape-for-dot (text)
  (cl-ppcre:regex-replace-all "\\n" text "\\\\n"))

(defun count-nodes (root)
  (let ((queue (list root))
        (count 0))
    (loop while queue do
         (incf count)
         (let ((n (pop queue)))
           (dolist (c (children n))
             (push (car c) queue))))
    count))

(defun mapc-children-actions (root fn)
  (labels ((%traverse (actions node)
             (loop :for (child-node . action) :in (children node)
                   :for new-actions := (cons action actions)
                   :do (funcall fn new-actions)
                   :do (%traverse new-actions child-node))))
    (%traverse nil root)))
