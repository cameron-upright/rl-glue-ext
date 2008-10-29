
;;; Copyright 2008 Gabor Balazs
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;
;;; $Revision$
;;; $Date$

(in-package #:cl-user)

(defpackage #:rl-mines
  (:use #:common-lisp)
  (:export
   #:mines
   #:start-mines))

(in-package #:rl-mines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mines environment.

(defclass mines (rl-glue-codec:environment)
  ((rand-state
    :accessor rand-state
    :initform (make-random-state t)
    :documentation "Random state of the environment.")
   (step-num
    :accessor step-num
    :initform 0
    :documentation "Step number.")
   (start
    :accessor start
    :documentation "Start marker in grid.")
   (goal
    :accessor goal
    :documentation "End marker in grid.")
   (land
    :accessor land
    :documentation "Free space in grid.")
   (obstacle
    :accessor obstacle
    :documentation "Obstical in grid.")
   (mine
    :accessor mine
    :documentation "Mine in grid.")
   (row
    :accessor row
    :documentation "Number of rows in grid.")
   (col
    :accessor col
    :documentation "Number of columns in grid.")
   (field-map
    :accessor field-map
    :documentation "Map of the mine field.")
   (start-row
    :accessor start-row
    :documentation "Row of starting position.")
   (start-col
    :accessor start-col
    :documentation "Column of starting position/")
   (agent-row
    :accessor agent-row
    :documentation "Row of agent's current position.")
   (agent-col
    :accessor agent-col
    :documentation "Column of agent's current position.")
   (terminal
    :accessor terminal
    :documentation "Shows whether the agent is in terminal state."))
  (:documentation "Environment about passing through a mine field."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions.

(defun make-observation (mines &key row col terminal)
  "Creates an observation of a mines environment state."
  (rl-glue-codec:make-observation
   :int-array (rl-glue-codec:make-int-array
               1
               :initial-contents
               (list (if terminal -1 (+ (* row (col mines)) col))))
   :float-array (rl-glue-codec:make-float-array
                 2
                 :initial-contents
                 (list 4.5d0 3.4d0))
   :char-string "Can I do it successfully?"))

(defun pos-type (mines row col)
  "Returns the land type of the specified position."
  (aref (field-map mines) row col))

(defun update-position (mines action-idx)
  "Updates the agent's position according to ACTION-IDX."
  (with-accessors ((arow agent-row) (acol agent-col)) mines
    (let ((new-row arow) (new-col acol))
      (ecase action-idx
        (0 (decf new-col))
        (1 (incf new-col))
        (2 (decf new-row))
        (3 (incf new-row)))
      (when (and (<= 0 new-row) (< new-row (row mines))
                 (<= 0 new-col) (< new-col (col mines))
                 (/= (pos-type mines new-row new-col) (obstacle mines)))
        (setf arow new-row)
        (setf acol new-col))))
  mines)

(defun get-reward (mines)
  "Returns the (reward,terminal) pair for the current position."
  (let ((postype (pos-type mines (agent-row mines) (agent-col mines))))
    (cond ((= postype (goal mines)) (values 10.0d0 t))
          ((= postype (mine mines)) (values -10.0d0 t))
          (t (values -1.0d0 nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface methods.

(defmethod rl-glue-codec:env-init ((env mines))
  ;; setting field marks
  (setf (start env) 0)
  (setf (goal env) 1)
  (setf (land env) 2)
  (setf (obstacle env) 3)
  (setf (mine env) 4)
  ;; creating the map
  (setf (row env) 6)
  (setf (col env) 18)
  (setf (field-map env)
        (make-array (list (row env) (col env))
                    :element-type 'integer
                    :initial-contents
                    '((3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
                      (3 2 2 2 2 2 2 4 4 2 2 2 0 2 2 2 2 3)
                      (3 2 2 2 2 4 2 2 2 2 2 2 2 2 2 2 2 3)
                      (3 2 2 2 2 2 2 2 2 4 4 4 2 2 2 2 3 3)
                      (3 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 3)
                      (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3))))
  ;; setting positions
  (setf (start-row env) 1)
  (setf (start-col env) 12)
  (setf (agent-row env) (start-row env))
  (setf (agent-col env) (start-col env))
  ;; returning the task specification
  (format nil "2:e:1_[i]_[0,~a]:1_[i]_[0,~a]:[-10,10]"
          (1- (* (row env) (col env))) 3))

(defmethod rl-glue-codec:env-start ((env mines))
  (with-accessors ((rand-state rand-state)
                   (start-row start-row) (start-col start-col)
                   (map field-map) (land land) (row row) (col col)) env
    (setf (terminal env) nil)
    (setf (aref map start-row start-col) land)
    (loop
       for r = (random row rand-state)
       for c = (random col rand-state)
       while (/= land (aref map r c))
       finally
         (setf start-row r)
         (setf start-col c))
    (setf (aref map start-row start-col) (start env))
    (setf (agent-row env) start-row)
    (setf (agent-col env) start-col)
    (make-observation env :row start-row :col start-col)))

(defmethod rl-glue-codec:env-step ((env mines) action)
  (incf (step-num env))
  (update-position env (aref (rl-glue-codec:int-array action) 0))
  (multiple-value-bind (reward terminal) (get-reward env)
    (values reward
            (make-observation env
                              :row (agent-row env)
                              :col (agent-col env)
                              :terminal terminal)
            terminal)))

(defmethod rl-glue-codec:env-cleanup ((env mines))
  env)

(defmethod rl-glue-codec:env-save-state ((env mines))
  (rl-glue-codec:make-state-key))

(defmethod rl-glue-codec:env-load-state ((env mines) state-key)
  'not-supported)

(defmethod rl-glue-codec:env-save-random-seed ((env mines))
  (rl-glue-codec:make-random-seed-key))

(defmethod rl-glue-codec:env-load-random-seed ((env mines) random-seed-key)
  'not-supported)

(defmethod rl-glue-codec:env-message ((env mines) input-message)
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Starter macro.

(defmacro start-mines (&rest args)
  "Starting a mines environment."
  `(rl-glue-codec:run-env (make-instance 'mines) ,@args))

