
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

(defpackage #:rl-random-agent
  (:use #:common-lisp #:rl-glue-clcdc #:rl-glue-utils)
  (:export
   #:random-agent
   #:start-random-agent))

(in-package #:rl-random-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Randomized agent for any task.

(defclass random-agent (agent)
  ((task-spec
    :accessor task-spec
    :documentation "Current task specification.")
   (rand-state
    :accessor rand-state
    :initform (make-random-state t)
    :documentation "Random state of the agent."))
  (:documentation "Random RL agent."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions.

(defun select-random-action (agent)
  (with-accessors ((task-spec task-spec) (rstate rand-state)) agent
    (let ((i-actions
           (make-array (num-discrete-action-dims task-spec)
                       :element-type 'integer))
          (f-actions
           (make-array (num-continuous-action-dims task-spec)
                       :element-type 'double-float)))
      (loop
         with ii = 0
         with fi = 0
         for min in (action-mins task-spec)
         for max in (action-maxs task-spec)
         for type in (action-types task-spec)
         do
           (ecase type
             ((#\i)
              (assert (and (integerp min) (integerp max)) (min max))
              (setf (aref i-actions ii) (+ min (random max rstate)))
              (incf ii))
             ((#\f)
              (setf (aref f-actions fi) (+ min (random (float max) rstate)))
              (incf fi))))
      (make-action :int-array i-actions :float-array f-actions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface methods.

(defmethod agent-init ((agent random-agent) task-spec)
  (setf (task-spec agent) (parse-task-spec task-spec))
  agent)

(defmethod agent-start ((agent random-agent) first-observation)
  (select-random-action agent))

(defmethod agent-step ((agent random-agent) reward observation)
  (select-random-action agent))

(defmethod agent-end ((agent random-agent) reward)
  agent)

(defmethod agent-cleanup ((agent random-agent))
  (setf (task-spec agent) nil)
  agent)

(defmethod agent-message ((agent random-agent) input-message)
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Starter macro.

(defmacro start-random-agent (&rest args)
  "Starting a random agent."
  `(run-agent (make-instance 'random-agent) ,@args))

