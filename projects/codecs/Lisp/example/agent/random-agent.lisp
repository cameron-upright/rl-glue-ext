
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
  (:use #:common-lisp #:rl-glue-codec #:rl-glue-utils)
  (:export
   #:random-agent
   #:start-random-agent))

(in-package #:rl-random-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun select-random-action (agent)
  (with-accessors ((task-spec task-spec) (rstate rand-state)) agent
    (flet ((get-random (min max)
             "Returns a random number between min and max."
             (+ min (random max rstate))))
      (make-action
       :int-array (let ((values (across-ranges
                                 #'get-random
                                 (int-actions task-spec))))
                    (make-int-array (length values)
                                    :initial-contents values))
       :float-array (let ((values (across-ranges
                                   #'get-random
                                   (float-actions task-spec))))
                      (make-float-array (length values)
                                        :initial-contents values))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun start-random-agent (&rest args)
  "Starting a random agent."
  (apply #'run-agent (make-instance 'random-agent) args))

