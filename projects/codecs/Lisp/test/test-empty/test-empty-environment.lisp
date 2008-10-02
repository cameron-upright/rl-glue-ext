
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

(in-package #:rl-glue-tests)

(defclass test-empty-environment (environment)
  ((empty-observation
    :accessor empty-observation
    :documentation "A fixed empty observation.")
   (non-empty-observation
    :accessor non-empty-observation
    :documentation "A fixed non empty observation.")
   (which-episode
    :accessor which-episode
    :documentation "Number of the current episode."))
  (:documentation "An environment which sends an empty observation in every 
second episodes and a non-empty one in the other cases."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface implementation.

(defmethod env-init ((env test-empty-environment))
  (setf (which-episode env) 0)
  (setf (empty-observation env) (make-observation))
  (setf (non-empty-observation env)
        (fill-adt (make-observation) :ints 2 :floats 4 :chars 5))
  "")

(defun get-test-empty-observation (env)
  (if (evenp (which-episode env))
      (empty-observation env)
      (non-empty-observation env)))

(defmethod env-start ((env test-empty-environment))
  (incf (which-episode env))
  (get-test-empty-observation env))

(defmethod env-step ((env test-empty-environment) action)
  (values 0 (get-test-empty-observation env) nil))

(defmethod env-cleanup ((env test-empty-environment))
  env)

(defmethod env-get-state ((env test-empty-environment))
  (make-state-key))

(defmethod env-set-state ((env test-empty-environment) state-key)
  'not-supported)

(defmethod env-get-random-seed ((env test-empty-environment))
  (make-random-seed-key))

(defmethod env-set-random-seed ((env test-empty-environment) random-seed-key)
  'not-supported)

(defmethod env-message ((env test-empty-environment) input-message)
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Starter macro.

(defmacro start-test-empty-environment (&rest args)
  "Starting a test-empty-environment environment."
  `(run-env (make-instance 'test-empty-environment) ,@args))

