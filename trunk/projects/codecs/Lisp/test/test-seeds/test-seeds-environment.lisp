
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

(defclass test-seeds-environment (environment)
  ((stored-state-key
    :accessor stored-state-key
    :initform (make-state-key)
    :documentation "A stored state key.")
   (stored-random-seed-key
    :accessor stored-random-seed-key
    :initform (make-random-seed-key)
    :documentation "A stored random seed key."))
  (:documentation "An environment which can store two keys."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface implementation.

(defmethod env-init ((env test-seeds-environment))
  "")

(defmethod env-start ((env test-seeds-environment))
  (make-observation))

(defmethod env-step ((env test-seeds-environment) action)
  (values 0.0d0 (make-observation) nil))

(defmethod env-cleanup ((env test-seeds-environment))
  env)

(defmethod env-save-state ((env test-seeds-environment))
  (stored-state-key env))

(defmethod env-load-state ((env test-seeds-environment) state-key)
  (setf (stored-state-key env) state-key)
  state-key)

(defmethod env-save-random-seed ((env test-seeds-environment))
  (stored-random-seed-key env))

(defmethod env-load-random-seed ((env test-seeds-environment) random-seed-key)
  (setf (stored-random-seed-key env) random-seed-key)
  random-seed-key)

(defmethod env-message ((env test-seeds-environment) input-message)
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Starter macro.

(defmacro start-test-seeds-environment (&rest args)
  "Starting a test-seeds-environment environment."
  `(run-env (make-instance 'test-seeds-environment) ,@args))

