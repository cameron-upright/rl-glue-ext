
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

(defclass test-1-environment (environment)
  ((step-count
    :accessor step-count
    :initform 0
    :documentation "Step counter."))
  (:documentation "A simple never terminating environment."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface implementation.

(defmethod env-init ((env test-1-environment))
  "sample task spec")

(defmethod env-start ((env test-1-environment))
  (setf (step-count env) 0)
  (fill-adt (make-observation) :ints 1 :floats 2 :chars 3))

(defmethod env-step ((env test-1-environment) action)
  (with-accessors ((step-count step-count)) env
    (let ((observation (fill-adt (make-observation) :ints 1)))
      (setf (aref (int-array observation) 0) step-count)
      (incf step-count)
      (values 1.0 observation (= step-count 5)))))

(defmethod env-cleanup ((env test-1-environment))
  env)

(defmethod env-get-state ((env test-1-environment))
  (make-state-key))

(defmethod env-set-state ((env test-1-environment) state-key)
  'not-supported)

(defmethod env-get-random-seed ((env test-1-environment))
  (make-random-seed-key))

(defmethod env-set-random-seed ((env test-1-environment) random-seed-key)
  'not-supported)

(defmethod env-message ((env test-1-environment) input-message)
  (create-answer-message (step-count env) input-message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Starter macro.

(defmacro start-test-1-environment (&rest args)
  "Starting a test-1-environment environment."
  `(run-env (make-instance 'test-1-environment) ,@args))

