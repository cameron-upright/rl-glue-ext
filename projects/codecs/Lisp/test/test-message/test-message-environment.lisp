
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

(defclass test-message-environment (environment)
  ()
  (:documentation "An environment which always sends empty observations, but 
tests the message transmitting mechanism."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface implementation.

(defmethod env-init ((env test-message-environment))
  "")

(defmethod env-start ((env test-message-environment))
  (make-observation))

(defmethod env-step ((env test-message-environment) action)
  (values 0 (make-observation) nil))

(defmethod env-cleanup ((env test-message-environment))
  env)

(defmethod env-get-state ((env test-message-environment))
  (make-state-key))

(defmethod env-set-state ((env test-message-environment) state-key)
  'not-supported)

(defmethod env-get-random-seed ((env test-message-environment))
  (make-random-seed-key))

(defmethod env-set-random-seed ((env test-message-environment) random-seed-key)
  'not-supported)

(defmethod env-message ((env test-message-environment) input-message)
  (cond
    ((string= input-message "") "empty")
    ((string= input-message "empty") "")
    ((string= input-message "null") "")
    (t input-message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Starter macro.

(defmacro start-test-message-environment (&rest args)
  "Starting a test-message-environment environment."
  `(run-env (make-instance 'test-message-environment) ,@args))

