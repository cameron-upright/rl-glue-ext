
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

(defclass test-1-experiment (test-experiment) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation.

(defun check-test-1 (exp exp-i0 exp-terminal-p
                     msg exp-agent-msg exp-env-msg)
  "Performs 12 (or 4 if EXP-I0 is nil) checks
during a step for the test-1 test."
  (multiple-value-bind (reward observation terminal-p action) (rl-step exp)
    (check exp #'string= exp-env-msg (rl-env-message exp msg))
    (check exp #'string= exp-agent-msg (rl-agent-message exp msg))
    (check exp #'eq exp-terminal-p terminal-p)
    (when exp-i0
      (check exp #'= 1 (length (int-array observation)))
      (check exp #'= 0 (length (float-array observation)))
      (check exp #'= 0 (length (char-string observation)))
      (check exp #'= exp-i0 (aref (int-array observation) 0))
      (check exp #'= 1 (length (int-array action)))
      (check exp #'= 0 (length (float-array action)))
      (check exp #'= 0 (length (char-string action)))
      (check exp #'= exp-i0 (aref (int-array action) 0)))
    (check exp #'= 1.0 reward)))

(defun run-test-1-experiment (exp &rest args)
  "Runs the experiment of test-1 test."
  (apply #'rl-init exp args)
  (rl-start exp)
  (check-test-1 exp 0 nil "one" "one|1.|one" "one|1.|one")
  (check-test-1 exp 1 nil "two" "two|2.2.|two" "two|2.2.|two")
  (check-test-1 exp 2 nil "three" "three||three" "three||three")
  (check-test-1 exp 3 nil "four" "four|4.|four" "four|4.|four")
  (check-test-1 exp nil t "five" "five|4.|five" "five|5.5.|five")
  (summarize-stat exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Starter macro.

(defmacro start-test-1-experiment (&rest args)
  "Starting a test-1-experiment experiment."
  `(run-test-1-experiment (make-instance 'test-1-experiment
                                         :test-name "test-1") ,@args))

