
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

(defclass test-seeds-experiment (test-experiment) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation.

(defun run-test-seeds-experiment (exp &rest args)
  "Runs the experiment of test-seeds test."
  (let ((state-key (fill-adt (make-state-key)
                             :ints 3 :floats 7 :chars 2))
        (empty-state-key (make-state-key))
        (random-seed-key (fill-adt (make-random-seed-key)
                                   :ints 1 :floats 2 :chars 4))
        (empty-random-seed-key (make-random-seed-key)))
    (apply #'rl-init exp args)
    (rl-load-state exp state-key)
    (check exp #'rl-equalp state-key (rl-save-state exp))
    (rl-load-random-seed exp random-seed-key)
    (check exp #'rl-equalp random-seed-key (rl-save-random-seed exp))
    (rl-load-state exp empty-state-key)
    (check exp #'rl-equalp empty-state-key (rl-save-state exp))
    (rl-load-random-seed exp empty-random-seed-key)
    (check exp #'rl-equalp empty-random-seed-key (rl-save-random-seed exp)))
  (summarize-stat exp)
  (rl-close exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Starter macro.

(defmacro start-test-seeds-experiment (&rest args)
  "Starting a test-seeds-experiment experiment."
  `(run-test-seeds-experiment (make-instance 'test-seeds-experiment
                                             :test-name "test-seeds") ,@args))

