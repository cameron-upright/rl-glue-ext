
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

(defclass test-speed-experiment (test-experiment) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation.

(defun test-speed-measurement (exp)
  (let ((time-string (with-output-to-string (*trace-output*)
                       (time (rl-episode exp 0)))))
    (multiple-value-bind (matched matches)
        (cl-ppcre:scan-to-strings
         "([0-9]+\\.[0-9]+) seconds of real time"
         time-string)
      (when matched
        (let ((ms (* 1000 (read-from-string (aref matches 0)))))
          (format t "Elapsed time in ms: ~a, per step is ~a~%"
                  ms (/ ms (rl-num-steps exp))))))))

(defun run-test-speed-experiment (exp &rest args)
  "Runs the experiment of test-speed test."
  (apply #'rl-init exp args)
  (rl-episode exp 500)
  (test-speed-measurement exp)
  (test-speed-measurement exp)
  (summarize-stat exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Starter macro.

(defmacro start-test-speed-experiment (&rest args)
  "Starting a test-speed-experiment experiment."
  `(run-test-speed-experiment (make-instance 'test-speed-experiment
                                             :test-name "test-speed") ,@args))

