
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

(defpackage #:rl-episode-avg
  (:use #:common-lisp)
  (:export
   #:episode-avg
   #:run-episode-avg
   #:start-episode-avg))

(in-package #:rl-episode-avg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Episode average experiment.

(defclass episode-avg (rl-glue-clcdc:experiment)
  ((steps
    :accessor steps
    :initform '()
    :documentation "Step number per episode (in reverse order).")
   (returns
    :accessor returns
    :initform '()
    :documentation "Returns per episode (in reverse order)."))
  (:documentation "Experiment printing average episode results."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Running the episodes.

(defun run-episode-avg (ep-avg num-episodes &rest args)
  "Plays NUM-EPISODES episodes and prints the averaged result."
  (assert (and (integerp num-episodes) (< 0 num-episodes)) (num-episodes))
  (with-accessors ((steps steps) (returns returns)) ep-avg
    (let ((task-spec (apply #'rl-glue-clcdc:rl-init ep-avg args)))
      (format *standard-output*
              "Task spec was: ~a~%" task-spec))
    (loop repeat num-episodes do
         (rl-glue-clcdc:rl-episode ep-avg 0)
         (format *error-output* ".")
         (force-output *error-output*)
         (push (rl-glue-clcdc:rl-num-steps ep-avg) steps)
         (push (rl-glue-clcdc:rl-return ep-avg) returns))
    (rl-glue-clcdc:rl-cleanup ep-avg)
    (loop
       for s in steps
       for r in returns
       summing s into s-sum
       summing r into r-sum
       finally
         (format *standard-output*
                 "~%-----------------------------------------------~%")
         (format *standard-output*
                 "Number of episodes: ~a~%" num-episodes)
         (format *standard-output*
                 "Average number of steps per episode: ~a~%"
                 (float (/ s-sum num-episodes)))
         (format *standard-output*
                 "Average return per episode: ~a~%"
                 (float (/ r-sum num-episodes)))
         (format *standard-output*
                 "-----------------------------------------------~%")
         (force-output *standard-output*)))
  ep-avg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Starter macro.

(defmacro start-episode-avg (num-episodes &rest args)
  "Starting an episode-avg experiment."
  `(run-episode-avg (make-instance 'episode-avg) ,num-episodes ,@args))

