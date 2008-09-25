
;;; Copyright 2008 Gabor Balazs
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package #:rl-glue-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser for the task specification language.

(defclass task-spec ()
  ((version
    :accessor version
    :documentation "Version number of the task specification language.")
   (episodic
    :accessor episodic
    :documentation "Type of task being solved.")
   (obs-dim
    :accessor obs-dim
    :documentation "Dimension of the observation space.")
   (num-discrete-obs-dims
    :accessor num-discrete-obs-dims
    :documentation "Dimension of the discrete type observation space.")
   (num-continuous-obs-dims
    :accessor num-continuous-obs-dims
    :documentation "Dimension of the continuous type observation space.")
   (obs-types
    :accessor obs-types
    :documentation "Types of the observation space components.")
   (obs-mins
    :accessor obs-mins
    :documentation "Minimum values of the observation space components.")
   (obs-maxs
    :accessor obs-maxs
    :documentation "Maximum values of the observation space components.")
   (action-dim
    :accessor action-dim
    :documentation "Dimension of the action space.")
   (num-discrete-action-dims
    :accessor num-discrete-action-dims
    :documentation "Dimension of the discrete type action space.")
   (num-continuous-action-dims
    :accessor num-continuous-action-dims
    :documentation "Dimension of the continuous type action space.")
   (action-types
    :accessor action-types
    :documentation "Types of the action space components.")
   (action-mins
    :accessor action-mins
    :documentation "Minimum values of the action space components.")
   (action-maxs
    :accessor action-maxs
    :documentation "Maximum values of the action space components.")
   (reward-min
    :accessor reward-min
    :documentation "Minimum reward value for the task.")
   (reward-max
    :accessor reward-max
    :documentation "Maximum reward value for the task."))
  (:documentation "Task specification parameters."))

(defun parse-task-spec-range (spec-string)
  "Parses a renge from SPEC-STRING and returns (min max)."
  (multiple-value-bind (matched matches)
      (cl-ppcre:scan-to-strings "\\[\\s*([^,\\s]*)\\s*,?\\s*([^,\\s]*)\\]"
                                spec-string)
    (declare (ignore matched))
    (assert (= 2 (length matches)))
    (flet ((read-match (str)
             (and str (read-from-string str nil))))
      (let ((min (or (read-match (aref matches 0)) '-inf))
            (max (or (read-match (aref matches 1)) 'inf)))
        (assert (or (numberp min) (eq min '-inf)) (min max))
        (assert (or (numberp max) (eq max 'inf)) (min max))
        (assert (or (not (numberp min))
                    (not (numberp max))
                    (<= min max)) (min max))
        (values min max)))))

(defun parse-task-spec-oa (spec-string)
  "Parses the observation or the action part of the task specification, 
and returns (dim ddim cdim types mins maxs)."
  (let ((matches (cl-ppcre:all-matches-as-strings "[^_]+" spec-string)))
    (assert (<= 3 (length matches)))
    (let ((dim (read-from-string (pop matches)))
          (type-matches (cl-ppcre:all-matches-as-strings "[^\\[\\],\\s]"
                                                         (pop matches))))
      (assert (and (numberp dim) (plusp dim)))
      (assert (= (length type-matches) (length matches)))
      (loop
         with ddim = 0
         with cdim = 0
         with types = '()
         with mins = '()
         with maxs = '()
         for typstr in type-matches
         for range in matches ; matches list only contains ranges
         do
           (assert (= 1 (length typstr)))
           (let ((typch (char-downcase (char typstr 0))))
             (assert (or (char= #\i typch) (char= #\f typch)))
             (ecase typch
               ((#\i) (incf ddim))
               ((#\f) (incf cdim)))
             (push typch types))
           (multiple-value-bind (min max)
               (parse-task-spec-range range)
             (push min mins)
             (push max maxs))
         finally
           (assert (= dim (+ ddim cdim)))
           (return (values dim ddim cdim
                           (nreverse types)
                           (nreverse mins)
                           (nreverse maxs)))))))

(defun parse-task-spec (task-spec-string)
  "Parses the TASK-SPEC-STRING into a task-spec structure and returns it."
  (let ((matches (cl-ppcre:all-matches-as-strings "[^:\\s]+"
                                                  task-spec-string)))
    (assert (= 5 (length matches)))
    (let ((ts (make-instance 'task-spec)))
      ;; language version
      (let ((ver (read-from-string (pop matches))))
        (assert (and (numberp ver) (<= 2 ver 2)) (ver))
        (setf (version ts) ver))
      ;; task type
      (let ((typ (pop matches)))
        (assert (= 1 (length typ)) (typ))
        (let ((typch (char-downcase (char typ 0))))
          (assert (or (char= typch #\e) (char= typch #\c)) (typch))
          (setf (episodic ts) typch)))
      ;; observation
      (multiple-value-bind (dim ddim cdim types mins maxs)
          (parse-task-spec-oa (pop matches))
        (setf (obs-dim ts) dim)
        (setf (num-discrete-obs-dims ts) ddim)
        (setf (num-continuous-obs-dims ts) cdim)
        (setf (obs-types ts) types)
        (setf (obs-mins ts) mins)
        (setf (obs-maxs ts) maxs))
      ;; action
      (multiple-value-bind (dim ddim cdim types mins maxs)
          (parse-task-spec-oa (pop matches))
        (setf (action-dim ts) dim)
        (setf (num-discrete-action-dims ts) ddim)
        (setf (num-continuous-action-dims ts) cdim)
        (setf (action-types ts) types)
        (setf (action-mins ts) mins)
        (setf (action-maxs ts) maxs))
      ;; reward
      (multiple-value-bind (min max)
          (parse-task-spec-range (pop matches))
        (setf (reward-min ts) min)
        (setf (reward-max ts) max))
      (assert (null matches))
      ts)))

