
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

(in-package #:cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RL-Glue utility package.

(defpackage #:rl-glue-utils
  (:use #:common-lisp)
  (:export
   ;; task-spec-parser
   #:task-spec
   #:version
   #:episodic
   #:obs-dim
   #:num-discrete-obs-dims
   #:num-continuous-obs-dims
   #:obs-types
   #:obs-mins
   #:obs-maxs
   #:action-dim
   #:num-discrete-action-dims
   #:num-continuous-action-dims
   #:action-types
   #:action-mins
   #:action-maxs
   #:reward-min
   #:reward-max
   #:parse-task-spec))

