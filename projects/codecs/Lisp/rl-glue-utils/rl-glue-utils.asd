
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
;;; RL-Glue Utilities ASDF System

(defpackage #:rl-glue-utils-asdf
  (:use #:asdf #:common-lisp))

(in-package #:rl-glue-utils-asdf)

(defsystem rl-glue-utils
  :name "RL-Glue Utilities"
  :version "0.1"
  :licence "Apache v2"
  :author "Gabor Balazs <gabalz@gmail.com>"
  :maintainer "Gabor Balazs <gabalz@gmail.com>"
  :description "Utilities for RL-Glue."
  :components
  ((:file "rl-utils-package")
   (:module task-spec-parser
            :components
            ((:file "task-spec-parser"))
            :depends-on ("rl-utils-package")))
  :depends-on ("cl-ppcre"))

