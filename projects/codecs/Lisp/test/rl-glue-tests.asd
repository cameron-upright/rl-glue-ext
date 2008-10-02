
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

(defpackage #:rl-glue-tests-asdf
  (:use #:asdf #:common-lisp))

(in-package #:rl-glue-tests-asdf)

(defsystem rl-glue-tests
  :name "RL-Glue Common Lisp Codec Tests"
  :version "0.1"
  :licence "Apache v2"
  :author "Gabor Balazs <gabalz@gmail.com>"
  :maintainer "Gabor Balazs <gabalz@gmail.com>"
  :description "Tests for RL-Glue components."
  :components
  ((:file "rl-tests-package")
   (:file "rl-tests-common" :depends-on ("rl-tests-package"))
   (:module test-1
            :components
            ((:file "test-1-agent")
             (:file "test-1-environment")
             (:file "test-1-experiment"))
            :depends-on ("rl-tests-common"))
   (:module test-empty
            :components
            ((:file "test-empty-agent")
             (:file "test-empty-environment")
             (:file "test-empty-experiment"))
            :depends-on ("rl-tests-common")))
  :depends-on ("rl-glue-clcdc" "rl-glue-utils"))

