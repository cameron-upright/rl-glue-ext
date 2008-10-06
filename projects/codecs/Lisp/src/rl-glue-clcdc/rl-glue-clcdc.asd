
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RL-Glue ASDF System

(defpackage #:rl-glue-clcdc-asdf
  (:use #:asdf #:common-lisp))

(in-package #:rl-glue-clcdc-asdf)

(defsystem rl-glue-clcdc
  :name "RL-Glue Common Lisp Codec"
  :version "0.2"
  :licence "Apache v2"
  :author "Gabor Balazs <gabalz@gmail.com>"
  :maintainer "Gabor Balazs <gabalz@gmail.com>"
  :description "Software protocol for connecting RL agents and environments."
  :components
  ((:file "rl-package")
   (:file "rl-version" :depends-on ("rl-package"))
   (:file "rl-buffer" :depends-on ("rl-package"))
   (:file "rl-common" :depends-on ("rl-package" "rl-buffer"))
   (:file "rl-network" :depends-on ("rl-package" "rl-buffer"))
   (:module agent
            :components
            ((:file "rl-client-agent"))
            :depends-on ("rl-package" "rl-buffer"
                         "rl-common" "rl-network" "rl-version"))
   (:module environment
            :components
            ((:file "rl-client-environment"))
            :depends-on ("rl-package" "rl-buffer"
                         "rl-common" "rl-network" "rl-version"))
   (:module experiment
            :components
            ((:file "rl-client-experiment"))
            :depends-on ("rl-package" "rl-buffer"
                         "rl-common" "rl-network" "rl-version")))
  :depends-on ("usocket"))

