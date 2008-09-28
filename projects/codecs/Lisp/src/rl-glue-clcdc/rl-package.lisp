
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
;;; RL-Glue package interface.

(defpackage #:rl-glue-clcdc
  (:use #:common-lisp #:ieee-floats #:usocket)
  (:export
   #:*rl-glue-path*
   #:rl-load-utility
   ;; rl-common
   #:int-array
   #:float-array
   #:observation
   #:make-observation
   #:action
   #:make-action
   #:random-seed-key
   #:make-random-seed-key
   #:state-key
   #:make-state-key
   ;; rl-client-agent
   #:agent
   #:agent-init
   #:agent-start
   #:agent-step
   #:agent-end
   #:agent-cleanup
   #:agent-message
   #:run-agent
   ;; rl-client-environment
   #:environment
   #:env-init
   #:env-start
   #:env-step
   #:env-get-state
   #:env-set-state
   #:env-get-random-seed
   #:env-set-random-seed
   #:env-cleanup
   #:env-message
   #:run-env
   ;; rl-client-experiment
   #:experiment
   #:rl-init
   #:rl-start
   #:rl-step
   #:rl-cleanup
   #:rl-return
   #:rl-num-steps
   #:rl-num-episodes
   #:rl-episode
   #:rl-get-state
   #:rl-set-state
   #:rl-get-random-seed
   #:rl-set-random-seed
   #:rl-agent-message
   #:rl-env-message))

