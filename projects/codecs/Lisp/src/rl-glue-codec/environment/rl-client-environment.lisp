
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

(in-package #:org.rl-community.rl-glue-codec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment client interface.

(defclass environment () () (:documentation "The RL-Glue environment."))

(defgeneric env-init (env)
  (:documentation "DESCRIPTION:
    This routine will be called exactly once for each trial/run. This
    function is an ideal place to initialize all environment information 
    and allocate any resources required to represent the environment. It 
    must return a task specification which adheres to the task specification 
    language. A task specification stores information regarding the 
    observation and action space, as well as whether the task is episodic 
    or continuous.

PARAMETERS:
    env : environment object in use [rl-glue:environment]

RETURNS:
    task specification [string]"))

(defgeneric env-start (env)
  (:documentation "DESCRIPTION:
    For a continuing task this is done once. For an episodic task, this is
    done at the beginning of each episode. Env_start assembles a
    first observation given the agent is in the start state. Note the start
    state cannot also be a terminal state.

PARAMETERS:
    env : environment object in use [rl-glue:environment]

RETURNS:
    1st observation of an episode [rl-glue:observation]"))

(defgeneric env-step (env action)
  (:documentation "DESCRIPTION:
    Complete one step in the environment. Take the action passed in and
    determine what the reward and next state are for that transition.

PARAMETERS:
    env    : environment object in use [rl-glue:environment]
    action : action to be performed [rl-glue:action]

RETURNS:
    reward        : reward of the step [double-float]
    observation   : observation after the step [rl-glue:observation]
    terminal flag : shows whether the episode is ended [boolean]"))

(defgeneric env-get-state (env)
  (:documentation "DESCRIPTION:
    The state-key is a compact representation of the current state of the 
    environment such that at any point in the future, provided with the
    state key, the environment could return to that state. Note that this
    does not include the agent's value function, it is merely restoring the
    details of the environment. For example, in a static grid world this
    would be as simple as the position of the agent.

PARAMETERS:
    env : environment object in use [rl-glue:environment]

RETURNS:
    state key [rl-glue:state-key]"))

(defgeneric env-set-state (env state-key)
  (:documentation "DESCRIPTION:
    Given the STATE-KEY, the environment should return to it's exact
    formation when the state_key was obtained.

PARAMETERS:
    env       : environment object in use [rl-glue:environment]
    state-key : state key to set [rl-glue:state-key]

RETURNS:
    (none)"))

(defgeneric env-get-random-seed (env)
  (:documentation "DESCRIPTION:
    Saves the random seed object used by the environment such that it can
    be restored upon presentation of random seed key.

PARAMETERS:
    env : environment object in use [rl-glue:environment]

RETURNS:
    random seed key [rl-glue:random-seed-key]"))

(defgeneric env-set-random-seed (env random-seed-key)
  (:documentation "DESCRIPTION:
    Sets the random seed used by the environment. Typically it is
    advantageous for the experiment program to control the randomness of
    the environment. The env-set-random-seed can be used in conjunction
    with env-set-state to save and restore a random_seed such that the
    environment will behave exactly the same way it has previously when
    it was in this state and given the same actions.

PARAMETERS:
    env             : environment object in use [rl-glue:environment]
    random-seed-key : random seed key to set [rl-glue:random-seed-key]

RETURNS:
    (none)"))

(defgeneric env-cleanup (env)
  (:documentation "DESCRIPTION:
    This can be used to release any allocated resources. It will be called
    once for every call to env-init.

PARAMETERS:
    env : environment object in use [rl-glue:environment]

RETURNS:
    (none)"))

(defgeneric env-message (env input-message)
  (:documentation "DESCRIPTION:
    Similar to agent-message, this function allows for any message passing
    to the environment required by the experiment program. This may be used
    to modify the environment mid experiment. Any information that needs to
    passed in or out of the environment can be handled by this function.

PARAMETERS:  
    env           : environment object in use [rl-glue:environment]
    input-message : recieved message [string]

RETURNS:
    output message to send [string]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment client methods surrounded by buffer handling.

(defun on-env-init (env buffer)
  (declare #.*optimize-settings*)
  (let ((task-spec (env-init env)))
    (buffer-clear buffer)
    (rl-write-task-spec task-spec buffer))
  env)

(defun on-env-start (env buffer)
  (declare #.*optimize-settings*)
  (let ((observation (env-start env)))
    (buffer-clear buffer)
    (rl-write-observation observation buffer))
  env)

(defun on-env-step (env buffer)
  (declare #.*optimize-settings*)
  (let ((action (rl-read-action buffer)))
    (multiple-value-bind (reward observation terminal)
        (env-step env action)
      (buffer-clear buffer)
      (rl-write-terminal terminal buffer)
      (rl-write-reward reward buffer)
      (rl-write-observation observation buffer)))
  env)

(defun on-env-get-state (env buffer)
  (declare #.*optimize-settings*)
  (let ((state-key (env-get-state env)))
    (buffer-clear buffer)
    (rl-write-state-key state-key buffer))
  env)

(defun on-env-set-state (env buffer)
  (declare #.*optimize-settings*)
  (let ((state-key (rl-read-state-key buffer)))
    (env-set-state env state-key))
  (buffer-clear buffer)
  env)

(defun on-env-get-random-seed (env buffer)
  (declare #.*optimize-settings*)
  (let ((random-seed (env-get-random-seed env)))
    (buffer-clear buffer)
    (rl-write-random-seed-key random-seed buffer))
  env)

(defun on-env-set-random-seed (env buffer)
  (declare #.*optimize-settings*)
  (let ((random-seed (rl-read-random-seed-key buffer)))
    (env-set-random-seed env random-seed))
  (buffer-clear buffer)
  env)

(defun on-env-cleanup (env buffer)
  (declare #.*optimize-settings*)
  (env-cleanup env)
  (buffer-clear buffer)
  env)

(defun on-env-message (env buffer)
  (declare #.*optimize-settings*)
  (let ((input-msg (rl-read-message buffer)))
    (let ((output-msg (env-message env input-msg)))
      (buffer-clear buffer)
      (rl-write-message output-msg buffer)))
  env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment client event loop.

(defun run-env-event-loop (env socket buffer)
  (declare #.*optimize-settings*)
  (loop do
       (buffer-clear buffer)
       (let ((state (rl-recv-buffer socket buffer)))
         (declare (fixnum state))
         (cond
           ((= state +k-env-init+) (on-env-init env buffer))
           ((= state +k-env-start+) (on-env-start env buffer))
           ((= state +k-env-step+) (on-env-step env buffer))
           ((= state +k-env-getstate+) (on-env-get-state env buffer))
           ((= state +k-env-setstate+) (on-env-set-state env buffer))
           ((= state +k-env-getrandomseed+) (on-env-get-random-seed env buffer))
           ((= state +k-env-setrandomseed+) (on-env-set-random-seed env buffer))
           ((= state +k-env-cleanup+) (on-env-cleanup env buffer))
           ((= state +k-env-message+) (on-env-message env buffer))
           ((= state +k-rl-term+) (return))
           (t (assert nil (state) "Unknown environment state: ~D" state)))
         (rl-send-buffer socket buffer state)))
  env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment client running.

(defun run-env (env
                &key
                (host +k-localhost+)
                (port +k-default-port+)
                (max-retry nil)
                (retry-timeout +k-retry-timeout+)
                (autoreconnect nil))
  "DESCRIPTION:
    Connects the specified ENV to RL-Glue on HOST and PORT. If the
    attempt is refused, it is tried again MAX-RETRY times, waiting for
    RETRY-TIMEOUT second between them.

PARAMETERS:
    env           : environment object in use [rl-glue:environment]
    host          : host name or address [string]
                    (key parameter, default is rl-glue:+k-localhost+)
    port          : port number [0 <= integer <= 65535]
                    (key parameter, default is rl-glue:+k-default-port+)
    max-retry     : maximum number of connection trials [nil or 0 < integer]
                    (key parameter, default is nil)
    retry-timeout : duration in seconds waited between retries [0 <= integer]
                    (key parameter, default is rl-glue:+k-retry-timeout+)
    autoreconnect : reconnecting after a finished experiment [boolean]
                    (key parameter, default is nil)
RETURNS:
    (none)"
  (forced-format "RL-Glue Lisp Environment Codec Version ~a, Build ~a~%"
                 (get-codec-version) (get-svn-codec-version))
  (rl-runner env +k-environment-connection+ #'run-env-event-loop
             host port max-retry retry-timeout autoreconnect))

