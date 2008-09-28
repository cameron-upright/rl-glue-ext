
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

(in-package #:rl-glue-clcdc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State constants.

;;; RL-Glue needs to know what type of object is trying to connect.

(defparameter +k-experiment-connection+ 1)
(defparameter +k-agent-connection+ 2)
(defparameter +k-environment-connection+ 3)

;;; The server starts by sending one of these values to the client 
;;; to let it know what type of event to respond to.

(defparameter +k-agent-init+ 4)
(defparameter +k-agent-start+ 5)
(defparameter +k-agent-step+ 6)
(defparameter +k-agent-end+ 7)
(defparameter +k-agent-cleanup+ 8)
(defparameter +k-agent-freeze+ 9) ; deprecated, not handled anymore
(defparameter +k-agent-message+ 10)

(defparameter +k-env-init+ 11)
(defparameter +k-env-start+ 12)
(defparameter +k-env-step+ 13)
(defparameter +k-env-cleanup+ 14)
(defparameter +k-env-setstate+ 15)
(defparameter +k-env-setrandomseed+ 16)
(defparameter +k-env-getstate+ 17)
(defparameter +k-env-getrandomseed+ 18)
(defparameter +k-env-message+ 19)

(defparameter +k-rl-init+ 20)
(defparameter +k-rl-start+ 21)
(defparameter +k-rl-step+ 22)
(defparameter +k-rl-cleanup+ 23)
(defparameter +k-rl-return+ 24)
(defparameter +k-rl-numsteps+ 25)
(defparameter +k-rl-numepisodes+ 26)
(defparameter +k-rl-episode+ 27)
(defparameter +k-rl-setstate+ 28)
(defparameter +k-rl-setrandomseed+ 29)
(defparameter +k-rl-getstate+ 30)
(defparameter +k-rl-getrandomseed+ 31)
(defparameter +k-rl-freeze+ 32) ; deprecated, not handled anymore
(defparameter +k-rl-agent-message+ 33)
(defparameter +k-rl-env-message+ 34)

(defparameter +k-rl-term+ 35)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Network constants.

(defparameter +k-localhost+ "127.0.0.1")
(defparameter +k-default-port+ 4096)
(defparameter +k-retry-timeout+ 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Network connection.

(defmacro forced-format (fmtstr &rest args)
  "Writes to the standard output without any delay."
  `(progn
     (format *error-output* ,fmtstr ,@args)
     (force-output *error-output*)))

(defun rl-wait-for-connection (host port max-retry retry-timeout)
  "Waiting for a connection to be established. Returns the obtained socket 
descriptor on success, or nil on failure."
  (forced-format "Connecting to ~a:~a " host port)
  (loop with retry = 0 do
       (forced-format ".")
       (when max-retry
         (incf retry)
         (when (< max-retry retry)
           (forced-format " failed~%")
           (return nil)))
       (handler-case
           (let ((socket (usocket:socket-connect host port
                                                 :element-type (ubyte-type))))
             (forced-format " ok~%")
             (return socket))
         (error (e)
           (declare (ignore e)) (sleep retry-timeout)))))

(defun rl-close (socket)
  "Closes SOCKET."
  (usocket:socket-close socket))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer sending / receiving.

(defun rl-send-buffer (socket buffer state)
  "Sends the BUFFER content with STATE identifier."
  (buffer-send buffer (usocket:socket-stream socket) state)
  buffer)

(defun rl-recv-buffer (socket buffer)
  "Receives the BUFFER content and a state identifier what is returned."
  (buffer-recv buffer (usocket:socket-stream socket)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RL runner.

(defun rl-runner (obj conn-state event-loop
                  host port max-retry retry-timeout autoreconnect)
  "Connects and runs the loop of OBJ."
  (assert (or (not max-retry) (< 0 max-retry)))
  (assert (<= 0 retry-timeout))
  (let ((buffer (make-buffer)) (socket nil))
    (loop
      (unwind-protect
           (progn
             (setf socket (rl-wait-for-connection host
                                                  port
                                                  max-retry
                                                  retry-timeout))
             (when socket
               (buffer-clear buffer)
               (rl-send-buffer socket buffer conn-state)
               (funcall event-loop obj socket buffer)
        (when socket (rl-close socket)))))
      (unless autoreconnect (return))))
  obj)

