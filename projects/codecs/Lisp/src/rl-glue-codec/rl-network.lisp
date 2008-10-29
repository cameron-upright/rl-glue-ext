
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
;;; State constants.

;;; RL-Glue needs to know what type of object is trying to connect.

(defconstant +k-experiment-connection+ 1)
(defconstant +k-agent-connection+ 2)
(defconstant +k-environment-connection+ 3)

;;; The server starts by sending one of these values to the client 
;;; to let it know what type of event to respond to.

(defconstant +k-agent-init+ 4)
(defconstant +k-agent-start+ 5)
(defconstant +k-agent-step+ 6)
(defconstant +k-agent-end+ 7)
(defconstant +k-agent-cleanup+ 8)
(defconstant +k-agent-freeze+ 9) ; deprecated, not handled anymore
(defconstant +k-agent-message+ 10)

(defconstant +k-env-init+ 11)
(defconstant +k-env-start+ 12)
(defconstant +k-env-step+ 13)
(defconstant +k-env-cleanup+ 14)
(defconstant +k-env-load-state+ 15)
(defconstant +k-env-load-random-seed+ 16)
(defconstant +k-env-save-state+ 17)
(defconstant +k-env-save-random-seed+ 18)
(defconstant +k-env-message+ 19)

(defconstant +k-rl-init+ 20)
(defconstant +k-rl-start+ 21)
(defconstant +k-rl-step+ 22)
(defconstant +k-rl-cleanup+ 23)
(defconstant +k-rl-return+ 24)
(defconstant +k-rl-numsteps+ 25)
(defconstant +k-rl-numepisodes+ 26)
(defconstant +k-rl-episode+ 27)
(defconstant +k-rl-load-state+ 28)
(defconstant +k-rl-load-random-seed+ 29)
(defconstant +k-rl-save-state+ 30)
(defconstant +k-rl-save-random-seed+ 31)
(defconstant +k-rl-freeze+ 32) ; deprecated, not handled anymore
(defconstant +k-rl-agent-message+ 33)
(defconstant +k-rl-env-message+ 34)
(defconstant +k-rl-term+ 35)

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
     (format *standard-output* ,fmtstr ,@args)
     (force-output *standard-output*)))

(defun rl-wait-for-connection (host port max-retry retry-timeout)
  "Waiting for a connection to be established. Returns the obtained socket 
descriptor on success, or nil on failure."
  (forced-format "        Connecting to ~a:~a " host port)
  (loop with retry = 0 do
       (forced-format ".")
       (when max-retry
         (incf retry)
         (when (< max-retry retry)
           (forced-format " failed~%")
           (return nil)))
       (handler-case
           (let ((socket (usocket:socket-connect host port
                                                 :element-type 'byte-t)))
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
  (declare #.*optimize-settings*)
  (buffer-send buffer (usocket:socket-stream socket) state)
  buffer)

(defun rl-recv-buffer (socket buffer)
  "Receives the BUFFER content and a state identifier what is returned."
  (declare #.*optimize-settings*)
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

