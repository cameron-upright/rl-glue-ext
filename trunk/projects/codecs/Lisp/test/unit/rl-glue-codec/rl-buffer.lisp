
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

(fiveam:def-suite buffer-suite
    :description "Tests for buffer handling."
    :in main-suite)

(fiveam:in-suite buffer-suite)

(defparameter *test-char-code-limit* 256)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((*random-state* (make-random-state t)))
  (fiveam:test char-encode/decode
    "Test of character encode/decode operations."
    (fiveam:for-all ((ch (fiveam:gen-character :code-limit
                                               *test-char-code-limit*)))
      (fiveam:is (char= ch (char-decoder (char-encoder ch))))))
  (fiveam:test integer-encode/decode
    "Test of integer encode/decode operations."
    (fiveam:for-all ((int (fiveam:gen-integer)))
      (fiveam:is (= int (integer-decoder (integer-encoder int))))))
  (fiveam:test float-encode/decode
    "Test of float encode/decode operations."
    (fiveam:for-all ((fl (fiveam:gen-float :type 'double-float)))
      (fiveam:is (= fl (multiple-value-call #'float-decoder
                         (float-encoder fl)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((*random-state* (make-random-state t))
      (*init-buffer-size* 4))
  (fiveam:test basic-read/write
    "Basic read/write operations on buffers."
    (let ((buffer (make-buffer)))
      (fiveam:is (= *init-buffer-size*
                    (length (buffer-bytes buffer))))
      ;; dirty read/write
      (fiveam:signals empty-buffer-error
        (buffer-read-char buffer))
      (fiveam:signals empty-buffer-error
        (buffer-read-int buffer))
      (fiveam:signals empty-buffer-error
        (buffer-read-float buffer))
      (fiveam:is (eq 'any-value (handler-bind
                                    ((error #'(lambda (condition)
                                                (declare (ignore condition))
                                                (invoke-restart 'use-value
                                                                'any-value))))
                                  (buffer-read-float buffer))))
      ;; clean read/write
      (fiveam:for-all ((ch (fiveam:gen-character :code-limit
                                                 *test-char-code-limit*))
                       (int (fiveam:gen-integer))
                       (fl (fiveam:gen-float :type 'double-float)))
        (buffer-clear buffer)
        (buffer-write-char ch buffer)
        (buffer-write-int int buffer)
        (buffer-write-float fl buffer)
        (fiveam:is (= (buffer-size buffer)
                      (+ +bytes-per-char+
                         +bytes-per-integer+
                         +bytes-per-float+)))
        (setf (buffer-offset buffer) 0)
        (fiveam:is (char= ch (buffer-read-char buffer)))
        (fiveam:is (= int (buffer-read-int buffer)))
        (fiveam:is (= fl (buffer-read-float buffer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO : seq-read/write, string-read/write

