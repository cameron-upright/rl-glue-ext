
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

(in-package #:rl-glue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architecture specific parameters.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +bits-per-byte+ 8)
  (defparameter +bytes-per-char+ 1)
  (defparameter +bytes-per-integer+ 4)
  (defparameter +bytes-per-float+ 8)

  (defmacro ubyte-type ()
    `'(unsigned-byte ,+bits-per-byte+))

  (setf (symbol-function 'get-endianness)
        #'(lambda ()
            (case (ldb (byte +bits-per-byte+ 0) 511)
              ((255) 'little-endian)
              ((1) 'big-endian)
              (t (error "Not supported endian type."))))))

;;; Character encoding / decoding

(defun char-encoder (ch)
  "Returns the code of CH."
  (char-code ch))

(defun char-decoder (code)
  "Returns the decoded character of CODE."
  (code-char code))

;;; Integer encoding / decoding.

(let* ((max-ipos (1- (* +bits-per-byte+ +bytes-per-integer+)))
       (max-uint (expt 2 (1+ max-ipos)))
       (max-sint (1- (/ max-uint 2)))
       (min-sint (1- (- max-sint))))

  (defun integer-encoder (int)
    "Returns the code of INT."
    (assert (<= int max-sint) (int) "Too big integer to encode: ~D" int)
    (assert (>= int min-sint) (int) "Too little integer to encode: ~D" int)
    int)

  (defun integer-decoder (code)
    "Returns the decoded sigend integer of CODE."
    (if (logbitp max-ipos code) (- code max-uint) code)))

;;; Float encoding / decoding.

(defun float-encoder (float)
  "Returns the code of FLOAT."
  (ieee-floats:encode-float64 float))

(defun float-decoder (code)
  "Returns the float of CODE."
  (ieee-floats:decode-float64 code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Byte buffer for network communication.

(defparameter *init-buffer-size* 4096) ; in bytes

(defclass buffer ()
  ((offset
    :accessor offset
    :initform 0
    :documentation "Position from/to where reading/writing happens.")
   (size
    :accessor size
    :initform 0
    :documentation "Number of bytes in the buffer.")
   (bytes
    :accessor bytes
    :initform (make-array *init-buffer-size*
                          :element-type (ubyte-type)
                          :adjustable t)))
  (:documentation "An adjustable unsigned byte data buffer."))

(defun make-buffer ()
  (make-instance 'buffer))

(defun buffer-clear (buffer)
  "Clears the content of BUFFER."
  (setf (size buffer) 0)
  (setf (offset buffer) 0)
  buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions.

(define-condition empty-buffer-error (error)
  ((otype :reader otype :initarg :otype))
  (:documentation "Raised if not enough data to read an object of OTYPE.")
  (:report (lambda (condition stream)
             (format stream "Not enough buffer data to read type ~A."
                     (otype condition)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper macros and functions.

(defmacro with-empty-buffer-check ((chk-p buffer type type-size) &body body)
  "Checks whether there is enough bytes in BUFFER to read an object of TYPE."
  (let ((buff buffer))
    `(if ,chk-p
         (if (>= (- (size ,buff) (offset ,buff)) ,type-size)
             ,@body
             (restart-case (error 'empty-buffer-error :otype ,type)
               (use-value (value) value)))
         (progn ,@body))))

(defun auto-adjust (buffer size)
  "Checks whether the buffer has enough free space to store SIZE bytes. 
If not, it automatically adjust the buffer."
  (with-accessors ((bytes bytes)) buffer
    (let* ((buff-size (size buffer))
           (buff-capab (length bytes))
           (free-size (- buff-capab buff-size)))
      (when (< free-size size)
        (setf bytes (adjust-array bytes
                                  (+ buff-capab (- size free-size) size)
                                  :element-type (ubyte-type))))))
  buffer)

(defmacro endian-loop ((pos-name size) &body body)
  "General loop for network read/write according to the endianness of the
current architecture. POS-NAME is the name of the byte position iterator,
and SIZE is the length of the encoded value in bytes."
  (let ((pos pos-name))
    `(loop
        repeat ,size
        ,@(ecase (get-endianness)
            ((big-endian) `(for ,pos = 0 then (+ ,pos +bits-per-byte+)))
            ((little-endian) `(for ,pos = (* (1- ,size) +bits-per-byte+)
                                   then (- ,pos +bits-per-byte+))))
        do ,@body)))

(defun buffer-read (size buffer)
  "Reads an encoded value (CODE) from BYTE-STREAM, where SIZE is the length 
of the encoded value in bytes."
  (let ((code 0))
    (with-accessors ((bytes bytes) (offset offset)) buffer
      (endian-loop (pos size)
        (setf code (dpb (aref bytes offset)
                        (byte +bits-per-byte+ pos)
                        code))
        (incf offset)))
    code))

(defun buffer-write (size code buffer)
  "Writes an encoded value (CODE) to BYTE-STREAM, where SIZE is the length 
of the encoded value in bytes."
  (endian-loop (pos size)
    (with-accessors ((bytes bytes) (offset offset) (size size)) buffer
      (setf (aref bytes offset) (ldb (byte +bits-per-byte+ pos) code))
      (incf offset)
      (incf size)))
  code)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read/write operations on buffers.

(defun buffer-read-char (buffer &key (buffchk-p t))
  (with-empty-buffer-check (buffchk-p buffer 'character +bytes-per-char+)
    (char-decoder (buffer-read +bytes-per-char+ buffer))))

(defun buffer-write-char (ch buffer &key (adjust-p t))
  (check-type ch character)
  (when adjust-p (auto-adjust buffer +bytes-per-char+))
  (buffer-write +bytes-per-char+
                (char-encoder ch)
                buffer)
  ch)

(defun buffer-read-int (buffer &key (buffchk-p t))
  (with-empty-buffer-check (buffchk-p buffer 'integer +bytes-per-integer+)
    (integer-decoder (buffer-read +bytes-per-integer+ buffer))))

(defun buffer-write-int (integer buffer &key (adjust-p t))
  (check-type integer integer)
  (when adjust-p (auto-adjust buffer +bytes-per-integer+))
  (buffer-write +bytes-per-integer+
                (integer-encoder integer)
                buffer)
  integer)

(defun buffer-read-float (buffer &key (buffchk-p t))
  (with-empty-buffer-check (buffchk-p buffer 'float +bytes-per-float+)
    (float-decoder (buffer-read +bytes-per-float+ buffer))))

(defun buffer-write-float (float buffer &key (adjust-p t))
  (check-type float number)
  (when adjust-p (auto-adjust buffer +bytes-per-float+))
  (buffer-write +bytes-per-float+
                (float-encoder (float float))
                buffer)
  float)

(defun buffer-read-seq (seq-type make-seq-fn elem-size reader-fn buffer
                        &key size (buffchk-p nil))
  (let ((size (or size (buffer-read-int buffer))))
    (if (zerop size)
        (funcall make-seq-fn 0)
        (with-empty-buffer-check (buffchk-p buffer seq-type (* elem-size size))
          (let ((seq (funcall make-seq-fn size)))
            (loop
               repeat size
               for i from 0
               do (setf (elt seq i) (funcall reader-fn buffer :buffchk-p nil)))
            seq)))))

(defun buffer-write-seq (seq writer-fn buffer
                         &key size write-size-p)
  (let ((size (or size (length seq))))
    (if write-size-p
        (progn
          (auto-adjust buffer (+ +bytes-per-integer+ size))
          (buffer-write-int size buffer :adjust-p nil))
        (auto-adjust buffer size))
    (when (plusp size)
      (loop for elem across seq
         do (funcall writer-fn elem buffer :adjust-p nil))))
  seq)

(defun buffer-read-string (buffer &key size)
  (buffer-read-seq 'string #'make-string +bytes-per-char+
                   #'buffer-read-char buffer :size size))

(defun buffer-write-string (string buffer &key (write-size-p t))
  (check-type string string)
  (buffer-write-seq string #'buffer-write-char buffer
                    :write-size-p write-size-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sending/receiving buffer to/from a stream.

(defun read-int (stream)
  "Returns a read integer from STREAM."
  (let ((code 0))
    (endian-loop (pos +bytes-per-integer+)
      (setf code (dpb (read-byte stream)
                      (byte +bits-per-byte+ pos)
                      code)))
    (integer-decoder code)))

(defun write-int (int stream)
  "Writes an integer to STREAM."
  (let ((code (integer-encoder int)))
    (endian-loop (pos +bytes-per-integer+)
      (write-byte (ldb (byte +bits-per-byte+ pos) code) stream)))
  stream)

(defun buffer-send (buffer stream state)
  "Sending the TARGET, the size and the bytes from BUFFER to STREAM."
  (let ((size (size buffer)))
    ;; sending header
    (write-int state stream)
    (write-int size stream)
    ;; sending buffer content
    (loop
       repeat size
       for idx from 0
       do (write-byte (aref (bytes buffer) idx) stream)))
  (force-output stream)
  buffer)

(defun buffer-recv (buffer stream)
  "Receiving the size and the bytes to BUFFER from STREAM."
  (with-accessors ((buffsize size)) buffer
    ;; receiving header
    (let ((state (read-int stream))
          (size (read-int stream)))
      ;; receiving buffer content
      (buffer-clear buffer)
      (setf buffsize size)
      (auto-adjust buffer buffsize)
      (loop
         repeat size
         for idx from 0
         do (setf (aref (bytes buffer) idx) (read-byte stream)))
      state)))

