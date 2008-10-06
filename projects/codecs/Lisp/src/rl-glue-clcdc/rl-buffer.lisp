
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
;;; Architecture specific parameters.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +bits-per-byte+ 8)
  (defparameter +bytes-per-char+ 1)
  (defparameter +bytes-per-integer+ 4)
  (defparameter +bytes-per-float+ 8)
  (declaim (type fixnum
                 +bits-per-byte+ +bytes-per-char+
                 +bytes-per-integer+ +bytes-per-float+))

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
  (declare #.*optimize-settings*)
  (declare (character ch))
  (char-code ch))

(defun char-decoder (code)
  "Returns the decoded character of CODE."
  (declare #.*optimize-settings*)
  (declare (type (integer 0 *) code))
  (code-char code))

;;; Integer encoding / decoding.

(let* ((max-ipos (1- (* +bits-per-byte+ +bytes-per-integer+)))
       (max-uint (expt 2 (1+ max-ipos)))
       (max-sint (1- (/ max-uint 2)))
       (min-sint (1- (- max-sint))))
  (declare (integer max-ipos max-uint max-sint min-sint))

  (defun integer-encoder (int)
    "Returns the code of INT."
    (declare #.*optimize-settings*)
    (declare (integer int))
    (assert (<= int max-sint) (int) "Too big integer to encode: ~D" int)
    (assert (>= int min-sint) (int) "Too little integer to encode: ~D" int)
    int)

  (defun integer-decoder (code)
    "Returns the decoded sigend integer of CODE."
    (declare #.*optimize-settings*)
    (declare (type (unsigned-byte #.(* +bits-per-byte+
                                       +bytes-per-integer+)) code))
    (if (logbitp max-ipos code) (- code max-uint) code)))

;;; Float encoding / decoding.

(let ((max-signif (expt 2 52)))
  (declare (type (integer 0 *) max-signif))

  (defun create-code (sign expo sigd)
    "Constructs the float code."
    (declare #.*optimize-settings*)
    (declare (fixnum sign expo))
    (declare (type (integer 0 *) sigd))
    (let ((code 0))
      (declare (type (unsigned-byte 64) code))
      (setf (ldb (byte 1 63) code) sign
            (ldb (byte 11 52) code) expo
            (ldb (byte 52 0) code) sigd)
      code))

  (defun float-encoder (float)
    "Returns the code of FLOAT."
    (declare #.*optimize-settings*)
    (check-type float double-float)
    (if (zerop float)
        (create-code 0 0 0)
        (multiple-value-bind (sigd expo sign) (decode-float float)
          (declare (type double-float sigd sign))
          (declare (type fixnum expo))
          (let ((expo (+ expo 1022))
                (sign (if (plusp sign) 0 1)))
            (declare (type fixnum expo sign))
            (if (minusp expo)
                (create-code sign
                             0
                             (ash (round (* max-signif sigd)) expo))
                (create-code sign
                             expo
                             (round (* max-signif (1- (* sigd 2))))))))))

  (defun float-decoder (code)
    "Returns the float of CODE."
    (declare #.*optimize-settings*)
    (declare (type (unsigned-byte 64) code))
    (let ((sign (ldb (byte 1 63) code))
          (expo (ldb (byte 11 52) code))
          (sigd (ldb (byte 52 0) code)))
      (declare (integer sign expo sigd))
      (if (zerop expo)
          (setf expo 1)
          (setf (ldb (byte 1 52) sigd) 1))
      (unless (zerop sign)
        (setf sigd (- sigd)))
      (scale-float (float sigd 1.0d0) (- expo 1075)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Byte buffer for network communication.

(defparameter *init-buffer-size* 4096) ; in bytes
(declaim (type (integer 0 *) *init-buffer-size*))

(defclass buffer ()
  ((offset
    :accessor offset
    :initform 0
    :type (integer 0 *)
    :documentation "Position from/to where reading/writing happens.")
   (size
    :accessor size
    :initform 0
    :type (integer 0 *)
    :documentation "Number of bytes in the buffer.")
   (bytes
    :accessor bytes
    :initform (make-array *init-buffer-size*
                          :element-type (ubyte-type)
                          :adjustable t)
    :type (vector #.(ubyte-type))
    :documentation "Data stored in the buffer."))
  (:documentation "An adjustable unsigned byte data buffer."))

(defun make-buffer ()
  (make-instance 'buffer))

(defun buffer-clear (buffer)
  "Clears the content of BUFFER."
  (declare #.*optimize-settings*)
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
         (if (>= (- (the (integer 0 *) (size ,buff))
                    (the (integer 0 *) (offset ,buff))) ,type-size)
             ,@body
             (restart-case (error 'empty-buffer-error :otype ,type)
               (use-value (value) value)))
         (progn ,@body))))

(defun auto-adjust (buffer size)
  "Checks whether the buffer has enough free space to store SIZE bytes. 
If not, it automatically adjust the buffer."
  (declare #.*optimize-settings*)
  (declare (type (integer 0 *) size))
  (with-accessors ((bytes bytes)) buffer
    (let* ((buff-size (size buffer))
           (buff-capab (length (the vector bytes)))
           (free-size (- buff-capab buff-size)))
      (declare (type (integer 0 *) buff-size buff-capab free-size))
      (when (< free-size size)
        (setf bytes (adjust-array bytes
                                  (+ buff-capab (- size free-size) size)
                                  :element-type (ubyte-type))))))
  buffer)

(defmacro endian-loop ((pos-name size) &body body)
  "General loop for network read/write according to the endianness of the
current architecture. POS-NAME is the name of the byte position iterator,
and SIZE is the length of the encoded value in bytes."
  (let ((pos pos-name) (ctr (gensym)))
    `(let ((,pos ,(ecase (get-endianness)
                    ((big-endian) 0)
                    ((little-endian) `(* (1- ,size) +bits-per-byte+)))))
       (declare (fixnum ,pos))
       (dotimes (,ctr ,size)
         (declare (fixnum ,ctr))
         ,@body
         (setf ,pos ,(ecase (get-endianness)
                       ((big-endian) `(+ ,pos +bits-per-byte+))
                       ((little-endian) `(- ,pos +bits-per-byte+))))))))

(defun buffer-read (size buffer)
  "Reads an encoded value (CODE) from BYTE-STREAM, where SIZE is the length 
of the encoded value in bytes."
  (declare #.*optimize-settings*)
  (declare (fixnum size))
  (let ((code 0))
    (declare (type (integer 0 *) code))
    (with-accessors ((bytes bytes) (offset offset)) buffer
      (endian-loop (pos size)
        (setf code (dpb (aref bytes offset)
                        (byte +bits-per-byte+ pos)
                        code))
        (incf (the (integer 0 *) offset))))
    code))

(defun buffer-write (size code buffer)
  "Writes an encoded value (CODE) to BYTE-STREAM, where SIZE is the length 
of the encoded value in bytes."
  (declare #.*optimize-settings*)
  (declare (fixnum size))
  (declare (type (integer 0 *) code))
  (endian-loop (pos size)
    (with-accessors ((bytes bytes) (offset offset) (size size)) buffer
      (setf (aref bytes offset) (ldb (byte +bits-per-byte+ pos) code))
      (incf (the (integer 0 *) offset))
      (incf (the (integer 0 *) size))))
  code)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read/write operations on buffers.

(defun buffer-read-char (buffer &key (buffchk-p t))
  (declare #.*optimize-settings*)
  (with-empty-buffer-check (buffchk-p buffer 'character +bytes-per-char+)
    (char-decoder (buffer-read +bytes-per-char+ buffer))))

(defun buffer-write-char (ch buffer &key (adjust-p t))
  (declare #.*optimize-settings*)
  (check-type ch character)
  (when adjust-p (auto-adjust buffer +bytes-per-char+))
  (buffer-write +bytes-per-char+
                (char-encoder ch)
                buffer)
  ch)

(defun buffer-read-int (buffer &key (buffchk-p t))
  (declare #.*optimize-settings*)
  (with-empty-buffer-check (buffchk-p buffer 'integer +bytes-per-integer+)
    (integer-decoder (buffer-read +bytes-per-integer+ buffer))))

(defun buffer-write-int (integer buffer &key (adjust-p t))
  (declare #.*optimize-settings*)
  (check-type integer integer)
  (when adjust-p (auto-adjust buffer +bytes-per-integer+))
  (buffer-write +bytes-per-integer+
                (integer-encoder integer)
                buffer)
  integer)

(defun buffer-read-float (buffer &key (buffchk-p t))
  (declare #.*optimize-settings*)
  (with-empty-buffer-check (buffchk-p buffer 'float +bytes-per-float+)
    (float-decoder (buffer-read +bytes-per-float+ buffer))))

(defun buffer-write-float (float buffer &key (adjust-p t))
  (declare #.*optimize-settings*)
  (check-type float double-float)
  (when adjust-p (auto-adjust buffer +bytes-per-float+))
  (buffer-write +bytes-per-float+
                (float-encoder (float float))
                buffer)
  float)

(defun buffer-read-seq (seq-type make-seq-fn elem-size reader-fn buffer
                        &key size (buffchk-p nil))
  (declare #.*optimize-settings*)
  (declare (function make-seq-fn reader-fn))
  (let ((size (or size (buffer-read-int buffer))))
    (declare (type (integer 0 *) size))
    (if (zerop size)
        (funcall make-seq-fn 0)
        (with-empty-buffer-check (buffchk-p buffer seq-type (* elem-size size))
          (let ((seq (funcall make-seq-fn size)))
            (declare (simple-array seq))
            (dotimes (i size)
              (setf (elt seq i) (funcall reader-fn buffer :buffchk-p nil)))
            seq)))))

(defun buffer-write-seq (seq elem-size writer-fn buffer
                         &key size write-size-p)
  (declare #.*optimize-settings*)
  (declare (vector seq))
  (declare (type (integer 0 *) elem-size))
  (declare (function writer-fn))
  (let* ((elem-num (or size (length seq)))
         (byte-num (* elem-size elem-num)))
    (declare (type (integer 0 *) elem-num byte-num))
    (if write-size-p
        (progn
          (auto-adjust buffer (+ +bytes-per-integer+ byte-num))
          (buffer-write-int elem-num buffer :adjust-p nil))
        (auto-adjust buffer byte-num))
    (when (plusp elem-num)
      (loop for elem across seq
         do (funcall writer-fn elem buffer :adjust-p nil))))
  seq)

(defun buffer-read-string (buffer &key size)
  (declare #.*optimize-settings*)
  (buffer-read-seq 'string #'make-string +bytes-per-char+
                   #'buffer-read-char buffer :size size))

(defun buffer-write-string (string buffer &key (write-size-p t))
  (declare #.*optimize-settings*)
  (check-type string string)
  (buffer-write-seq string +bytes-per-char+ #'buffer-write-char buffer
                    :write-size-p write-size-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sending/receiving buffer to/from a stream.

(defun read-int (stream)
  "Returns a read integer from STREAM."
  (declare #.*optimize-settings*)
  (let ((code 0))
    (declare (type (integer 0 *) code))
    (endian-loop (pos +bytes-per-integer+)
      (setf code (dpb (read-byte stream)
                      (byte +bits-per-byte+ pos)
                      code)))
    (integer-decoder code)))

(defun write-int (int stream)
  "Writes an integer to STREAM."
  (declare #.*optimize-settings*)
  (let ((code (integer-encoder int)))
    (declare (type (integer 0 *) code))
    (endian-loop (pos +bytes-per-integer+)
      (write-byte (ldb (byte +bits-per-byte+ pos) code) stream)))
  stream)

(defun buffer-send (buffer stream state)
  "Sending the TARGET, the size and the bytes from BUFFER to STREAM."
  (declare #.*optimize-settings*)
  (let ((size (size buffer)))
    (declare (type (integer 0 *) size))
    ;; sending header
    (write-int state stream)
    (write-int size stream)
    ;; sending buffer content
    (dotimes (idx size)
      (write-byte (aref (bytes buffer) idx) stream)))
  (force-output stream)
  buffer)

(defun buffer-recv (buffer stream)
  "Receiving the size and the bytes to BUFFER from STREAM."
  ;; receiving header
  (declare #.*optimize-settings*)
  (let ((state (read-int stream))
        (size (read-int stream)))
    (declare (type (integer 0 *) state size))
    ;; receiving buffer content
    (buffer-clear buffer)
    (auto-adjust buffer size)
    (dotimes (idx size)
      (setf (aref (bytes buffer) idx) (read-byte stream)))
    (setf (size buffer) size)
    state))

