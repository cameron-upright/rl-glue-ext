
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
;;; Architecture specific parameters.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +bits-per-byte+ 8)
  (deftype byte-t () `(unsigned-byte ,+bits-per-byte+))

  (defconstant +bytes-per-char+ 1)
  (defconstant +bits-per-char+ (* +bits-per-byte+ +bytes-per-char+))
  (deftype char-code-t () `(unsigned-byte ,+bits-per-char+))

  (defconstant +bytes-per-integer+ 4)
  (defconstant +bits-per-integer+ (* +bits-per-byte+ +bytes-per-integer+))
  (defconstant +max-int-pos+ (1- +bits-per-integer+))
  (defconstant +uint-limit+ (expt 2 (1+ +max-int-pos+)))
  (deftype int-code-t () `(unsigned-byte ,+bits-per-integer+))
  (deftype integer-t () `(signed-byte ,+bits-per-integer+))
  (deftype int-code-t () `(unsigned-byte ,+bits-per-integer+))

  (defconstant +expo-bits+ 11)
  (defconstant +sigd-bits+ 52)
  ;; Because of the floating point encoding and decoding optimizations,
  ;; only the 32 bit architecture is supported when the code of a double-float 
  ;; is represented by two integer codes. If one wants to add support for the
  ;; 64 bit architectures, one should reimplement the float-encoder, 
  ;; float-decoder, buffer-read-float and buffer-write-float functions.
  (defconstant +bits-per-float+ (+ 1 +expo-bits+ +sigd-bits+))
  (defconstant +bytes-per-float+ (/ +bits-per-float+ +bits-per-byte+))
  (defconstant +max-sigd+ (expt 2 +sigd-bits+))
  (defconstant +expo-offset+ (1- (expt 2 (1- +expo-bits+))))
  (deftype float-code-t () `(unsigned-byte ,+bits-per-float+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Byte buffer for network communication.

(defparameter *init-buffer-size* 2048) ; in bytes
(declaim (fixnum *init-buffer-size*))

(defclass buffer ()
  ((offset
    :accessor offset
    :initform 0
    :type fixnum
    :documentation "Position from/to where reading/writing happens.")
   (size
    :accessor size
    :initform 0
    :type fixnum
    :documentation "Number of bytes in the buffer.")
   (bytes
    :accessor bytes
    :initform (make-array *init-buffer-size*
                          :element-type 'byte-t
                          :initial-element 0)
    :type (simple-array byte-t)
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
;;; Data encoding and decoding.

(declaim (inline char-encoder))
(defun char-encoder (ch)
  "Returns the code of CH."
  (declare #.*optimize-settings*)
  (declare (type character ch))
  (char-code ch))

(declaim (inline char-decoder))
(defun char-decoder (code)
  "Returns the decoded character of CODE."
  (declare #.*optimize-settings*)
  (declare (type char-code-t code))
  (code-char code))

(declaim (inline integer-encoder))
(defun integer-encoder (int)
  "Returns the code of INT."
  (declare #.*optimize-settings*)
  (declare (type integer-t int))
  (if (logbitp +max-int-pos+ int) (+ int +uint-limit+) int))

(declaim (inline integer-decoder))
(defun integer-decoder (code)
  "Returns the decoded sigend integer of CODE."
  (declare #.*optimize-settings*)
  (declare (type int-code-t code))
  (if (logbitp +max-int-pos+ code) (- code +uint-limit+) code))

(declaim (inline float-encoder))
(defun float-encoder (float)
  "Returns the codes of FLOAT."
  (declare #.*optimize-settings*)
  (declare (type double-float float))
  (flet ((create-float-code (sign expo sigd)
           (declare #.*optimize-settings*)
           (declare (type fixnum sign expo))
           (declare (type float-code-t sigd))
           (let ((l-code 0))
             (declare (type int-code-t l-code))
             (setf (ldb (byte 1 #.(1- +bits-per-integer+)) l-code) sign)
             (setf (ldb (byte +expo-bits+ #.(- +bits-per-integer+
                                               1 +expo-bits+)) l-code) expo)
             (setf (ldb (byte #.(- +sigd-bits+ +bits-per-integer+) 0) l-code)
                   (ldb (byte #.(- +sigd-bits+ +bits-per-integer+)
                              +bits-per-integer+) sigd))
             (values l-code (ldb (byte +bits-per-integer+ 0) sigd)))))
    (if (zerop float)
        (create-float-code 0 0 0)
        (multiple-value-bind (sigd expo sign) (decode-float float)
          (declare (type double-float sigd sign))
          (declare (type fixnum expo))
          (let ((expo (+ expo #.(1- +expo-offset+)))
                (sign (if (plusp sign) 0 1)))
            (declare (type fixnum expo sign))
            (if (minusp expo)
                (create-float-code sign
                                   0
                                   (ash (round (* +max-sigd+ sigd)) expo))
                (create-float-code sign
                                   expo
                                   (round (* +max-sigd+ (1- (* sigd 2)))))))))))

(declaim (inline float-decoder))
(defun float-decoder (l-code r-code)
  "Returns the float generated from the L-CODE and R-CODE codes."
  (declare #.*optimize-settings*)
  (declare (type int-code-t l-code r-code))
  (let ((sign (ldb (byte 1 #.(1- +bits-per-integer+)) l-code))
        (expo (ldb (byte +expo-bits+ #.(- +bits-per-integer+ 1 +expo-bits+))
                   l-code))
        (l-sigd (ldb (byte #.(- +sigd-bits+ +bits-per-integer+) 0) l-code)))
    (declare (type (bit) sign))
    (declare (type (unsigned-byte #.+expo-bits+) expo))
    (declare (type (signed-byte #.(1+ +sigd-bits+)) l-sigd))
    (if (zerop expo)
        (setf expo 1)
        (setf (ldb (byte 1 #.(- +sigd-bits+ +bits-per-integer+)) l-sigd) 1))
    (let ((sigd (logior (ash l-sigd +bits-per-integer+) r-code)))
      (declare (type (unsigned-byte #.+sigd-bits+) sigd))
      (scale-float (coerce (if (zerop sign) sigd (- sigd)) 'double-float)
                   (- expo #.(+ +expo-offset+ +sigd-bits+))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper macros and functions.

(defmacro with-empty-buffer-check ((chk-p buffer type type-size) &body body)
  "Checks whether there is enough bytes in BUFFER to read an object of TYPE."
  (let ((buff buffer))
    `(if (and ,chk-p (< (- (the fixnum (size ,buff))
                            (the fixnum (offset ,buff)))
                        (the fixnum ,type-size)))
         (restart-case (error 'empty-buffer-error :otype ,type)
           (use-value (value) value))
         (progn ,@body))))

(defun auto-adjust (buffer size)
  "Checks whether the buffer has enough free space to store SIZE bytes. 
If not, it automatically adjust the buffer."
  (declare #.*optimize-settings*)
  (declare (fixnum size))
  (with-accessors ((bytes bytes)) buffer
    (let* ((buff-size (size buffer))
           (buff-capab (length (the simple-array bytes)))
           (free-size (- buff-capab buff-size)))
      (declare (fixnum buff-size buff-capab free-size))
      (when (< free-size size)
        (let ((arr (make-array (+ buff-capab (- size free-size) 1)
                               :element-type 'byte-t
                               :initial-element 0)))
          (dotimes (idx buff-size)
            (declare (fixnum idx))
            (setf (aref arr idx) (aref (the (simple-array byte-t)
                                         bytes) idx)))
          (setf bytes arr)))))
  buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read/write operations on buffers.

(defmacro buffer-read (size buffer)
  "Reads an encoded value (CODE) from BUFFER, where SIZE is the length 
of the encoded value in bytes."
  (let ((code-size (* size +bits-per-byte+)))
    `(let ((code 0))
       (declare (type (unsigned-byte ,code-size) code))
       (with-accessors ((bytes bytes) (offset offset)) ,buffer
         (dotimes (idx ,size)
           (setf code (logior (the (unsigned-byte ,code-size)
                                (ash code +bits-per-byte+))
                              (aref (the (simple-array byte-t)
                                      bytes) offset)))
           (incf (the fixnum offset))))
       code)))

(defmacro buffer-write (size code buffer)
  "Writes an encoded value (CODE) to BUFFER, where SIZE is the length 
of the encoded value in bytes."
  `(let ((code-value ,code))
     (declare (type (unsigned-byte ,(* +bits-per-byte+ size)) code-value))
     (with-accessors ((bytes bytes) (offset offset) (buffsize size)) ,buffer
       (loop
          repeat ,size
          for pos = (the fixnum ,(* +bits-per-byte+ (1- size)))
                    then (the fixnum (- pos +bits-per-byte+))
          do
            (setf (aref (the (simple-array byte-t) bytes) offset)
                  (ldb (byte +bits-per-byte+ pos) code-value))
            (incf (the fixnum offset)))
       (incf (the fixnum buffsize) ,size))
     code-value))

(defun buffer-read-char (buffer &key (buffchk-p t))
  "Reads a character from BUFFER."
  (declare #.*optimize-settings*)
  (with-empty-buffer-check (buffchk-p buffer 'character +bytes-per-char+)
    (char-decoder (buffer-read #.+bytes-per-char+ buffer))))

(defun buffer-write-char (ch buffer &key (adjust-p t))
  "Writes the CH character to BUFFER."
  (declare #.*optimize-settings*)
  (when adjust-p (auto-adjust buffer +bytes-per-char+))
  (buffer-write #.+bytes-per-char+
                (char-encoder ch)
                buffer)
  ch)

(defun buffer-read-int (buffer &key (buffchk-p t))
  "Reads an integer from BUFFER."
  (declare #.*optimize-settings*)
  (with-empty-buffer-check (buffchk-p buffer 'integer +bytes-per-integer+)
    (integer-decoder (buffer-read #.+bytes-per-integer+ buffer))))

(defun buffer-write-int (integer buffer &key (adjust-p t))
  "Writes the INTEGER integer to BUFFER."
  (declare #.*optimize-settings*)
  (declare (type integer-t integer))
  (when adjust-p (auto-adjust buffer +bytes-per-integer+))
  (buffer-write #.+bytes-per-integer+
                (integer-encoder integer)
                buffer)
  integer)

(defun buffer-read-float (buffer &key (buffchk-p t))
  "Reads a float from BUFFER."
  (declare #.*optimize-settings*)
  (with-empty-buffer-check (buffchk-p buffer 'float +bytes-per-float+)
    (float-decoder (buffer-read #.+bytes-per-integer+ buffer)
                   (buffer-read #.+bytes-per-integer+ buffer))))

(defun buffer-write-float (float buffer &key (adjust-p t))
  "Writes the FLOAT float to BUFFER."
  (declare #.*optimize-settings*)
  (declare (type double-float float))
  (when adjust-p (auto-adjust buffer +bytes-per-float+))
  (multiple-value-bind (l-code r-code) (float-encoder float)
    (buffer-write #.+bytes-per-integer+ l-code buffer)
    (buffer-write #.+bytes-per-integer+ r-code buffer))
  float)

(defmacro buffer-read-seq (elem-type elem-size
                           reader-fn buffer size buffchk-p)
  "Read a sequence of elements with type ELEM-TYPE and size ELEM-SIZE from 
BUFFER by the READER-FN function. If given, the SIZE parameter specifies the 
number of elements to be read. Otherwise it is read from the BUFFER. If 
BUFFCHK-P is T, the content of BUFFER is checked whether it contains enough 
data for the read operation."
  `(let ((arr-size (or ,size (buffer-read-int buffer))))
     (declare (type fixnum arr-size))
     (if (zerop arr-size)
         (make-array 0 :element-type ',elem-type)
         (with-empty-buffer-check (,buffchk-p
                                   ,buffer
                                   'simple-array
                                   (* ,elem-size arr-size))
           (let ((arr (make-array arr-size :element-type ',elem-type)))
             (dotimes (idx arr-size)
               (setf (aref (the (simple-array ,elem-type) arr) idx)
                     (,reader-fn ,buffer :buffchk-p ,buffchk-p)))
             arr)))))

(defmacro buffer-write-seq (seq elem-type elem-size
                            writer-fn buffer size write-size-p)
  "Write the SEQ sequence of elements with type ELEM-TYPE and size ELEM-SIZE to 
BUFFER by the WRITER-FN function. If given, the SIZE parameter specifies the 
number of elements to be written. Otherwise it is calculated from SEQ. If 
WRITE-SIZE-P is T, it is written to the BUFFER before writing the sequence."
  `(let* ((elem-num (or ,size (length (the simple-array ,seq))))
          (byte-num (* ,elem-size elem-num)))
     (declare (type fixnum elem-num byte-num))
     (if ,write-size-p
         (progn
           (auto-adjust ,buffer (the fixnum (+ +bytes-per-integer+ byte-num)))
           (buffer-write-int elem-num ,buffer :adjust-p nil))
         (auto-adjust ,buffer byte-num))
     (when (plusp elem-num)
       (loop for elem across (the (simple-array ,elem-type) ,seq)
          do (,writer-fn elem ,buffer :adjust-p nil)))
     ,seq))

(defun buffer-read-int-seq (buffer &key size (buffchk-p nil))
  "Reads an integer sequence from BUFFER."
  (declare #.*optimize-settings*)
  (buffer-read-seq integer-t +bytes-per-integer+
                   buffer-read-int buffer size buffchk-p))

(defun buffer-write-int-seq (int-seq buffer &key size (write-size-p t))
  "Writes the INT-SEQ integer sequence to BUFFER."
  (declare #.*optimize-settings*)
  (buffer-write-seq int-seq integer-t +bytes-per-integer+
                    buffer-write-int buffer size write-size-p))

(defun buffer-read-float-seq (buffer &key size (buffchk-p nil))
  "Reads a float sequence from BUFFER."
  (declare #.*optimize-settings*)
  (buffer-read-seq double-float +bytes-per-float+
                   buffer-read-float buffer size buffchk-p))

(defun buffer-write-float-seq (float-seq buffer &key size (write-size-p t))
  "Writes the FLOAT-SEQ float sequence to BUFFER."
  (declare #.*optimize-settings*)
  (buffer-write-seq float-seq double-float +bytes-per-float+
                    buffer-write-float buffer size write-size-p))

(defun buffer-read-string (buffer &key size (buffchk-p nil))
  "Reads a string (character sequence) from BUFFER."
  (declare #.*optimize-settings*)
  (buffer-read-seq character +bytes-per-char+
                   buffer-read-char buffer size buffchk-p))

(defun buffer-write-string (string buffer &key (write-size-p t))
  "Writes the STRING character sequence to BUFFER."
  (declare #.*optimize-settings*)
  (buffer-write-seq string character +bytes-per-char+
                    buffer-write-char buffer nil write-size-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sending/receiving buffer to/from a stream.

(defun read-int (stream)
  "Returns a read integer from STREAM."
  (declare #.*optimize-settings*)
  (let ((code 0))
    (declare (type int-code-t code))
    (loop repeat +bytes-per-integer+ do
         (setf code (+ (the int-code-t (ash code +bits-per-byte+))
                       (the byte-t (read-byte stream)))))
    (integer-decoder code)))

(defun write-int (int stream)
  "Writes an integer to STREAM."
  (declare #.*optimize-settings*)
  (let ((code (integer-encoder int)))
    (declare (type int-code-t code))
    (loop
       repeat +bytes-per-integer+
       for pos = #.(* +bits-per-byte+ (1- +bytes-per-integer+))
                 then (the fixnum (- pos +bits-per-byte+))
       do (write-byte (ldb (byte +bits-per-byte+ pos) code) stream)))
  stream)

(defun buffer-send (buffer stream state)
  "Sending the TARGET, the size and the bytes from BUFFER to STREAM."
  (declare #.*optimize-settings*)
  (let ((size (size buffer)))
    (declare (type fixnum size))
    ;; sending header
    (write-int state stream)
    (write-int size stream)
    ;; sending buffer content
    (dotimes (idx size)
      (write-byte (aref (the (simple-array byte-t)
                          (bytes buffer)) idx)
                  stream)))
  (force-output stream)
  buffer)

(defun buffer-recv (buffer stream)
  "Receiving the size and the bytes to BUFFER from STREAM."
  ;; receiving header
  (declare #.*optimize-settings*)
  (let ((state (read-int stream))
        (size (read-int stream)))
    (declare (type fixnum state size))
    ;; receiving buffer content
    (buffer-clear buffer)
    (auto-adjust buffer size)
    (dotimes (idx size)
      (setf (aref (the (simple-array byte-t)
                    (bytes buffer)) idx)
            (read-byte stream)))
    (setf (size buffer) size)
    state))

