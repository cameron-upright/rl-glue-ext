
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
;;; Common RL types.

(defparameter +empty-integer-array+ (make-array 0 :element-type 'integer))
(defparameter +empty-float-array+ (make-array 0 :element-type 'double-float))
(declaim (simple-vector +empty-integer-array+ +empty-float-array+))

(defclass rl-abstract-type ()
  ((int-array
    :accessor int-array
    :initarg :int-array
    :initform +empty-integer-array+
    :type simple-vector
    :documentation "Array of integer numbers.")
   (float-array
    :accessor float-array
    :initarg :float-array
    :initform +empty-float-array+
    :type simple-vector
    :documentation "Array of floating point numbers.")
   (char-string
    :accessor char-string
    :initarg :char-string
    :initform ""
    :type string
    :documentation "Character string."))
  (:documentation "General RL-Glue data representation."))

(defclass observation (rl-abstract-type)
  () (:documentation "General RL-Glue observation data representation."))

(defclass action (rl-abstract-type)
  () (:documentation "General RL-Glue action data representation."))

(defclass random-seed-key (rl-abstract-type)
  () (:documentation "General RL-Glue random seed key representation."))

(defclass state-key (rl-abstract-type)
  () (:documentation "General RL-Glue state key representation."))

(defmacro make-rl-make (type)
  "Creating a make-TYPE macro."
  (let ((typ type))
    `(defmacro ,(intern (format nil "MAKE-~a" typ)) (&rest args)
       `(make-instance ',',typ ,@args))))

(make-rl-make observation)
(make-rl-make action)
(make-rl-make random-seed-key)
(make-rl-make state-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic functions.

(defgeneric rl-equalp (object-1 object-2)
  (declare #.*optimize-settings*)
  (:documentation "Compares two RL objects."))

(defgeneric rl-read (object byte-stream)
  (declare #.*optimize-settings*)
  (:documentation "Reads an object from BYTE-STREAM."))

(defgeneric rl-write (object byte-stream)
  (declare #.*optimize-settings*)
  (:documentation "Writes an object to BYTE-STREAM."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADT read / write.

(defmethod rl-equalp ((object-1 t) (object-2 t))
  "By default it works than the equalp function."
  (declare #.*optimize-settings*)
  (equalp object-1 object-2))

(defmethod rl-equalp ((object-1 rl-abstract-type) (object-2 rl-abstract-type))
  "Compares two RL abstract data type objects."
  (declare #.*optimize-settings*)
  (and (equalp (int-array object-1) (int-array object-2))
       (equalp (float-array object-1) (float-array object-2))
       (string= (char-string object-1) (char-string object-2))))

(defmethod rl-read ((object rl-abstract-type) buffer)
  "Reads an ADT object from the buffer."
  (declare #.*optimize-settings*)
  (let ((int-num (buffer-read-int buffer))
        (float-num (buffer-read-int buffer))
        (char-num (buffer-read-int buffer)))
    (declare (type (integer 0 *) int-num float-num char-num))
    (setf (int-array object)
          (buffer-read-seq 'array #'make-array +bytes-per-integer+
                           #'buffer-read-int buffer :size int-num))
    (setf (float-array object)
          (buffer-read-seq 'array #'make-array +bytes-per-float+
                           #'buffer-read-float buffer :size float-num))
    (setf (char-string object)
          (buffer-read-string buffer :size char-num)))
  object)

(defmethod rl-write ((object rl-abstract-type) buffer)
  "Writes an ADT object to the buffer."
  (declare #.*optimize-settings*)
  (with-accessors ((int-array int-array)
                   (float-array float-array)
                   (char-string char-string)) object
    (let ((int-num (length (the vector int-array)))
          (float-num (length (the vector float-array)))
          (char-num (length (the string char-string))))
      (declare (type (integer 0 *) int-num float-num char-num))
      (buffer-write-int int-num buffer)
      (buffer-write-int float-num buffer)
      (buffer-write-int char-num buffer)
      (when (plusp int-num)
        (buffer-write-seq int-array +bytes-per-integer+
                          #'buffer-write-int buffer :size int-num))
      (when (plusp float-num)
        (buffer-write-seq float-array +bytes-per-float+
                          #'buffer-write-float buffer :size float-num))
      (when (plusp char-num)
        (buffer-write-string char-string buffer :write-size-p nil))))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RL read / write.

(defmacro make-rl-read-write (type)
  (let ((typ type))
    `(progn
       (setf (symbol-function (intern ,(format nil "RL-READ-~a" typ)))
             (lambda (buffer)
               (declare #.*optimize-settings*)
               (rl-read (make-instance ',typ) buffer)))
       (setf (symbol-function (intern ,(format nil "RL-WRITE-~a" typ)))
             (lambda (,typ buffer)
               (declare #.*optimize-settings*)
               (rl-write ,typ buffer))))))

(make-rl-read-write observation)
(make-rl-read-write action)
(make-rl-read-write random-seed-key)
(make-rl-read-write state-key)

(defun rl-read-reward (buffer)
  (declare #.*optimize-settings*)
  (buffer-read-float buffer))

(defun rl-write-reward (reward buffer)
  (declare #.*optimize-settings*)
  (buffer-write-float reward buffer))

(defun rl-read-message (buffer)
  (declare #.*optimize-settings*)
  (buffer-read-string buffer))

(defun rl-write-message (message buffer)
  (declare #.*optimize-settings*)
  (buffer-write-string message buffer))

(defun rl-read-task-spec (buffer)
  (declare #.*optimize-settings*)
  (buffer-read-string buffer))

(defun rl-write-task-spec (task-spec buffer)
  (declare #.*optimize-settings*)
  (buffer-write-string task-spec buffer))

(defun rl-read-terminal (buffer)
  (declare #.*optimize-settings*)
  (ecase (buffer-read-int buffer)
    ((0) nil)
    ((1) t)))

(defun rl-write-terminal (terminal buffer)
  (declare #.*optimize-settings*)
  (let ((boolint (if terminal 1 0)))
    (buffer-write-int boolint buffer)))

