
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

(defmacro make-adt-array (size type initial-contents)
  `(make-array ,size
               :element-type ,type
               ,@(when initial-contents
                   `(:initial-contents ,initial-contents))))

(defmacro make-int-array (size &key initial-contents)
  `(make-adt-array ,size 'integer-t ,initial-contents))

(defmacro make-float-array (size &key initial-contents)
  `(make-adt-array ,size 'double-float ,initial-contents))

(defparameter *init-integer-array* (make-int-array 0))
(defparameter *init-float-array* (make-float-array 0))
(declaim (simple-array +empty-integer-array+ +empty-float-array+))

(defclass rl-abstract-type ()
  ((int-array
    :accessor int-array
    :initarg :int-array
    :initform *init-integer-array*
    :type simple-array
    :documentation "Array of integer numbers.")
   (float-array
    :accessor float-array
    :initarg :float-array
    :initform *init-float-array*
    :type simple-array
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

(defmethod rl-read ((self rl-abstract-type) buffer)
  "Reads an ADT object from the buffer."
  (declare #.*optimize-settings*)
  (let ((int-num (buffer-read-int buffer))
        (float-num (buffer-read-int buffer))
        (char-num (buffer-read-int buffer)))
    (declare (fixnum int-num float-num char-num))
    (setf (int-array self) (buffer-read-int-seq buffer :size int-num))
    (setf (float-array self) (buffer-read-float-seq buffer :size float-num))
    (setf (char-string self) (buffer-read-string buffer :size char-num)))
  self)

(defmethod rl-write ((self rl-abstract-type) buffer)
  "Writes an ADT object to the buffer."
  (declare #.*optimize-settings*)
  (with-accessors ((int-array int-array)
                   (float-array float-array)
                   (char-string char-string)) self
    (check-type int-array (simple-array integer-t))
    (check-type float-array (simple-array double-float))
    (check-type char-string string)
    (let ((int-num (length (the simple-array int-array)))
          (float-num (length (the simple-array float-array)))
          (char-num (length (the string char-string))))
      (declare (fixnum int-num float-num char-num))
      (buffer-write-int int-num buffer)
      (buffer-write-int float-num buffer)
      (buffer-write-int char-num buffer)
      (when (plusp int-num)
        (buffer-write-int-seq int-array buffer
                              :size int-num :write-size-p nil))
      (when (plusp float-num)
        (buffer-write-float-seq float-array buffer
                                :size float-num :write-size-p nil))
      (when (plusp char-num)
        (buffer-write-string char-string buffer :write-size-p nil))))
  self)

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
  (check-type reward number)
  (buffer-write-float (coerce reward 'double-float) buffer))

(defun rl-read-message (buffer)
  (declare #.*optimize-settings*)
  (buffer-read-string buffer))

(defun rl-write-message (message buffer)
  (declare #.*optimize-settings*)
  (check-type message string)
  (buffer-write-string message buffer))

(defun rl-read-task-spec (buffer)
  (declare #.*optimize-settings*)
  (buffer-read-string buffer))

(defun rl-write-task-spec (task-spec buffer)
  (declare #.*optimize-settings*)
  (check-type task-spec string)
  (buffer-write-string task-spec buffer))

(defun rl-read-terminal (buffer)
  (declare #.*optimize-settings*)
  (ecase (buffer-read-int buffer)
    ((0) nil)
    ((1) t)))

(defun rl-write-terminal (terminal buffer)
  (declare #.*optimize-settings*)
  (check-type terminal boolean)
  (let ((boolint (if terminal 1 0)))
    (buffer-write-int boolint buffer)))

