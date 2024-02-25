(defpackage :http/header
  (:nicknames :http/h)
  (:use :cl)
  (:import-from :alexandria :when-let)
  (:import-from :http/util :crlf)
  (:export :make-header
           :render-header
           :parse-headers
           :header-key
           :header-value))
(in-package :http/header)

(defclass header ()
  ((key
    :initarg :key
    :initform (warn "Must specify header key.")
    :accessor header-key)
   (value
    :initarg :value
    :initform (warn "Must specify header value.")
    :accessor header-value)))

(defmacro make-header (&rest initargs)
  `(make-instance 'header ,@initargs))

(defmethod render-header (out h)
  (format out "~a: ~a" (header-key h) (header-value h))
  (crlf out))

(defun parse-header (in)
  (let ((header (make-header :key nil :value nil)))
    (when-let ((split (mapcar #'str:trim (str:split ": " (read-line in)))))
      (destructuring-bind (key value) split
        (setf (header-key header) key)
        (setf (header-value header) value)
        header))))

(defun parse-headers (in)
  (loop :for header := (parse-header in)
        :while header :collect header))
