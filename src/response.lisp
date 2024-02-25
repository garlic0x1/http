(defpackage :http/response
  (:nicknames :http/resp)
  (:use :cl)
  (:import-from :http/util :crlf)
  (:import-from :http/header :render-header :parse-headers)
  (:export :make-resp
           :render-resp
           :parse-resp
           :resp-method
           :resp-uri
           :resp-protocol
           :resp-headers
           :resp-body))
(in-package :http/response)

(defclass resp ()
  ((protocol
    :initarg :protocol
    :initform 200
    :accessor resp-protocol)
   (status-code
    :initarg :status-code
    :initform 200
    :accessor resp-status-code)
   (status
    :initarg :status
    :initform ""
    :accessor resp-status)
   (headers
    :initarg :headers
    :initform '()
    :accessor resp-headers)
   (body
    :initarg :body
    :initform nil
    :accessor resp-body)))

(defmacro make-resp (&rest initargs)
  `(make-instance 'resp ,@initargs))

(defun render-resp (out resp)
  (with-slots (protocol status-code status headers body) resp
    (format out "~a ~a ~a" protocol status-code status)
    (crlf out)
    (dolist (h headers) (render-header out h))
    (when headers (crlf out))
    (when body (format out "~a" body) (crlf out))
    (crlf out)))

(defun parse-first-line (in resp)
  (destructuring-bind (protocol status-code status) (str:words (read-line in))
    (setf (resp-protocol resp) protocol)
    (setf (resp-status-code resp) (parse-integer status-code))
    (setf (resp-status resp) status)))

(defun parse-resp (in)
  (let ((resp (make-resp)))
    (parse-first-line in resp)
    (setf (resp-headers resp) (parse-headers in))
    (setf (resp-body resp) in)))
