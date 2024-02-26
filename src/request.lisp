(defpackage :http/request
  (:nicknames :http/req)
  (:use :cl)
  (:import-from :alexandria-2 :line-up-last)
  (:import-from :http/util :crlf)
  (:import-from :http/header :parse-headers :render-header :header-key :header-value)
  (:export :make-req
           :render-req
           :parse-req
           :req-method
           :req-uri
           :req-protocol
           :req-headers
           :req-body
           :req-host))
(in-package :http/request)

(defclass req ()
  ((method
    :initarg :method
    :initform :GET
    :accessor req-method)
   (uri
    :initarg :uri
    :initform "/"
    :accessor req-uri)
   (protocol
    :initarg :protocol
    :initform "HTTP/1.1"
    :accessor req-protocol)
   (headers
    :initarg :headers
    :initform nil
    :accessor req-headers)
   (body
    :initarg :body
    :initform nil
    :accessor req-body)))

(defmacro make-req (&rest initargs)
  `(make-instance 'req ,@initargs))

(defun render-req (out req)
  (with-slots (method uri protocol headers body) req
    (format out "~a ~a ~a" method uri protocol)
    (crlf out)
    (dolist (h headers) (render-header out h))
    (when headers (crlf out))
    (when body (format out "~a" body) (crlf out))
    (crlf out)))

(defun parse-first-line (in req)
  (destructuring-bind (method uri protocol) (str:words (read-line in))
    (setf (req-method req) method)
    (setf (req-uri req) uri)
    (setf (req-protocol req) protocol)))

(defun parse-req (in)
  (let ((req (make-req)))
    (parse-first-line in req)
    (setf (req-headers req) (parse-headers in))
    (setf (req-body req) in)))

(defun req-host (req)
  (flet ((host-header-p (h) (string-equal "Host" (header-key h))))
    (line-up-last
     (req-headers req)
     (find-if #'host-header-p)
     (header-value)
     (str:split ":")
     (apply #'values))))
