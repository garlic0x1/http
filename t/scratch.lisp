(defpackage :http/scratch
  (:use :cl)
  (:local-nicknames (:us :usocket)))
(in-package :http/scratch)

(defparameter *last-line* nil)
(defparameter *last-head* nil)

(defun handle-connection (conn)
  (let* ((stream (us:socket-stream conn)))
    (setf *last-line* (chunga:read-line* stream))
    (setf *last-head* (chunga:read-http-headers stream))))
