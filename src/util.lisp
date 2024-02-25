(defpackage :http/util
  (:nicknames :http/u)
  (:use :cl)
  (:export :crlf))
(in-package :http/util)

(defun crlf (stream)
  (write-char #\return stream)
  (write-char #\linefeed stream)
  (values))
