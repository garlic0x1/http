(defpackage :http/write
  (:use :cl :http/types :http/util)
  (:import-from :alexandria :when-let)
  (:export :write-raw-message
           :write-request
           :write-response))
(in-package :http/write)

(defun write-raw-message (stream message)
  (format stream "~a" (message-raw message)))

(defun write-request (stream req)
  (format stream "~a ~a ~a"
          (request-method req)
          (request-uri req)
          (request-protocol req))
  (crlf stream)
  (dolist (h (message-headers req))
    (format stream "~a: ~a" (car h) (cdr h))
    (crlf stream))
  (crlf stream)
  (when-let ((body (message-body req)))
    (format stream "~a" body)))

(defun write-response (stream resp)
  (format stream "~a ~a ~a"
          (response-protocol resp)
          (response-status-code resp)
          (response-status resp))
  (crlf stream)
  (dolist (h (message-headers resp))
    (format stream "~a: ~a" (car h) (cdr h))
    (crlf stream))
  (crlf stream)
  (when-let ((body (message-body resp)))
    (format stream "~a" body)))
