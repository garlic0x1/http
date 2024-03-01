(defpackage :http/write
  (:use :cl :http/types :http/util)
  (:import-from :alexandria :when-let)
  (:export :write-raw-message
           :write-request
           :write-response))
(in-package :http/write)

(defun write-raw-message (stream message)
  (format stream "~a" (message-raw message)))

(defun write-headers (stream message)
  (dolist (h (message-headers message))
    (format stream "~a: ~a" (car h) (cdr h))
    (crlf stream)))

(defun write-body (stream message)
  (when-let ((body (message-body message)))
    (format stream "~a" body)))

(defun write-request (stream req)
  (with-nil-to-string (stream)
    (format stream "~a ~a ~a"
            (request-method req)
            (request-uri req)
            (request-protocol req))
    (crlf stream)
    (write-headers stream req)
    (crlf stream)
    (write-body stream req)))

(defun write-response (stream resp)
  (with-nil-to-string (stream)
    (format stream "~a ~a ~a"
            (response-protocol resp)
            (response-status-code resp)
            (response-status resp))
    (crlf stream)
    (write-headers stream resp)
    (crlf stream)
    (write-body stream resp)))
