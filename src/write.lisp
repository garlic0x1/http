(defpackage :http/write
  (:use :cl :http/types :http/util)
  (:import-from :alexandria :when-let)
  (:export :write-raw-message :write-request :write-response))
(in-package :http/write)

(defun write-raw-message (stream message)
  "Write raw message string to a binary stream."
  (loop :for byte :across (flexi-streams:string-to-octets (message-raw message))
        :do (write-byte byte stream)))

(defun write-headers (stream message)
  (dolist (h (message-headers message))
    (format stream "~a: ~a" (car h) (cdr h))
    (crlf stream)))

(defun write-body (stream message)
  (when-let ((body (message-body message)))
    (format stream "~a" body)))

(defun write-request (stream req &key to-string)
  "Write HTTP request to a binary stream or return a string."
  (let ((string
          (with-output-to-string (capture)
            (format capture "~a ~a ~a"
                    (request-method req)
                    (request-uri req)
                    (request-protocol req))
            (crlf capture)
            (write-headers capture req)
            (crlf capture)
            (write-body capture req))))
    (if to-string
        string
        (loop :for byte :across (flexi-streams:string-to-octets string)
              :do (write-byte byte stream)))))

(defun write-response (stream resp &key to-string)
  "Write HTTP response to a binary stream or return a string."
  (let ((string
          (with-output-to-string (capture)
            (format capture "~a ~a ~a"
                    (response-protocol resp)
                    (response-status-code resp)
                    (response-status resp))
            (crlf capture)
            (write-headers capture resp)
            (crlf capture)
            (write-body capture resp))))
    (if to-string
        string
        (loop :for byte :across (flexi-streams:string-to-octets string)
              :do (write-byte byte stream)))))
