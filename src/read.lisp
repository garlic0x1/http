(defpackage :http/read
  (:use :cl :http/types :http/util)
  (:import-from :alexandria :assoc-value :when-let)
  (:export :read-request
           :read-response
           :extract-host-and-port))
(in-package :http/read)

(defvar *capture* nil
  "Lexical capture stream for raw message.")

(defun read-status-line (stream)
  (let ((line (chunga:read-line* stream)))
    (write-line line *capture*)
    (apply #'values (mapcar #'str:trim (str:words line :limit 3)))))

(defun read-headers (stream)
  (flet ((split-header (line)
           (let ((split (mapcar #'str:trim (str:split ":" line :limit 2))))
             (cons (make-keyword (first split)) (second split)))))
    (loop :with headers := '()
          :for line := (chunga:read-line* stream)
          :do (write-line line *capture*)
          :while (not (str:emptyp (str:trim line)))
          :do (push (split-header line) headers)
          :finally (return headers))))

(defun read-length (stream length)
  (with-output-to-string (capture)
    (dotimes (i length)
      (when-let ((c (chunga:read-char* stream)))
        (write-char c *capture*)
        (write-char c capture)))))

(defun read-body (stream headers)
  (let ((length (assoc-value headers :content-length))
        (type (assoc-value headers :content-type)))
    (declare (ignore type))
    (cond (length (read-length stream (parse-integer length)))
          (t ""))))

(defun read-request (stream)
  (let ((req (make-instance 'request)))
    (setf (message-raw req)
          (with-output-to-string (capture)
            (let ((*capture* capture))
              (multiple-value-bind (method uri protocol) (read-status-line stream)
                (setf (request-method req) method
                      (request-uri req) (puri:parse-uri uri)
                      (request-protocol req) protocol))
              (let ((headers (read-headers stream)))
                (setf (message-headers req) headers
                      (message-body req) (read-body stream headers))))))
    req))

(defun read-response (stream)
  (let ((resp (make-instance 'response)))
    (setf (message-raw resp)
          (with-output-to-string (capture)
            (let ((*capture* capture))
              (multiple-value-bind (protocol code status) (read-status-line stream)
                (setf (response-protocol resp) protocol
                      (response-status-code resp) (parse-integer code)
                      (response-status resp) status))
              (let ((headers (read-headers stream)))
                (setf (message-headers resp) headers
                      (message-body resp) (read-body stream headers))))))
    resp))
