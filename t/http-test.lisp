(defpackage :http-test
  (:use :cl :alexandria :fiveam :usocket :http)
  (:import-from :http/util :crlf))
(in-package :http-test)

(def-suite :http-test
  :description "Tests for HTTP package")
(in-suite :http-test)

(defun setup-db ()
  (mito:connect-toplevel
   :sqlite3
   :database-name "/tmp/http-test.sqlite3")
  (mito:recreate-table 'message)
  (mito:recreate-table 'request)
  (mito:recreate-table 'response))

(defun send-request (req)
  (multiple-value-bind (host port) (extract-host-and-port req)
    (let* ((conn (socket-connect host port))
           (stream (socket-stream conn)))
      (unwind-protect
           (progn (write-request stream req)
                  (force-output stream)
                  (read-response stream))
        (socket-close conn)))))

(defvar *server* nil)

(defun ok-response (stream)
  (format stream "HTTP/1.1 200 Ok")
  (crlf stream)
  (crlf stream)
  (force-output))

(defun handle-connection (conn)
  (let* ((stream (usocket:socket-stream conn))
         (req (read-request stream)))
    (declare (ignore req))
    (ok-response stream)))

(defun accept-connection (sock)
  (let ((conn (usocket:socket-accept sock)))
    (unwind-protect (handle-connection conn)
      (usocket:socket-close conn))))

(defun server-loop (host port)
  (let ((sock (usocket:socket-listen host port)))
    (unwind-protect (loop (accept-connection sock))
      (usocket:socket-close sock))))

(defmacro with-server ((host port) &body body)
  `(let ((thread (bt:make-thread (lambda () (server-loop ,host ,port)))))
     (unwind-protect (progn ,@body)
       (bt:destroy-thread thread))))

(test :client
  (let ((req (make-instance 'request :headers '((:host . "example.com:80")))))
    (is (= 200 (response-status-code (send-request req))))))

(test :server
  (with-server ("localhost" 8887)
    (let ((req (make-instance 'request :headers '((:host . "localhost:8887")))))
      (is (= 200 (response-status-code (send-request req)))))))
