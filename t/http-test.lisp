(defpackage :http-test
  (:use :cl :alexandria :fiveam :http)
  (:local-nicknames (:us :usocket))
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

(defun canary-response (stream)
  (format stream "HTTP/1.1 200 Canary")
  (crlf stream 2)
  (force-output stream))

(defun handle-connection (conn)
  (let* ((stream (us:socket-stream conn))
         (req (read-request stream)))
    (declare (ignore req))
    (canary-response stream)))

(defmacro with-server ((host port handler) &body body)
  `(let ((thread (bt:make-thread
                  (lambda ()
                    (http/server::server-loop ,host ,port ,handler)))))
     (unwind-protect (progn (sleep 0.2) ,@body)
       (ignore-errors (bt:destroy-thread thread)))))

;; requires internet connection
(test :client
  (let* ((req (make-instance 'request :headers '((:host . "example.com:80"))))
         (resp (send-request req)))
    (is (= 200 (response-status-code resp)))
    (is (not (str:emptyp (message-body resp))))))

(test :server
  (with-server ("127.0.0.1" 8887 'handle-connection)
    (let ((req (make-instance 'request :headers '((:host . "127.0.0.1:8887")))))
      (is (equal "Canary" (response-status (send-request req)))))))

(test :write
  (let ((req (make-instance 'request)))
    (is (str:starts-with-p "GET / HTTP/1.1" (write-request nil req)))))

(test :read
  (let ((stream
          (make-string-input-stream
           (with-output-to-string (capture)
             (format capture "GET /bruh HTTP/1.1")
             (crlf capture 2)))))
    (is (equal "/bruh"
               (puri:render-uri
                (request-uri (read-request stream))
                nil)))))
