(defpackage :http-test
  (:use :cl :alexandria :fiveam :usocket :http))
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
