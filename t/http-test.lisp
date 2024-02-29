(defpackage :http-test
  (:use :cl :fiveam :usocket :http))
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
