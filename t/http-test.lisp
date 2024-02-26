(defpackage :http-test
  (:use :cl :fiveam :usocket :http))
(in-package :http-test)

(def-suite :http-test
  :description "Tests for HTTP package")
(in-suite :http-test)

(defun http-get (host port)
  (send-req (make-req :headers (list (make-header :key "Host" :value host)))
            host
            port))

(test :client
  (let ((resp (http-get "example.com" 80)))
    (is (= 200 (resp-status-code resp)))))
