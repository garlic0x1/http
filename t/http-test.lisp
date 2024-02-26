(defpackage :http-test
  (:use :cl :fiveam :usocket :http))
(in-package :http-test)

(def-suite :http-test
  :description "Tests for HTTP package")
(in-suite :http-test)

(defun http-get (host port)
  (let* ((sock (socket-connect host port))
         (stream (socket-stream sock))
         (req (make-req :headers (list (make-header :key "Host" :value host)))))
    (unwind-protect
         (progn
           (render-req stream req)
           (force-output stream)
           (parse-resp stream))
      (socket-close sock))))

(test :client
  (let ((resp (http-get "example.com" 80)))
    (is (= 200 (resp-status-code resp)))))
