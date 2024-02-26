(defpackage :http/client
  (:nicknames :http/c)
  (:use :cl)
  (:import-from :http/url :url-host :url-port :parse-url :extract-url-host-and-port)
  (:import-from :http/request :make-req :render-req :req-host :req-uri)
  (:import-from :http/response :parse-resp)
  (:import-from :http/header :make-header)
  (:shadow :get)
  (:export :send-req
           :get
           :post))
(in-package :http/client)

(defun send-req (req)
  (multiple-value-bind (host port) (extract-url-host-and-port (req-uri req))
    (let* ((sock (usocket:socket-connect host port))
           (stream (usocket:socket-stream sock)))
      (unwind-protect
           (progn
             (render-req stream req)
             (force-output stream)
             (parse-resp stream))
        (usocket:socket-close sock)))))

(defun get (uri &key headers)
  (multiple-value-bind (host port) (extract-url-host-and-port uri)
    (let* ((host (format nil "~a:~a" host port))
           (host-header (make-header :key "Host" :value host)))
      (send-req (make-req
                 :method :get
                 :uri uri
                 :headers (cons host-header headers))))))

(defun post (uri &key headers body)
  (let* ((url (parse-url uri))
         (host (format nil "~a:~a" (url-host url) (url-port url)))
         (host-header (make-header :key "Host" :value host)))
    (send-req (make-req
               :method :post
               :uri uri
               :headers (cons host-header headers)
               :body body))))
