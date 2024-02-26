(defpackage :http/client
  (:nicknames :http/c)
  (:use :cl)
  (:import-from :http/url :url-host :url-port :parse-url)
  (:shadow :get)
  (:export :send-req
           :get
           :post))
(in-package :http/client)

(defun send-req (req)
  (multiple-value-bind (host port) (http/req:req-host req)
    (let* ((sock (usocket:socket-connect host port))
           (stream (usocket:socket-stream sock)))
      (unwind-protect
           (progn
             (http/req:render-req stream req)
             (force-output stream)
             (http/resp:parse-resp stream))
        (usocket:socket-close sock)))))

(defun get (uri &key headers)
  (let* ((url (parse-url uri))
         (host (format nil "~a:~a" (url-host url) (url-port url)))
         (host-header (http/h:make-header :key "Host" :value host)))
    (send-req (http/req:make-req
               :method :get
               :headers (cons host-header headers)))))

(defun post (uri &key headers body)
  (let* ((url (parse-url uri))
         (host (format nil "~a:~a" (url-host url) (url-port url)))
         (host-header (http/h:make-header :key "Host" :value host)))
    (send-req (http/req:make-req
               :method :post
               :headers (cons host-header headers)
               :body body))))
