(defpackage :http/client
  (:nicknames :http/c)
  (:use :cl)
  (:export :send-req))
(in-package :http/client)

(defun send-req (req host port)
  (let* ((sock (usocket:socket-connect host port))
         (stream (usocket:socket-stream sock)))
    (unwind-protect
         (progn
           (http/req:render-req stream req)
           (force-output stream)
           (http/resp:parse-resp stream))
      (usocket:socket-close sock))))
