(defpackage :http/server
  (:use :cl :http/types :http/util)
  (:local-nicknames (:us :usocket))
  (:export :start-server
           :stop-server))
(in-package :http/server)

(defvar *halt* nil)

(defun accept-connection (sock handler)
  (let ((conn (us:socket-accept sock)))
    (bt:make-thread
     (lambda ()
       (unwind-protect (funcall handler conn)
         (us:socket-close conn))))))

(defun server-loop (host port handler)
  (let ((sock (us:socket-listen host port :reuse-address t)))
    (unwind-protect
         (loop :while (not *halt*)
               :do (accept-connection sock handler))
      (us:socket-close sock))))


(defun stop-server ()
  (setf *halt* t))

(defun start-server (&key (host "127.0.0.1")
                          (port 5000)
                          (handler (error "Must provide handler.")))
  (setf *halt* nil)
  (bt:make-thread
   (lambda ()
     (server-loop host port handler))))
