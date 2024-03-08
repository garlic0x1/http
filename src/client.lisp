(defpackage :http/client
  (:use :cl :http/types :http/util)
  (:local-nicknames (:us :usocket))
  (:import-from :alexandria :assoc-value)
  (:export :send-request))
(in-package :http/client)

(defun extract-host-and-port (message)
  (let* ((host (assoc-value (message-headers message) :host))
         (split (str:split ":" host :limit 2)))
    (values (first split)
            (parse-integer (or (second split) "80")))))

(defun send-request (req &key raw)
  (multiple-value-bind (host port) (extract-host-and-port req)
    (let* ((conn (us:socket-connect host port))
           (stream (us:socket-stream conn)))
      (unwind-protect
           (progn (if raw
                      (write-string (message-raw req) stream)
                      (http/write:write-request stream req))
                  (force-output stream)
                  (http/read:read-response stream))
        (us:socket-close conn)))))
