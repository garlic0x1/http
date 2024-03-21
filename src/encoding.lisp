(defpackage :http/encoding
  (:use :cl)
  (:import-from :alexandria :assoc-value)
  (:export :decompress-string))
(in-package :http/encoding)

(defun read-encoding (string)
  (cond ((string= "gzip" string) 'chipz:gzip)
        ((string= "deflate" string) 'chipz:deflate)))

(defparameter *last* nil)
(defun decompress-string (string headers)
  (setf *last*
        (flexi-streams:octets-to-string
         (reduce (lambda (acc encoding)
                   (chipz:decompress nil (read-encoding encoding) acc))
                 (str:words (assoc-value headers :content-encoding))
                 :initial-value (flexi-streams:string-to-octets string)))))
