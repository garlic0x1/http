(defpackage :http/encoding
  (:use :cl)
  (:import-from :alexandria :assoc-value)
  (:export :decompress-string))
(in-package :http/encoding)

(defun read-encoding (string)
  (print string)
  (cond ((string= "gzip" string) 'chipz:gzip)
        ((string= "deflate" string) 'chipz:deflate)))

(defun decompress-string (string headers)
  (let ((encodings (str:words (assoc-value headers :content-encoding))))
    (flexi-streams:octets-to-string
     (reduce (lambda (acc encoding) (chipz:decompress nil encoding acc))
             (mapcar #'read-encoding encodings)
             :initial-value (flexi-streams:string-to-octets string)))))
