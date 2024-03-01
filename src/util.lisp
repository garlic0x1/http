(defpackage :http/util
  (:use :cl)
  (:export :crlf
           :make-keyword
           :inflate-alist
           :deflate-alist
           :inflate-uri
           :deflate-uri))
(in-package :http/util)

(defun crlf (stream)
  (write-char #\return stream)
  (write-char #\linefeed stream)
  (values))

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))

(defun inflate-alist (jsonb)
  (let ((yason:*parse-object-as* :alist)
        (yason:*parse-object-key-fn* #'make-keyword))
    (yason:parse jsonb)))

(defun deflate-alist (alist)
  (let ((yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase))
    (with-output-to-string (capture)
      (yason:encode-alist alist capture))))

(defun inflate-uri (string)
  (puri:parse-uri string))

(defun deflate-uri (uri)
  (with-output-to-string (capture)
    (puri:render-uri uri capture)))
