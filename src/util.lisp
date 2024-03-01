(defpackage :http/util
  (:use :cl)
  (:export :with-nil-to-string
           :crlf
           :make-keyword
           :inflate-alist
           :deflate-alist
           :inflate-uri
           :deflate-uri))
(in-package :http/util)

(defmacro with-nil-to-string ((stream) &body body)
  "Capture nil streams as strings, like #'format."
  `(if ,stream
       (progn ,@body)
       (with-output-to-string (,stream) ,@body)))

(defun crlf (stream &optional (count 1))
  "Write CRLF(s) to stream."
  (dotimes (i count)
    (write-char #\return stream)
    (write-char #\linefeed stream)))

(defun make-keyword (string)
  "Interns upcase keyword like the reader does."
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
