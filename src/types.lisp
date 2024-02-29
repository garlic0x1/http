(uiop:define-package :http/types
  (:use :cl)
  (:use :cl :cl-annot.class))
(in-package :http/types)
(cl-annot:enable-annot-syntax)

@export-class
(mito:deftable message ()
  ((raw
    :col-type :text
    :initarg :raw
    :initform ""
    :accessor message-raw)
   (headers
    :col-type :jsonb
    :initarg :headers
    :initform (make-hash-table)
    :accessor message-headers
    :inflate #'http/util:inflate-alist
    :deflate #'http/util:deflate-alist)
   (body
    :col-type :text
    :initarg :body
    :initform nil
    :accessor message-body)))

@export-class
(mito:deftable request (message)
  ((method
    :col-type (:varchar 32)
    :initarg :method
    :initform :GET
    :accessor request-method)
   (uri
    :col-type (:varchar 2048)
    :initarg :uri
    :initform "/"
    :accessor request-uri
    :inflate #'http/util:inflate-uri
    :deflate #'http/util:deflate-uri)
   (protocol
    :col-type (:varchar 32)
    :initarg :protocol
    :initform "HTTP/1.1"
    :accessor request-protocol)))

@export-class
(mito:deftable response (message)
  ((protocol
    :col-type (:varchar 32)
    :initarg :protocol
    :initform "HTTP/1.1"
    :accessor response-protocol)
   (status-code
    :col-type :integer
    :initarg :status-code
    :initform 200
    :accessor response-status-code)
   (status
    :col-type (:varchar 256)
    :initarg :status
    :initform "OK"
    :accessor response-status)))
