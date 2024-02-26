(defpackage :http/url
  (:nicknames :http/u)
  (:use :cl)
  (:import-from :alexandria-2 :line-up-last)
  (:export :parse-url
           :extract-url-host-and-port
           :url-scheme
           :url-host
           :url-port
           :url-path
           :url-query
           :url-fragment))
(in-package :http/url)

(defclass url ()
  ((scheme
    :initarg :scheme
    :initform "http"
    :accessor url-scheme)
   (host
    :initarg :host
    :initform (error "Must specify host.")
    :accessor url-host)
   (port
    :initarg :port
    :initform 80
    :accessor url-port)
   (path
    :initarg :path
    :initform "/"
    :accessor url-path)
   (query
    :initarg :query
    :initform nil
    :accessor url-query)
   (fragment
    :initarg :fragment
    :initform nil
    :accessor url-fragment)))

(defun extract-url-scheme (string)
  (first (str:split "://" string)))

(defun extract-url-host-and-port (string)
  (let ((host-and-port (line-up-last
                        (str:split "://" string)
                        (second)
                        (str:split "/")
                        (first)
                        (str:split ":"))))
    (values (first host-and-port)
            (parse-integer (or (second host-and-port) "80")))))

(defun extract-url-path (string)
  (line-up-last
   (str:split "://" string)
   (second)
   (str:split "/")
   (second)
   (str:split "?")
   (first)
   (str:split "#")
   (first)))

(defun extract-url-query (string)
  (line-up-last
   (str:split "?" string)
   (second)
   (str:split "#")
   (first)))

(defun extract-url-fragment (string)
  (second (str:split "#" string)))

(defun parse-url (string)
  (multiple-value-bind (host port) (extract-url-host-and-port string)
    (make-instance
     'url
     :scheme (extract-url-scheme string)
     :host host
     :port port
     :path (or (extract-url-path string) "/")
     :query (extract-url-query string)
     :fragment (extract-url-fragment string))))
