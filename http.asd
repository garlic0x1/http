(asdf:defsystem "http"
  :author "garlic0x1"
  :license "MIT"
  :description "HTTP utilities preserving raw TCP messages."
  :depends-on (:alexandria :cl-annot :str :usocket :mito :yason :puri)
  :components ((:module "src"
                :components ((:file "util")
                             (:file "types")
                             (:file "read")
                             (:file "write")
                             (:file "server")
                             (:file "client")
                             (:file "package")))))
