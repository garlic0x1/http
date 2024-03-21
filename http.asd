(asdf:defsystem "http"
  :author "garlic0x1"
  :license "MIT"
  :description "HTTP utilities preserving raw TCP messages."
  :depends-on (:alexandria
               :cl-annot
               :str
               :usocket
               :chunga
               :flexi-streams
               :chipz
               :mito
               :yason
               :puri)
  :components ((:module "src"
                :components ((:file "util")
                             (:file "types")
                             (:file "encoding")
                             (:file "read")
                             (:file "write")
                             (:file "server")
                             (:file "client")
                             (:file "package")))))
