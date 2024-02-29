(asdf:defsystem "http"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:alexandria :cl-annot :str :usocket :mito :yason :puri)
  :components ((:module "src"
                :components ((:file "util")
                             (:file "types")
                             (:file "read")
                             (:file "package")))))
