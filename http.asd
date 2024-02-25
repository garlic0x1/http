(asdf:defsystem "http"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:alexandria :str)
  :components ((:module "src"
                :components ((:file "util")
                             (:file "header")
                             (:file "request")
                             (:file "response")
                             (:file "package")))))
