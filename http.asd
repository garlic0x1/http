(asdf:defsystem "http"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:alexandria :str :usocket)
  :components ((:module "src"
                :components ((:file "util")
                             (:file "url")
                             (:file "header")
                             (:file "request")
                             (:file "response")
                             (:file "client")
                             (:file "package")))))
