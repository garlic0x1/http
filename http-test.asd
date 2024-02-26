(asdf:defsystem "http-test"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:fiveam :usocket :http)
  :components ((:module "t"
                :components ((:file "http-test")))))
