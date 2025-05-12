(asdf:defsystem "oscl"
  :description "minimal OSC toolkit written in Common Lisp"
  :version "0.1.0"
  :author "ogrew"
  :license "MIT"
  :depends-on (:usocket)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "signal")
               (:file "help")
               (:file "log")
               (:file "const")
               (:file "util")
               (:file "main")
               (:file "validate")
               (:file "builder")
               (:file "send")
               (:file "parser")
               (:file "recv")
               ))

(defsystem "oscl/test"
  :description "Test suite for oscl"
  :depends-on (:oscl :rove)
  :components ((:module "t"
                :components
                ((:file "test-validate")
                 (:file "test-util")
                 (:file "test-parser")
                 (:file "test-builder")
                 ))))