(asdf:defsystem "oscl"
  :description "minimal OSC toolkit written in Common Lisp"
  :version "0.2.0"
  :author "ogrew"
  :license "MIT"
  :depends-on (:usocket :yason)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "const")
               (:file "help")
               (:file "log")
               (:file "signal")
               (:file "util")
               (:file "validate")
               (:file "argsparse")
               (:file "parser")
               (:file "recv")
               (:file "builder")
               (:file "send")
               (:file "bridge")
               (:file "main")
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
                 (:file "test-argsparse")
                 ))))