(defpackage :oscl
  (:use :cl :usocket)
  (:documentation "Minimal ffmpeg wrapper CLI tool written in Common Lisp.")
  (:export

    ;; util.lisp
    :split-by-whitespace
    :infer-osc-arg-type
    :parse-osc-arg-list

    ;; main.lisp
    :main

    ;; recv.lisp
    :send-main

    ;; recv.lisp
    :recv-main

    ;; parser.lisp
    :parse-buffer
    :read-osc-string
    :read-osc-int
    :read-osc-float
    :parse-message
    :print-buffer
    
    ;; const.lisp
    :*default-recv-port*))