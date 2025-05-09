(defpackage :oscl
  (:use :cl :usocket)
  (:documentation "Minimal ffmpeg wrapper CLI tool written in Common Lisp.")
  (:export

    ;; main.lisp
    :main
    
    ;; recv.lisp
    :recv-main

    ;; parse.lisp
    :parse-buffer
    :read-osc-string
    :read-osc-int
    :read-osc-float
    :parse-message
    :print-buffer
    
    ;; const.lisp
    :*default-recv-port*))