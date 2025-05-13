(defpackage :oscl
  (:use :cl :usocket :yason)
  (:documentation "Minimal ffmpeg wrapper CLI tool written in Common Lisp.")
  (:export

    ;; util.lisp
    :split-by-whitespace
    :split-by-dot
    :infer-osc-arg-type
    :parse-osc-arg-list
    :escape-pressed-p

    ;; validate.lisp
    :valid-port-number-p
    :valid-ipv4-address-p
    :valid-address-string-p
    :valid-interval-ms-p
    :valid-osc-args-p
    :valid-send-json-p

    ;; main.lisp
    :main

    ;; send.lisp
    :send-main

    ;; builder.lisp
    :build-osc-str
    :build-osc-int
    :build-osc-float
    :osc-type-tag
    :build-osc-arg
    :build-osc-message

    ;; recv.lisp
    :recv-main
    :*recv-filter*
    :*recv-filter-mode*
    :*remote-host*
    :*remote-port*

    ;; parser.lisp
    :parse-osc-str
    :parse-osc-int
    :parse-osc-float
    :parse-message
    :parse-buffer

    ;; const.lisp
    :*default-recv-port*
    :*recv-loop-interval*
    :*recv-socket-timeout*
    :*recv-running*
    :*send-running*
    
    ;; argsparse.lisp
    :parse-send-args
    :parse-recv-args
    ))