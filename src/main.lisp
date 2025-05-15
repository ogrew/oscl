(in-package :oscl)

(defun main (&rest args)
  "Entry point for oscl. Parses command-line arguments and dispatches to the appropriate mode."

  ;; Signal handler setup (only for SBCL)
  #+sbcl (setup-signal-handlers)

  (cond
    ((null args)
     (format t "~a No command provided. Available commands: send, recv, bridge.~%" (log-tag "error")))
    ((string= (first args) "--help")
     (print-help))
    ((string= (first args) "send")
     (send-main (rest args)))
    ((string= (first args) "recv")
     (recv-main (rest args)))
    ((string= (first args) "bridge")
     (bridge-main (rest args)))
    (t
     (format t "~a Unknown command ~a. Available commands: send, recv, bridge.~%" (log-tag "error") (first args)))))
