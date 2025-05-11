(in-package :oscl)

(defun main (&rest args)
  "Entry point for oscl. Parses command-line arguments and dispatches to the appropriate mode."

  ;; Signal handler setup (only for SBCL)
  #+sbcl (setup-signal-handlers)

  (cond
    ((null args)
     (format t "No command provided. Available commands: send, recv.~%"))
    ((string= (first args) "send")
     (send-main (rest args)))
    ((string= (first args) "recv")
     (recv-main (rest args)))
    (t
     (format t "Unknown command: ~a~%" (first args)))))
