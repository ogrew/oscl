(in-package :oscl)

(defun main (&optional args)
  "Entry point for oscl. Parses command-line arguments and dispatches to the appropriate mode."
  (unless args (setf args (uiop:command-line-arguments)))
  
  (cond
    ((null args)
     (format t "No command provided. Available commands: send, recv, network.~%"))

    ((string= (first args) "send")
     (send-main (rest args)))

    ((string= (first args) "recv")
     (recv-main (rest args)))

    ; ((string= (first args) "network")
    ;  (network-main (rest args)))

    (t
     (format t "Unknown command: ~a~%Available commands: send, recv, network.~%" (first args)))))