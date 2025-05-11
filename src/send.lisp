(in-package :oscl)

#+sbcl
(progn
  (require :sb-posix)
  (sb-sys:enable-interrupt sb-unix:sigint
  (lambda (&rest _)
    (declare (ignore _))
    (setf *send-running* nil))))

(defun send-main (args)
  "Entry point for send mode. Sends OSC message based on CLI args."
  (let ((host nil)
        (port nil)
        (address nil)
        (osc-args '())
        (interval-ms nil))

      (loop for pair on args by #'cddr
            for opt = (first pair)
            for val = (second pair)
              do (cond
               ((string= opt "--host") (setf host val))
               ((string= opt "--port") (setf port (parse-integer val)))
               ((string= opt "--address") (setf address val))
               ((string= opt "--args") (setf osc-args (parse-osc-arg-list val)))
               ((string= opt "--interval") (setf interval-ms (parse-integer val)))
               (t (format t "[WARN] Unknown option: ~a~%" opt))))

    (unless (and host port address)
      (format t "[ERROR] --host, --port, --address are required.~%")
      (return-from send-main))

    (let ((message (build-osc-message address osc-args)))
      (let ((socket (usocket:socket-connect host port
                                            :protocol :datagram
                                            :element-type '(unsigned-byte 8))))
        (format t "[SEND] Sending to ~a:~a~%" host port)
        
        (if interval-ms
            (loop while *send-running*
              do
                (handler-case
                  (progn
                    (usocket:socket-send socket message (length message))
                    (sleep (/ interval-ms 1000.0)))
                  (usocket:socket-error (e)
                    (format t "[ERROR] Failed to send message: ~A~%" e))))
            
            (usocket:socket-send socket message (length message))))

    (format t "~%[INFO] send terminated.~%"))))
