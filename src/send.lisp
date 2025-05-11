(in-package :oscl)

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
               ((string= opt "--host")
                  (if (valid-ipv4-address-p val)
                    (setf host val)
                    (error "[ERROR] Invalid IP address: ~A" val)))
                ((string= opt "--port")
                  (if (valid-port-number-p val)
                    (setf port (parse-integer val))
                    (error "[ERROR] Invalid Port Number: ~A" val)))
               ((string= opt "--address")
                  (if (valid-address-string-p val)
                    (setf address val)
                    (error "[ERROR] Invalid OSC address: ~A" val)))
               ((string= opt "--interval")
                  (if (valid-interval-ms-p val)
                    (setf interval-ms (parse-integer val))
                    (error "[ERROR] Invalid interval value: ~A" val)))
               ((string= opt "--args")
                  (if (valid-osc-args-p val)
                    (setf osc-args (parse-osc-arg-list val))
                    (error "[ERROR] Invalid arguments: ~A" val)))
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
          (progn
            #+sbcl (format t "[INFO] Press Ctrl+C to stop sending.~%")
            (loop while *send-running*
                do
                  (handler-case
                    (progn
                     (usocket:socket-send socket message (length message))
                      (sleep (/ interval-ms 1000.0)))
                     (usocket:socket-error (e)
                      (format t "[ERROR] Failed to send message: ~A~%" e)))))
          (usocket:socket-send socket message (length message)))

    (format t "~%[INFO] send finished.~%")))))
