(in-package :oscl)

(defun recv-main (args)
  "Entry point for recv mode. Starts listening for OSC messages."
  (let* ((port (if args (parse-integer (first args)) *default-recv-port*))
         ;; UDPソケットをローカルポートにバインドして作成
         (socket (usocket:socket-connect 
                    nil nil ; No remote host/port for UDP server mode
                    :local-host "0.0.0.0"     ; Bind to all interfaces
                    :local-port port          ; Listen on specified port
                    :element-type '(unsigned-byte 8)
                    :protocol :datagram))     ; Configure for UDP
         ;; 受信用バッファをループの外で作成
         (buffer (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))
         ;; Buffer size info
         (buffer-size (length buffer)))
    (format t "[INFO] Created receive buffer of ~A bytes~%" buffer-size)
    (format t "[RECEIVE] Listening on 0.0.0.0:~a...~%" port)
    (format t "[INFO] Socket info: ~A~%" socket)

    ;; Add non-blocking logic
    (let ((non-blocking-time 0.1))  ; 100ms timeout
      (format t "[INFO] Using ~A second timeout for non-blocking operation~%" non-blocking-time)

      ;; 受信ループ
      (loop
        ;; Brief pause between socket reads to avoid CPU spinning
        (sleep 0.2)
        (format t "~%[INFO] Checking for data...~%")
        
        ;; Non-blocking socket receive with proper error handling
        (handler-case
          (let ((ready (usocket:wait-for-input socket :timeout non-blocking-time :ready-only t)))
            (when ready
              ;; Socket is ready for reading
              (format t "[INFO] Data detected! Reading...~%")

              ;; Clear buffer before receiving new data
              (dotimes (i buffer-size)
                (setf (aref buffer i) 0))

              ;; socket-receive modifies the buffer in-place and returns bytes-received as first value
              (let ((bytes-received 0)
                    (remote-host nil)
                    (remote-port nil))

                ;; Use multiple-value-bind but handle carefully
                (multiple-value-bind (bytes host port)
                    (usocket:socket-receive socket buffer buffer-size)
                  
                  ;; Handle possible types of return values
                  (cond
                    ;; If bytes-received is a number, use it directly
                    ((numberp bytes)
                     (setf bytes-received bytes
                           remote-host host
                           remote-port port))
                    
                    ;; If buffer was returned as first value (bug workaround)
                    ((typep bytes '(array (unsigned-byte 8)))
                     ;; Count non-zero bytes for approximate message size
                     (loop for i from 0 below buffer-size
                           while (> (aref bytes i) 0)
                           do (incf bytes-received))
                     (setf remote-host host
                           remote-port port))
                    
                    ;; Any other case
                    (t (setf bytes-received 0)))
                  
                  (format t "[INFO] Received ~A bytes from ~A:~A~%" 
                          bytes-received
                          (or remote-host "nil") 
                          (or remote-port "nil"))
                  
                  ;; Handle the received data if we got something
                  (cond
                    ;; Valid data received
                    ((> bytes-received 0)
                     (format t "[SUCCESS] From ~A:~A - " 
                             (or remote-host "unknown") 
                             (or remote-port "unknown"))
                     ;(print-buffer buffer bytes-received))
                     (parse-buffer buffer bytes-received))
                    
                    ;; Zero bytes received 
                    ((= bytes-received 0)
                     (format t "[WARN] Zero bytes received~%"))
                    
                    ;; Shouldn't happen with our improved handling
                    (t (format t "[ERROR] Invalid receive result~%")))))))

          ;; Catch any other errors to prevent crashes
          (error () 
            (format t "[ERROR] Unexpected error in socket handling~%")
            ;; Longer pause for unexpected errors
            (sleep 2)))))))
