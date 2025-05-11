(in-package :oscl)

#+sbcl
(progn
  (require :sb-posix)
  (sb-sys:enable-interrupt sb-unix:sigint
  (lambda (&rest _)
    (declare (ignore _))
    (setf *recv-running* nil))))

(defun recv-main (args)
  "Entry point for recv mode. Starts listening for OSC messages."
  (let ((port *default-recv-port*))
    (loop for pair on args by #'cddr
          for opt = (first pair)
          for val = (second pair)
          do (cond
               ((string= opt "--port")
                (setf port (parse-integer val)))
               (t
                (format t "[WARN] Unknown option in recv-main: ~a~%" opt))))

    (let* ((socket (usocket:socket-connect 
                     nil nil
                     :local-host "0.0.0.0"
                     :local-port port
                     :element-type '(unsigned-byte 8)
                     :protocol :datagram))
           (buffer (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))
           (buffer-size (length buffer)))
      (format t "[RECEIVE] Listening on port ~A (all interfaces: 0.0.0.0)~%" port)
      (format t "[CONFIG] Loop interval: ~A s / Socket timeout: ~A s~%"
          *recv-loop-interval* *recv-socket-timeout*)
      (format t "[INFO] Press Ctrl+C to stop receiving.~%")

    ;; Add non-blocking logic
    (let ((non-blocking-time *recv-socket-timeout*))

      (loop while *recv-running*
        do
        ;; Brief pause between socket reads to avoid CPU spinning
        (sleep *recv-loop-interval*)
        ;(format t "[INFO] Checking for data...~%")
        ;; Non-blocking socket receive with proper error handling
        (handler-case
          (let ((ready (usocket:wait-for-input socket :timeout non-blocking-time :ready-only t)))
            (when ready
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
                  
                  (cond
                    ;; Valid data received
                    ((> bytes-received 0)
                     (format t "[SUCCESS] From ~A:~A - " 
                             (or remote-host "unknown") 
                             (or remote-port "unknown"))
                     (parse-buffer buffer bytes-received))
                    
                    ;; Zero bytes received 
                    ((= bytes-received 0)
                     (format t "[WARN] Zero bytes received~%"))
                    
                    ;; Shouldn't happen with our improved handling
                    (t (format t "[ERROR] Invalid receive result~%")))))))

          (error () 
            (format t "[ERROR] Unexpected error in socket handling~%")
            ;; Longer pause for unexpected errors
            (sleep 2))))

      (format t "~%[INFO] recv terminated.~%")))))
