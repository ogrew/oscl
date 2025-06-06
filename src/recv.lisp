(in-package :oscl)

(defparameter *recv-filter* nil)
(defparameter *recv-filter-mode* :include)
(defparameter *remote-host* nil)
(defparameter *remote-port* nil)
(defparameter *recv-raw* nil)

(defun recv-main (args)
  "Entry point for recv mode. Starts listening for OSC messages."
  (multiple-value-bind (port filter filter-mode raw-flag)
      (parse-recv-args args)

    (setf *recv-filter* filter
          *recv-filter-mode* filter-mode
          *recv-raw* raw-flag)

    (let* ((socket
            (handler-case
              (usocket:socket-connect
                nil nil
                :local-host "0.0.0.0"
                :local-port port
                :element-type '(unsigned-byte 8)
                :protocol :datagram)
              (sb-bsd-sockets:address-in-use-error ()
                (format t "~a Port ~a is already in use. Please try a different port.~%" (log-tag "error") port)
                (return-from recv-main))))
           (buffer (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))
           (buffer-size (length buffer)))
      (format t "~a Listening on port ~a (all interfaces: 0.0.0.0)~%" (log-tag "receive") port)
      (format t "~a Loop interval: ~a s / Socket timeout: ~a s~%"
          (log-tag "config") *recv-loop-interval* *recv-socket-timeout*)
      (when filter
        (format t "~a Filter mode: ~a (~a)~%" (log-tag "config")
                (string-upcase (string filter-mode)) filter))

      #+sbcl (format t "~a Press Ctrl+C to stop receiving.~%" (log-tag "info"))

    ;; Add non-blocking logic
    (let ((non-blocking-time *recv-socket-timeout*))

      (loop while *recv-running*
        do
        ;; Brief pause between socket reads to avoid CPU spinning
        (sleep *recv-loop-interval*)
        ;; Non-blocking socket receive with proper error handling
        (handler-case
          (let ((ready (usocket:wait-for-input socket :timeout non-blocking-time :ready-only t)))
            (when ready
              ;; Clear buffer before receiving new data
              (dotimes (i buffer-size)
                (setf (aref buffer i) 0))

              ;; socket-receive modifies the buffer in-place and returns bytes-received as first value
              (let ((bytes-received 0))

                ;; Use multiple-value-bind but handle carefully
                (multiple-value-bind (bytes host port)
                    (usocket:socket-receive socket buffer buffer-size)
                  
                  (cond
                    ;; If bytes-received is a number, use it directly
                    ((numberp bytes)
                     (setf bytes-received bytes
                           *remote-host* host
                           *remote-port* port))
                    
                    ;; If buffer was returned as first value (bug workaround)
                    ((typep bytes '(array (unsigned-byte 8)))
                     ;; Count non-zero bytes for approximate message size
                     (loop for i from 0 below buffer-size
                           while (> (aref bytes i) 0)
                           do (incf bytes-received))
                     (setf *remote-host* host
                           *remote-port* port))
                    
                    ;; Any other case
                    (t (setf bytes-received 0)))
                  
                  (cond
                    ;; Valid data received
                    ((> bytes-received 0)
                     (parse-buffer buffer bytes-received))
                    
                    ;; Zero bytes received 
                    ((= bytes-received 0)
                     (format t "~a Zero bytes received~%" (log-tag "warn")))
                    
                    ;; Shouldn't happen with our improved handling
                    (t 
                     (format t "~a Invalid receive result~%" (log-tag "error"))))))))

          (error (e) 
            (format t "~a Unexpected error in socket handling : ~a~%" (log-tag "error") e)
            ;; Longer pause for unexpected errors
            (sleep 2))))

      (format t "~a recv finished.~%" (log-tag "info"))))))
