(in-package :oscl)

(defun send-json-once (json-data socket delay)
  "Send each message in JSON once with delay."
  (loop for json in json-data
        while *send-running*
        for address = (gethash "address" json)
        for args = (gethash "args" json)
        for message = (build-osc-message address args)
        do (handler-case
               (progn
                 (format t "~a [SEND] ~a ~a~%" (log-tag "success") address args)
                 (usocket:socket-send socket message (length message))
                 (sleep delay))
             (usocket:socket-error (e)
               (format t "~a Failed to send message: ~a~%" (log-tag "error") e)))))

(defun send-json-loop (json-data socket delay)
  "Send each message in JSON repeatedly until interrupted."
  #+sbcl (format t "~a Press Ctrl+C to stop sending.~%" (log-tag "info"))
  (loop while *send-running* do (send-json-once json-data socket delay)))

(defun send-from-json (host port file interval-ms &optional loop-flag)
  "Read OSC messages from JSON and dispatch to once or loop sender."
  (format t "~a Reading OSC messages from file: ~a~%" (log-tag "info") file)

  (let ((json-data (yason:parse (uiop:read-file-string file))))
    (unless (and (listp json-data)
                 (every (lambda (x)
                          (and (hash-table-p x)
                               (gethash "address" x)
                               (gethash "args" x)))
                        json-data))
      (format t "~a JSON must be an array of objects with 'address' and 'args'.~%" (log-tag "error"))
      (return-from send-from-json))

    (let* ((delay (/ (or interval-ms *default-send-json-interval*) 1000.0))
           (socket (usocket:socket-connect host port
                                           :protocol :datagram
                                           :element-type '(unsigned-byte 8))))
      (format t "~a Sending to ~a:~a~%" (log-tag "send") host port)
      (if loop-flag
          (send-json-loop json-data socket delay)
          (send-json-once json-data socket delay))
      (format t "~a send finished.~%" (log-tag "info")))))

(defun send-once (host port address args)
  "Send a single OSC message once."
  (let ((message (build-osc-message address args))
        (socket (usocket:socket-connect host port
                                        :protocol :datagram
                                        :element-type '(unsigned-byte 8))))
    (format t "~a Sending to ~a:~a~%" (log-tag "send") host port)
    (usocket:socket-send socket message (length message))
    (format t "~a send finished.~%" (log-tag "info"))))

(defun send-loop (host port address args interval-ms)
  "Send OSC message in a loop with given interval (ms)."
  (let ((message (build-osc-message address args))
        (socket (usocket:socket-connect host port
                                        :protocol :datagram
                                        :element-type '(unsigned-byte 8))))
    (format t "~a Sending to ~a:~a~%" (log-tag "send") host port)
    #+sbcl (format t "~a Press Ctrl+C to stop sending.~%" (log-tag "info"))
    (loop while *send-running*
          do (handler-case
                (progn
                  (usocket:socket-send socket message (length message))
                  (sleep (/ interval-ms 1000.0)))
                (usocket:socket-error (e)
                  (format t "~a Failed to send message: ~a~%" (log-tag "error") e))))
    (format t "~a send finished.~%" (log-tag "info"))))

(defun send-main (args)
  "Entry point for send mode. Sends OSC message based on CLI args."
  (let ((host nil)
        (port nil)
        (address nil)
        (osc-args '())
        (interval-ms nil)
        (file nil)
        (loop-flag nil))

    (let ((i 0))
      (loop while (< i (length args)) do
        (let ((opt (nth i args)))
          (cond

            ((args-flag-p opt)
              (when (string= opt "--loop")
                (setf loop-flag t))
              (incf i))

            ((args-kv-p opt)
              (when (< (1+ i) (length args))
                (let ((val (nth (+ i 1) args)))
                  (cond
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
                    ((string= opt "--from")
                      (if (valid-send-json-p val)
                        (setf file val)
                        (error "[ERROR] Invalid json file: ~A" val)))))
                (setf i (+ i 2))))

            (t
              (format t "~a Unknown option: ~a~%" (log-tag "warn") opt)
              (incf i))
    ))))

    (if file
    ;; read from json file
      (progn
        ;; check args
        (when (or address osc-args)
          (format t "~a --from cannot be combined with --address or --args.~%"
            (log-tag "error"))
          (return-from send-main))

        (send-from-json host port file interval-ms loop-flag))
    ;; normal mode
      (progn
        ;; check args
        (when loop-flag
          (format t "~a --loop is only valid with --from.~%"
            (log-tag "error"))
          (return-from send-main))
        (unless (and host port address)
          (format t "~a --host, --port, and --address are required when not using --from.~%"
            (log-tag "error"))
          (return-from send-main))

        (if interval-ms
          (send-loop host port address osc-args interval-ms)
          (send-once host port address osc-args))))))
