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
  (multiple-value-bind (host port address interval-ms osc-args file loop-flag)
      (parse-send-args args)

    (if file

      ;; read from json file
      (progn
        ;; check args
        (when (or address osc-args)
          (format t "~a --from cannot be combined with --address or --args.~%" (log-tag "error"))
          (return-from send-main))
        (send-from-json host port file interval-ms loop-flag))

      ;; normal mode
      (progn
        ;; check args
        (when loop-flag
          (format t "~a --loop is only valid with --from.~%" (log-tag "error"))
          (return-from send-main))
        (unless (and host port address)
          (format t "~a --host, --port, and --address are required when not using --from.~%" (log-tag "error"))
          (return-from send-main))
        (if interval-ms
          (send-loop host port address osc-args interval-ms)
          (send-once host port address osc-args))))))


;; -------------------------------------------------------------- ;;
;; -------------------------------------------------------------- ;;


(defun send-raw-buffer (host port buffer)
  "Send a raw OSC buffer (unsigned-byte 8 array) to given host:port via UDP."
  (handler-case
      (let ((socket (usocket:socket-connect
                     host port
                     :element-type '(unsigned-byte 8)
                     :protocol :datagram)))
        (usocket:socket-send socket buffer (length buffer))
        (usocket:socket-close socket))
    (usocket:socket-error (e)
      (format t "~a Failed to send to ~a:~a: ~a~%" (log-tag "error") host port e))))
