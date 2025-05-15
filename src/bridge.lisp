(in-package :oscl)

(defun bridge-main (args)
  "Bridge mode: Receive OSC messages and forward them to another host:port."
  (multiple-value-bind (in-host in-port out-host out-port filter filter-mode)
      (parse-bridge-args args)

    ;; フィルタ設定をグローバルにはしない（ローカル変数として持つ）
    (let* ((socket
            (handler-case
                (usocket:socket-connect
                  nil nil
                  :local-host in-host
                  :local-port in-port
                  :element-type '(unsigned-byte 8)
                  :protocol :datagram)
              (sb-bsd-sockets:address-in-use-error ()
                (format t "~a Port ~a is already in use. Please try a different port.~%"
                        (log-tag "error") in-port)
                (return-from bridge-main))))
           (buffer (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))
           (buffer-size (length buffer)))

      (format t "~a Listening on ~a:~a for incoming OSC messages~%"
              (log-tag "receive") in-host in-port)
      (format t "~a Forwarding to ~a:~a~%" (log-tag "config") out-host out-port)
      (when filter
        (format t "~a Filter mode: ~a (~a)~%" (log-tag "config")
                (string-upcase (string filter-mode)) filter))

      #+sbcl (format t "~a Press Ctrl+C to stop bridging.~%" (log-tag "info"))

      (let ((non-blocking-time *recv-socket-timeout*))
        (loop while *recv-running* do
          (handler-case

            (let ((ready (usocket:wait-for-input socket :timeout non-blocking-time :ready-only t)))
              (when ready
                (multiple-value-bind (bytes host port)
                    (usocket:socket-receive socket buffer buffer-size)
                  (declare (ignore host port))
                    (when (and bytes (arrayp bytes))
                      (let ((cnt (length bytes)))
                        (multiple-value-bind (address args)
                          (parse-buffer-for-bridge bytes filter filter-mode cnt)
                          (when address
                            (format t "~a Forwarding ~a to ~a:~a~%"
                                    (log-tag "info") address out-host out-port)
                            (let ((msg (build-osc-message address args)))
                              (send-raw-buffer out-host out-port msg)))))))))

            (error ()
              (format t "~a Unexpected error in socket handling~%" (log-tag "error"))
              (sleep 2))))

        ;; 明示的な終了メッセージ
        (format t "~a bridge finished.~%" (log-tag "info"))))))
