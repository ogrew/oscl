(in-package :oscl)

(defun parse-send-args (args)
  (loop with host = nil
        with port = nil
        with address = nil
        with interval-ms = nil
        with osc-args = nil
        with file = nil
        with loop-flag = nil
        with i = 0

        while (< i (length args)) do
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
                        (if (string= val "localhost")
                            (progn
                              (setf host "127.0.0.1")
                              (format t "~a Host 'localhost' converted to 127.0.0.1~%" (log-tag "info")))
                            (setf host val)))
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
                  (incf i 2)))

              (t
                (format t "~a Unknown option: ~a~%" (log-tag "warn") opt)
                (incf i))))

        finally (return (values host port address interval-ms osc-args file loop-flag))))

(defun parse-recv-args (args)
  (loop with port = *default-recv-port*
        with recv-filter = nil
        with recv-filter-mode = nil
        with recv-raw = nil
        with i = 0

        while (< i (length args)) do
          (let ((opt (nth i args)))
            (cond
              ((string= opt "--port")
                (let ((val (nth (1+ i) args)))
                  (if (valid-port-number-p val)
                      (setf port (parse-integer val))
                      (format t "~a Invalid port number: ~a~%" (log-tag "error") val)))
                (incf i 2))

              ((string= opt "--filter")
                (let ((val (nth (1+ i) args)))
                  (when val
                    (if (and (> (length val) 0) (char= (char val 0) #\-))
                        (progn
                          (setf recv-filter (subseq val 1))
                          (setf recv-filter-mode :exclude))
                        (progn
                          (setf recv-filter val)
                          (setf recv-filter-mode :include)))))
                (incf i 2))

              ((string= opt "--raw")
                (setf recv-raw t)
                (incf i))

              (t
                (format t "~a Unknown option in recv: ~a~%" (log-tag "warn") opt)
                (incf i))))

        finally (return (values port recv-filter recv-filter-mode recv-raw))))

(defun parse-bridge-args (args)
  "Parse command-line arguments for bridge mode.
Returns: in-host, in-port, out-host, out-port, filter (string or nil), filter-mode (:include or :exclude)"

  (loop with in-host = "0.0.0.0"
        with in-port = *default-recv-port*
        with out-host = nil
        with out-port = nil
        with filter = nil
        with filter-mode = nil
        with i = 0

        while (< i (length args)) do
          (let ((opt (nth i args)))
            (cond
              ((string= opt "--in-host")
                (let ((val (nth (1+ i) args)))
                  (when val
                    (setf in-host (if (string= val "localhost") "127.0.0.1" val))))
                (incf i 2))

              ((string= opt "--in-port")
                (let ((val (nth (1+ i) args)))
                  (when (valid-port-number-p val)
                    (setf in-port (parse-integer val))))
                (incf i 2))

              ((string= opt "--out-host")
                (let ((val (nth (1+ i) args)))
                  (when val
                    (setf out-host (if (string= val "localhost") "127.0.0.1" val))))
                (incf i 2))

              ((string= opt "--out-port")
                (let ((val (nth (1+ i) args)))
                  (when (valid-port-number-p val)
                    (setf out-port (parse-integer val))))
                (incf i 2))

              ((string= opt "--filter")
                (let ((val (nth (1+ i) args)))
                  (when val
                    (if (and (> (length val) 0) (char= (char val 0) #\-))
                        (progn
                          (setf filter (subseq val 1))
                          (setf filter-mode :exclude))
                        (progn
                          (setf filter val)
                          (setf filter-mode :include)))))
                (incf i 2))

              (t
                (format t "~a Unknown option in bridge: ~a~%" (log-tag "warn") opt)
                (incf i))))

        finally (return (values in-host in-port out-host out-port filter filter-mode))))
