(in-package :oscl)

(defun valid-port-number-p (val)
  "Check if the given value is a valid port number (1-65535)."
  (let ((port (parse-integer val :junk-allowed t)))
    (and port (<= 1 port) (<= port 65535))))

(defun valid-ipv4-address-p (str)
  "Check if STR is a valid IPv4 address (e.g., 192.168.0.1)."
  (let ((parts (split-by-dot str)))
    (and (= (length parts) 4)
         (every (lambda (part)
                  (and (every #'digit-char-p part)
                       (let ((n (parse-integer part :junk-allowed t)))
                         (and n (<= 0 n 255)))))
                parts))))

(defun valid-address-string-p (str)
  "Check if STR is a valid OSC address (starts with / and has at least one more character)."
  (and (stringp str)
       (plusp (length str))
       (char= (char str 0) #\/)))

(defun valid-interval-ms-p (val)
  "Check if VAL is a valid non-negative integer string."
  (and (every #'digit-char-p val)
       (let ((n (parse-integer val :junk-allowed t)))
         (and n (>= n 0)))))

(defun valid-osc-args-p (str)
  "Check that args string is non-empty and has even number of quotes."
  (and (stringp str)
       (> (length str) 0)
       (evenp (count #\" str))))

(defun valid-send-json-p (path)
  "Check if PATH is a valid JSON file containing OSC messages with valid types."
  (and (probe-file path)
       (ignore-errors
         (let ((json (with-open-file (in path)
                        (yason:parse in))))
           (and (listp json)
                (every (lambda (entry)
                         (and (hash-table-p entry)
                              (let ((address (gethash "address" entry))
                                    (args (gethash "args" entry)))
                                (and address args
                                     (valid-address-string-p address)
                                     (listp args)))))
                       json))))))

(defun args-flag-p (opt)
  (member opt '("--loop") :test #'string=))

(defun args-kv-p (opt)
  (member opt '("--host" "--port" "--address" "--args" "--interval" "--from") :test #'string=))
