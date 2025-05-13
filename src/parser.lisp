(in-package :oscl)

(defvar oscl::*recv-filter*)
(defvar oscl::*remote-host*)
(defvar oscl::*remote-port*)

(defun parse-osc-str (buffer start)
  "Parse null-terminated string from buffer starting at START. Returns string and next index."
  (let ((result "")
        (index start))
    (loop
      (let ((byte (aref buffer index)))
        (when (zerop byte) (return))
        (setf result (concatenate 'string result (string (code-char byte))))
        (incf index)))
    ;; Advance to 4-byte boundary
    (incf index (- 4 (mod index 4)))
    (values result index)))

(defun parse-osc-int (buffer index)
  "Parse 4-byte big-endian integer."
  (let ((value (+ (* (aref buffer index) (expt 256 3))
                  (* (aref buffer (+ index 1)) (expt 256 2))
                  (* (aref buffer (+ index 2)) (expt 256 1))
                  (aref buffer (+ index 3)))))
    (values value (+ index 4))))

(defun parse-osc-float (buffer index)
  "Parse 4-byte big-endian float."
  (let ((int-value (logior (ash (aref buffer index) 24)
                           (ash (aref buffer (+ index 1)) 16)
                           (ash (aref buffer (+ index 2)) 8)
                           (aref buffer (+ index 3)))))
    (values (sb-kernel:make-single-float (ldb (byte 32 0) int-value))
            (+ index 4))))

(defun parse-message (buffer)
  "Parse standard OSC message."
  (multiple-value-bind (address index) (parse-osc-str buffer 0)

    (when (and *recv-filter*
              (not (search *recv-filter* address)))
      (return-from parse-message nil))

    (format t "~a From ~a:~a - " (log-tag "success")
                          (or *remote-host* "Unknown") 
                          (or *remote-port* "Unknown"))

    (format t "#ADDRESS ~a " address)

    (multiple-value-bind (typetags index) (parse-osc-str buffer index)

      (let ((args '()))
        (loop for i from 1 below (length typetags)
              do (let ((tag (char typetags i)))
                   (cond
                     ((char= tag #\i)
                      (multiple-value-bind (val next-index) (parse-osc-int buffer index)
                        (setf args (append args (list val)))
                        (setf index next-index)))

                     ((char= tag #\f)
                      (multiple-value-bind (val next-index) (parse-osc-float buffer index)
                        (setf args (append args (list val)))
                        (setf index next-index)))

                     ((char= tag #\s)
                      (multiple-value-bind (val next-index) (parse-osc-str buffer index)
                        (setf args (append args (list val)))
                        (setf index next-index)))

                     (t
                      (format t "~a Unsupported type tag: ~a~%" (log-tag "warn") tag)))))
        (format t "#ARGS ~a~%" args)))))

(defun parse-buffer (buffer cnt)
  "Parse an OSC buffer. Determine if it is a bundle or message."
  (let ((is-bundle (and (>= cnt 7)
                        (= (aref buffer 0) 35)  ;; #
                        (= (aref buffer 1) 98)  ;; b
                        (= (aref buffer 2) 117) ;; u
                        (= (aref buffer 3) 110) ;; n
                        (= (aref buffer 4) 100) ;; d
                        (= (aref buffer 5) 108) ;; l
                        (= (aref buffer 6) 101)))) ;; e
    (if is-bundle
        (format t "~a #bundle detected (bundle support is TODO)~%" (log-tag "info"))
        (parse-message buffer))))
