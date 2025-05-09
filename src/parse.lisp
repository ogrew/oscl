(in-package :oscl)

(defun parse-osc-str (buffer start)
  "Parse null-terminated string from buffer starting at START. Returns string and next index."
  (let ((result "")
        (index start))
    (loop
      (let ((byte (aref buffer index)))
        (when (zerop byte) (return))
        (setf result (concatenate 'string result (string (code-char byte))))
        (incf index)))
    ;; 4バイト境界まで進める
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
    (format t "[ADDRESS] ~A~%" address)

    ;; 型タグを読む
    (multiple-value-bind (typetags index) (parse-osc-str buffer index)
      (format t "[TYPES] ~A~%" typetags)

      ;; 引数を読む
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
                      (format t "[WARN] Unsupported type tag: ~A~%" tag)))))
        (format t "[ARGS] ~A~%" args)))))

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
        (format t "[INFO] #bundle detected (bundle support is TODO)~%")
        (parse-message buffer))))

;; DEBUG
(defun print-buffer (buffer cnt)
  "Print the first few bytes of a buffer.
   BUFFER: The byte array buffer
   COUNT: Number of bytes to display (as integer)"
  ;; First ensure the count is a number and not a buffer
  (when (not (numberp cnt))
    (setf cnt 0))
  
  ;; Print some basic info about what we received
  (format t "Received message of ~A bytes~%" cnt)
  
  ;; Only display at most 20 bytes to keep the output manageable
  (let ((display-count (min cnt 20)))
    (format t "First ~A bytes: " display-count)
    (dotimes (i display-count)
      (format t "~2,'0X " (aref buffer i)))  ;; HEX format for better readability
    (format t "~%"))
  
  ;; Try to interpret as ASCII for debugging
  (when (> cnt 0)
    (format t "ASCII interpretation: ")
    (let ((display-count (min cnt 40)))
      (dotimes (i display-count)
        (let ((byte (aref buffer i)))
          (if (and (>= byte 32) (<= byte 126))  ;; Printable ASCII range
              (format t "~c" (code-char byte))
              (format t ".")))))
    (format t "~%")))