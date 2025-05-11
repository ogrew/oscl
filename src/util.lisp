(in-package :oscl)

(defun split-by-whitespace (string)
  "Split STRING into tokens by space and full-width space characters."
  (let ((tokens '())
        (start 0)
        (length (length string)))
    (labels ((is-whitespace (ch)
               (or (char= ch #\Space) (char= ch #\u3000))))
      (loop for i from 0 to length
            do (cond
                 ((or (= i length)
                      (is-whitespace (char string i)))
                  (when (< start i)
                    (push (subseq string start i) tokens))
                  (setf start (1+ i)))))
    (nreverse tokens))))

(defun infer-osc-arg-type (token)
  "Convert token string to integer, float, or string."
  (or (let ((int (ignore-errors (parse-integer token :junk-allowed t))))
        (when int int))
      (let ((flt (ignore-errors
                   (let ((val (read-from-string token)))
                     (when (floatp val) val)))))
        (when flt flt))
      (string-trim "\"" token)))

(defun parse-osc-arg-list (arg-string)
  "Parse OSC argument string into a list of typed values."
  (mapcar #'infer-osc-arg-type (split-by-whitespace arg-string)))

(defun escape-pressed-p ()
  "Check if the ESC key (ASCII 27) has been pressed.
   Returns T if pressed, NIL otherwise."
  (when (listen *standard-input*)
    (let ((char (read-char-no-hang *standard-input* nil nil)))
      (and char (= (char-code char) 27)))))
