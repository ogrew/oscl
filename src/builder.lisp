(in-package :oscl)

(defun build-osc-str (str)
  "Build an OSC-compliant string with null-termination and 4-byte padding.
   Returns a (vector (unsigned-byte 8)) representing the binary string."
  (let* ((bytes (map 'vector #'char-code str))
         (null-term (vector 0))  ; Null-termination
         (base (concatenate 'vector bytes null-term))
         (padding-num (mod (- 4 (mod (length base) 4)) 4))
         (padding-bytes (make-array padding-num
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
    (concatenate 'vector base padding-bytes)))

(defun build-osc-int (value)
  "Convert a Lisp integer to a 4-byte big-endian OSC int32 vector."
  (let ((b0 (ldb (byte 8 24) value))
        (b1 (ldb (byte 8 16) value))
        (b2 (ldb (byte 8 8)  value))
        (b3 (ldb (byte 8 0)  value)))
    (vector b0 b1 b2 b3)))

(defun build-osc-float (value)
  "Convert a Lisp float into 4-byte big-endian OSC float32 format."
  (let* ((int-bits (ldb (byte 32 0)
                        (sb-kernel:single-float-bits (coerce value 'single-float))))
         (b0 (ldb (byte 8 24) int-bits))
         (b1 (ldb (byte 8 16) int-bits))
         (b2 (ldb (byte 8 8)  int-bits))
         (b3 (ldb (byte 8 0)  int-bits)))
    (vector b0 b1 b2 b3)))

(defun osc-type-tag (value)
  "Return a single-character OSC type tag based on Lisp value."
  (cond
    ((integerp value) #\i)
    ((floatp value)   #\f)
    ((stringp value)  #\s)
    (t (error "Unsupported OSC argument type: ~a" value))))

(defun build-osc-arg (value)
  "Convert a single argument to its OSC-encoded byte vector."
  (cond
    ((integerp value) (build-osc-int value))
    ((floatp value)   (build-osc-float value))
    ((stringp value)  (build-osc-str value))
    (t (error "Unsupported OSC argument type: ~a" value))))

(defun build-osc-message (address args)
  "Construct a binary OSC message from address and argument list."
  (let* ((address-bin (build-osc-str address))
         (types (mapcar #'osc-type-tag args))
         (type-tag-str (concatenate 'string "," (coerce types 'string)))
         (type-bin (build-osc-str type-tag-str))
         (arg-bin (mapcan #'build-osc-arg args)))
    (concatenate '(vector (unsigned-byte 8)) address-bin type-bin arg-bin)))
