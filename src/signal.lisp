(in-package :oscl)

#+sbcl
(progn
  (require :sb-posix)
  (defun setup-signal-handlers ()
    (sb-sys:enable-interrupt sb-unix:sigint
      (lambda (&rest _)
        (declare (ignore _))
        (setf *recv-running* nil)
        (setf *send-running* nil)
        (format t " -> Ctrl+C detected. Exiting cleanly...~%")
      ))))
