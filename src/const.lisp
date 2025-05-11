(in-package :oscl)

(defparameter *default-recv-port* 9000
  "Default port number for OSC receive mode.")

(defparameter *recv-loop-interval* 0.2
  "Interval in seconds between each OSC receive loop iteration.")

(defparameter *recv-socket-timeout* 0.1
  "Timeout in seconds for non-blocking socket input wait.")
