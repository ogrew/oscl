(in-package :oscl)

(defparameter *default-recv-port* 9000
  "Default port number for OSC receive mode.")

(defparameter *recv-loop-interval* 0.2
  "Interval in seconds between each OSC receive loop iteration.")

(defparameter *recv-socket-timeout* 0.1
  "Timeout in seconds for non-blocking socket input wait.")

(defparameter *recv-running* t
  "Control flag for the recv loop. Set to NIL to exit gracefully.")

(defparameter *send-running* t
  "Control flag for the send loop. Set to NIL to exit gracefully.")

(defparameter *default-send-json-interval* 600
  "Default interval (in milliseconds) between OSC messages when sending from JSON.")

(defparameter *raw-display-limit* 64
  "Maximum number of bytes to display in raw output (hexadecimal).")
