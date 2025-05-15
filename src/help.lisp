(in-package :oscl)

(defun print-help ()
  (format t "~64~~%")
  (format t "~%oscl - minimal OSC send/receive tool written in Common Lisp~%~%")
  (format t "~64~~%~%")

  ;; Usage セクション
  (format t "Usage:~%")
  (format t "  oscl send --host <ip> --port <num> --address <addr> [--args <args> --interval <ms>]~%")
  (format t "  oscl recv --port <num> [--filter <str>] [--raw]~%")
    (format t "  oscl bridge --in-host <ip> --in-port <num> --out-host <ip> --out-port <num> [--filter <str>]~%~%")

  ;; send モードのオプション
  (format t "Send mode options:~%")
  (format t "  --host <ip>           Destination IP address                         (required)~%")
  (format t "  --port <num>          Destination port number                        (required)~%")
  (format t "  --address <addr>      OSC address string (e.g., /test)               (required)~%")
  (format t "  --args <string>       OSC arguments as quoted string, e.g., \"1 2.0 hello\"~%")
  (format t "  --interval <ms>       Repeat message every <ms> milliseconds (Ctrl+C to stop)~%~%")

  ;; recv モードのオプション
  (format t "Receive mode options:~%")
  (format t "  --port <num>          Local port to listen on (Ctrl+C to stop)       (required)~%")
  (format t "  --filter <str>        Only show messages whose address includes <str>.~%")
  (format t "                        Use a leading '-' to exclude matches.           ~%")
  (format t "  --raw                 Show first 64 bytes of raw data in hex.         ~%")
  (format t "                        Must appear at the end of the argument list.     ~%~%")

  ;; bridge モードのオプション
  (format t "Bridge mode options:~%")
  (format t "  --in-host <ip>        Source IP to listen for incoming OSC           (required)~%")
  (format t "  --in-port <num>       Source port to listen on                       (required)~%")
  (format t "  --out-host <ip>       Destination IP to forward messages to          (required)~%")
  (format t "  --out-port <num>      Destination port for forwarding                (required)~%")
  (format t "  --filter <str>        Forward only if address includes <str>.          ~%")
  (format t "                        Use leading '-' to exclude matches.              ~%~%")

  ;; その他
  (format t "General:~%")
  (format t "  --help                Show this help message~%~%"))
