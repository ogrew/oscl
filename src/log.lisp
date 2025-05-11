(in-package :oscl)

(defun colorize (text code)
  (format nil "~c[~am~a~c[0m" #\Esc code text #\Esc))

(defun log-tag (type)
  (cond
    ((string= type "info")     (colorize "[INFO]"     "34;1"))  ; 青
    ((string= type "dry-run")  (colorize "[DRY-RUN]"  "33;1"))  ; 黄
    ((string= type "warn")     (colorize "[WARN]"     "35;1"))  ; 紫
    ((string= type "error")    (colorize "[ERROR]"    "31;1"))  ; 赤
    ((string= type "receive")  (colorize "[RECEIVE]"  "32"))    ; 緑（通常）
    ((string= type "send")     (colorize "[SEND]"     "36"))    ; シアン
    ((string= type "config")   (colorize "[CONFIG]"   "36;1"))  ; 水色（明るめ）
    ((string= type "success")  (colorize "[SUCCESS]"  "32;1"))  ; 緑（強調）
    (t type)))  ; fallback