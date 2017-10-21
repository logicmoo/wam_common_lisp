

(defun show-ascii-art ()
        
(write-line "  __________")
(write-line " / ___  ___ \")
(write-line "/ / @ \/ @ \ \")
(write-line "\ \___/\___/ /\")
(write-line " \____\/____/||")
(write-line " /     /\\\\\//")
(write-line "|     |\\\\\\")
(write-line " \      \\\\\\")
(write-line "   \______/\\\\")
(write-line "    _||_||_")
(write-line ""))

(show-ascii-art)
(load "wam-cl-init1")
(load "wam-cl-init2")
(load "wam-cl-init3")
(write-line " WAM CommonLisp ")
(read-eval-print-loop)

