
(in-package #:system)

(defpackage "SYSTEM" (:nicknames "SYS"))
(defpackage "COMMON-LISP" (:nicknames "CL" "LISP")(:uses "SYSTEM"))
(defpackage "COMMON-LISP-USER" (:nicknames "U" "USER" "CL-USER") (:uses "COMMON-LISP"))
(defvar *lisp-file-type* "lisp") 
(defvar *default-pathname-defaults* #P"")
 
(defun dd () 
 (let ((*lisp-file-type* "cl") 
        (*default-pathname-defaults* (merge-pathnames "daydreamer/"))) (load "dd")))


(defun show-ascii-art ()
        
(write-line "  __________    ")
(write-line " / ___  ___ \\   ")
(write-line "/ / @ \\/ @ \\ \\  ")
(write-line "\\ \\___/\\___/ /\\ ")
(write-line " \\____\\/____/|| ")
(write-line " /     /\\\\\\\\\\// ")
(write-line "|     |\\\\\\\\\\\\   ")
(write-line " \\      \\\\\\\\\\\\  ")
(write-line "   \\______/\\\\\\\\ ")
(write-line "    _||_||_     ")
(write-line "                "))

(show-ascii-art)

(compile-file "wam-cl-init-00")
(load "wam-cl-init-00")

'(compile-file "wam-cl-init-10")
'(compile-file "wam-cl-init-20")
'(compile-file "wam-cl-init-30")
'(load "wam-cl-init2")
'(load "wam-cl-init3")
'(write-line " WAM CommonLisp ")
'(read-eval-print-loop)

