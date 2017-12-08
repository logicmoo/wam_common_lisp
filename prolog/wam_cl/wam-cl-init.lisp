
(defpackage "SYSTEM" (:nicknames "SYS"))
(defpackage "COMMON-LISP" (:nicknames "CL" "LISP")(:uses "SYSTEM"))
(defpackage "COMMON-LISP-USER" (:nicknames "U" "USER" "CL-USER") (:uses "COMMON-LISP"))
(defvar *lisp-file-type* "lisp") 
(defvar *default-pathname-defaults* #P"")
 
(defun dd () 
 (let ((*lisp-file-type* "cl") 
        (*default-pathname-defaults* (merge-pathnames "daydreamer/"))) (load "dd")))


(in-package "COMMON-LISP-USER")
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
(load "wam-cl-init-1")
'(load "wam-cl-init2")
'(load "wam-cl-init3")
'(write-line " WAM CommonLisp ")
'(read-eval-print-loop)

 

#|

;; (when (eq (symbol-package sym) p) (format t "~a ~a ~a ~a~%" ......)) 

(defun packagesyminfo (p0)
 (let ((p (find-package p0)))
 (do-all-symbols (sym)    
  (when (eq (symbol-package sym) p)
   (format t "symbolinfo('~s','~s').~%"
    sn (package-name (symbol-package sym))
    (constantp sym)
    (special-operator-p sym)
    (symbol-plist sym)
    sn (symbol-package sym)
    (if (boundp sym) (symbol-value sym))
    (if (fboundp sym) (type-of (symbol-function sym)))
    (fboundp sym)))))))
(packagesyminfo :cl)





(defun packagesyminfo (p0)
 (let ((p (find-package p0)))
 (do-all-symbols (sym)    
  (when (eq (symbol-package sym) p)
   (format t "symbol_package('~a','~a').~%"
    sn (package-name (symbol-package sym)))))))
(packagesyminfo :cl)


|#
