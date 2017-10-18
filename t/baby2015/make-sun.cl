;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;; replace >home>juergen>Babylon-2.3> by the the pathname you put babylon 
;;; use ">" as seperator!!!
 

(defvar *bab-host* "")

(load "kernel/require.cl")
(load "kernel/babtrans.cl")

(setf *recompile* t)

;;------------------------------------------------------------------------ 

(defun make-local-pathname (bab-path host type)
  (declare (ignore host))
  (let ((true-type (case type
                     (source "cl")
                     (bin   #+:EXCL "fasl"
			    #+:CMU  "cmu"
			    #+:AKCL "o"
			    #+:CLISP "fas"
			    #-(or :EXCL :CMU :AKCL :CLISP) "bin" )
                     (t      (string-downcase type)))))
    (merge-pathnames (substitute #\/ #\> bab-path)
                     (concatenate 'string "/foo." true-type))))

(setf *trans-path-fkt*  #'make-local-pathname)


(defbabylon-translation "babhome^"  ">home>juergen>Babylon>Babylon-2.3>")  ;;; <--- change !!!
(defbabylon-translation "fmcs^"     "babhome^fmcs>")
(defbabylon-translation "tty^"      "babhome^tty>")

(defbabylon-translation "basic-interface-mixin" "b-interf")
(defbabylon-translation "mini-interface-mixin"  "m-interf")

(setq *babylon-module-search-path*
      '("tty^modules>" "modules^" "configs^"))

;;------------------------------------------------------------------------ 

(cc-load "tty^extens")

;#+:FLAVORS(cc-load "fls^fls-map")
(bab-require 'fmcs)
(bab-require 'common)
(bab-require 'meta)

(bab-require 'free-text-mixin)
(bab-require 'normal-frame-mixin)
(bab-require 'normal-rule-mixin)
(bab-require 'normal-prolog-mixin)
(bab-require 'normal-constraint-mixin)

;;; replace by your implementation of normal-interface-mixin later on

(bab-require 'mini-interface-mixin)
(def$flavor normal-interface-mixin () (mini-interface-mixin))

(cc-load "tty^customs.cl")

;;------------------------------------------------------------------------ 

(defun babylon-hello ()
  (format *default-dialog-stream*
	  "Welcome to Babylon Release ~A~%" *babylon-version*))

;;; later on you may dumplisp an image here

(defun after-image-load ()
  (progn
    (load-user-babylon-init-file)
    (babylon-hello)))

(defun make-babylon-image ()
  #+:EXCL (progn (excl:gc)
		 (excl:dumplisp :name "babylon" 
				:checkpoint nil 
				:restart-function #'after-image-load))

  #+:CLISP(progn t			; (gc)
		 (LISP:SAVEINITMEM "babylon.mem" 
				   :quiet t 
				   :init-function #'after-image-load))

  #+:CMU(extensions:save-lisp "babylon.core" 
			      :print-herald t
			      :init-function #'after-image-load)

  #+:AKCL(SI:SAVE-SYSTEM "babylon.kcl")

  )

;;; eof

  