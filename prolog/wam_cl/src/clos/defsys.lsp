;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;
;;;

(in-package 'user)

(defparameter *pathname-types*	'("lsp" . "o"))
#+PDE
(setq si:*record-source-pathname-p* nil) ; No recording in distribution

;;; ----------------------------------------------------------------------
;;;	CLOS
;;; ----------------------------------------------------------------------

(defparameter
  *clos-modules*
  ;; file       load              compile               files which force
  ;;            environment       environment           recompilations of
  ;;                                                    this file
  '(
    (walk      ()           	  ()               	())
    (macros    (walk)      	  (walk)                ())
    (method    (macros)           (macros)              ())
    (kernel    (method)           (method)      	(method))
    (slot      ()	          ()			())
    (combin    (method)          (method)		(method))
    (dcode-pre1 t		 t			())
    (boot      (kernel slot combin)
	       			 (kernel slot)   	(kernel slot))
    (print (boot)		  (boot)		(boot))
    (defclass  (kernel slot)      (kernel slot)         (boot))
    (standard  (boot defclass dcode-pre1)
	      			 (boot defclass)	(method defclass))
    (inspect   (standard)        (standard)             (standard))
    (change    (standard)	 (standard)		(standard))
    (built-in  (standard)        (standard)	   	(standard))
    (std-method(built-in)	 (built-in)	        (built-in))
    (generic   (std-method)      (std-method)           ())
    (fixup     (std-method)      (std-method)           (standard))
    ))

(sbt:defsystem 
 clos
 :modules *clos-modules*
 :directory (cons "" "")
 :pathname-types *pathname-types*)
