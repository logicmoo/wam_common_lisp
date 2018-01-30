;;; ----------------------------------------------------------------------
;;;	COMMON LISP LIBRARY
;;; ----------------------------------------------------------------------

(defparameter
  *lsp-modules*
  ;; file	load		compile			files which force
  ;;		environment	environment		recompilations of
  ;;							this file
  '(
    (export	()		()			())
    (defmacro	()		()			())
    (helpfile	()		()			())
#-runtime
    (evalmacros	()		()			())
    (module	()		()			())
    (autoload	()		()			())
    (setf	()		()			())
    (arraylib	()		()			())
    (predlib	()		()			())
    (assert	()		()			())
    (defstruct	()		()			())
    (listlib	()		()			())
    (mislib	()		()			())
    (numlib	()		()			())
    (packlib	()		()			())
    (seq	()		()			())
    (seqlib	()		()			())
    (iolib	()		()			())
    (ansi	()		()			())
#+old-loop
    (loop	()		()			())
#-old-loop
    (loop2	()		()			())
    (defpackage ()		()			())
    (ffi	()		()			())
#-runtime
    (describe	()		()			())
    (top	()		()			())
    (trace	()		()			())
    (config	()		()			())
#+threads
    (thread	()		()			())
#+nil ; This is loaded as source file
    (tk-init	()		()			())))

(sbt:defsystem 
 lsp
 :modules *lsp-modules*
 :source-directory '("/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/src/lsp/" "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/build/lsp/")
 :fasl-directory "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/build/lsp/"
 :library-directory "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/build/")
