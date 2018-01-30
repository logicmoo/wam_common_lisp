;;;
;;; Configuration file for the bootstrapping version of ECL
;;;
;;; * Set ourselves in the 'SYSTEM package
;;;
(setq *package* (find-package "SYSTEM"))
(setq si::*keep-definitions* nil)

;;;
;;; * Load Common-Lisp base library
;;;
(if (member "ECL-MIN" *features* :test #'string-equal)
  (load "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/build/lsp/load.lsp"))
(defun si::process-command-args () )

;;;
;;; * Load PCL-based Common-Lisp Object System
;;;
(setf sys::*gc-verbose* nil)
#+(and wants-clos ecl-min)
(load "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/build/clos/load.lsp")

;;;
;;; * By redefining "SYS:" ECL will be able to
;;;   find headers and libraries in the build directory.
;;;
(si::pathname-translations "SYS" '(("*.*" "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/build/*.*")))

;;;
;;; * Load the compiler.
;;;
(load #+ecl-min "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/build/cmp/load.lsp" #-ecl-min "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/build/cmp.so")

;;;
;;; * Add include path to not yet installed headers
;;;
(setq compiler::*cc-flags* (concatenate 'string compiler::*cc-flags* " -I/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/src/h -I/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/src/gmp "))

;;;
;;; * Remove documentation from compiled files
;;;
(setq si::*keep-documentation* nil)

;;;
;;; * Beppe's defsystem utility
;;;
(load "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/src/util/system.lsp")

;;;
;;; * We redefine this to force generation of source files
;;; in the object directory -- source files help debugging
;;; with GDB.
;;;
(defun sbt::sbt-compile-file (&rest s)
  (apply #'compiler::compile-file
	   (car s)
	   :c-file t :h-file t :data-file t :system-p t
	   (cdr s)))

;;;
;;; * Go back to build directory to start compiling
;;;
#+ecl-min
(setq *features* (remove :ecl-min *features*))
