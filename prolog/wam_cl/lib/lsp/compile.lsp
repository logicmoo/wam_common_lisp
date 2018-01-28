;;;
;;; Configuration file for the bootstrapping version of ECL
;;;
(load "bare.lsp")

;;;
;;; Dump documentation
;;;
(load "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/reference/ecl-OLD/src/doc/help.lsp")
(si::dump-documentation "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/reference/ecl-OLD/build/help.doc")

;;;
;;; Trick to make names shorter
;;;
(rename-package "CL" "CL" '("COMMON-LISP" "LISP"))

;;;
;;; * Compile, load and link Common-Lisp base library
;;;
(setq si::*keep-documentation* nil)
(in-package "COMMON-LISP-USER")
(load "lsp/defsys.lsp")
(proclaim '(optimize (safety 2) (space 3)))
(sbt::operate-on-system lsp :library)
#+dlopen
(progn
  (sbt::operate-on-system lsp :shared-library)
  (load "lsp"))
(si::pathname-translations "SYS" '(("**;*.*" "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/reference/ecl-OLD/build/**/*.*")))
(setq compiler::*cc-flags* (concatenate 'string compiler::*cc-flags* " -I/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/reference/ecl-OLD/src/h -I/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/reference/ecl-OLD/src/gmp -I./h"))

;;;
;;; * Compile, load and link PCL based Common-Lisp Object System
;;;
#+WANTS-CLOS
(progn
(load "clos/defsys.lsp")
(proclaim '(optimize (safety 2) (space 3)))
(let ((c::*compile-to-linking-call* nil))
  (sbt::operate-on-system clos :library))
;(sbt::operate-on-system clos :load)
)

;;;
;;; * Compile, load and link Common-Lisp to C compiler
;;;
#+WANTS-CMP
(progn
(load "cmp/defsys.lsp")
(proclaim '(optimize (safety 2) (space 3)))
(sbt::operate-on-system cmp #-dlopen :library #+dlopen :shared-library)
;(sbt::operate-on-system cmp :load)
)

(compiler::build-program "ecl" :lisp-files '(#+(and (not dlopen) WANTS-CMP) cmp))

(quit)
