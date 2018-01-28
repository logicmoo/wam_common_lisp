;;;
;;; Configuration file for the remaining libraries of ECL
;;;
(load "bare.lsp")

;;;
;;; * Compile and link MIT CLX extensions
;;;
(setq si::*keep-documentation* nil)
(in-package "COMMON-LISP-USER")
#+WANTS-CLX
(progn
(push :clx-ansi-common-lisp *features*)
(load "clx/defsys.lsp")
(sbt::operate-on-system clx :library)
(compiler::build-program "eclx" :lisp-files '(#+(and (not DLOPEN) WANTS-CMP) cmp clx))
)
(print "HOLA")