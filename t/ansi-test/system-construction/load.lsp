;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Dec 12 19:44:29 2004
;;;; Contains: Load tests for system construction (section 24)

(in-package :cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-pathname*))))
  (load "compile-file.lsp")
  (load "load-file.lsp")
  (load "with-compilation-unit.lsp")
  (load "features.lsp")
  (load "modules.lsp"))
