;;; Tests of symbols
(compile-and-load "ANSI-TESTS:AUX;cl-symbols-aux.lsp")

(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-pathname*))))
  (load "cl-symbols.lsp")
  (load "symbolp.lsp")
  (load "keywordp.lsp")
  (load "make-symbol.lsp")
  (load "copy-symbol.lsp")
  (load "gensym.lsp")
  (load "gentemp.lsp")
  (load "symbol-function.lsp")
  (load "symbol-name.lsp")
  (load "boundp.lsp")
  (load "special-operator-p.lsp")
  (load "makunbound.lsp")
  (load "set.lsp")
  (load "remprop.lsp")
  (load "get.lsp"))
