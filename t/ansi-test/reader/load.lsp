;;;; Tests of the reader
(compile-and-load "ANSI-TESTS:AUX;reader-aux.lsp")

(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-pathname*))))
  (load "reader-test.lsp")
  (load "with-standard-io-syntax.lsp")
  (load "copy-readtable.lsp")
  (load "read.lsp")
  (load "read-preserving-whitespace.lsp")
  (load "read-delimited-list.lsp")
  (load "read-from-string.lsp")
  (load "readtable-case.lsp")
  (load "readtablep.lsp")
  (load "get-macro-character.lsp")
  (load "set-macro-character.lsp")
  (load "read-suppress.lsp")
  (load "set-syntax-from-char.lsp")
  (load "dispatch-macro-characters.lsp")

  (load "syntax.lsp")
  (load "syntax-tokens.lsp"))
