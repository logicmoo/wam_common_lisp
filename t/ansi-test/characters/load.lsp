;;;; Character tests
(compile-and-load "ANSI-TESTS:AUX;char-aux.lsp")

(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-pathname*))))
  (load "character.lsp")
  (load "char-compare.lsp")
  (load "name-char.lsp")
)
