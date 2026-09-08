;;; Tests of structures

(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-pathname*))))
  (load "structure-00.lsp")
  (load "structures-01.lsp")
  (load "structures-02.lsp")
  (load "structures-03.lsp")
  (load "structures-04.lsp")
)
