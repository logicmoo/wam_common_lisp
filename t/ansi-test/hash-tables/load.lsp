(compile-and-load "ANSI-TESTS:AUX;hash-table-aux.lsp")

(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-pathname*))))
  (load "hash-table.lsp")
  (load "make-hash-table.lsp")
  (load "hash-table-p.lsp")
  (load "hash-table-count.lsp")
  (load "hash-table-size.lsp")
  (load "hash-table-rehash-size.lsp")
  (load "hash-table-rehash-threshold.lsp")
  (load "hash-table-test.lsp")
  (load "gethash.lsp")
  (load "remhash.lsp")
  (load "clrhash.lsp")
  (load "maphash.lsp")
  (load "with-hash-table-iterator.lsp")
  (load "sxhash.lsp")
)
