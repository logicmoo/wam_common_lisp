;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan  1 11:59:35 2004
;;;; Contains: Load tests of section 20, 'Files'

(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-pathname*))))
  (load "directory.lsp")
  (load "probe-file.lsp")
  (load "ensure-directories-exist.lsp")
  (load "truename.lsp")
  (load "file-author.lsp")
  (load "file-write-date.lsp")
  (load "rename-file.lsp")
  (load "delete-file.lsp")
  (load "file-error.lsp")
)
