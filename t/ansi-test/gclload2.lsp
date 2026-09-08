;;; Load test files
(in-package :cl-test)

;;; Tests of symbols
(load "symbols/load.lsp")

;;; Tests of evaluation and compilation
(load "eval-and-compile/load.lsp")

;;; Tests of data and control flow
(load "data-and-control-flow/load.lsp")

;;; Tests of iteration forms
(load "iteration/load.lsp")

;;; Tests of objects
(load "objects/load.lsp")

;;; Tests of conditions
(load "conditions/load.lsp")

;;; Tests of conses
(load "cons/load.lsp")

;;; Tests on arrays
(load "arrays/load.lsp")

;;; Tests of hash tables
(load "hash-tables/load.lsp")

;;; Tests of packages

(load "packages/load.lsp")

;;; Tests of numbers (section 12)
(load "numbers/load.lsp")

;;; Tests of sequences
(load "sequences/load.lsp")

;;; Tests of structures
(load "structures/load.lsp")

;;; Tests of types and classes
(load "types-and-classes/load.lsp")

;;; Tests of strings
(load "strings/load.lsp")

;;; Tests for character functions
(load "characters/load.lsp")

;;; Tests of pathnames
(load "pathnames/load.lsp")

;;; Tests of file operations
(load "files/load.lsp")

;;; Tests of streams
(load "streams/load.lsp")

;;; Tests of the printer
(load "printer/load.lsp")

;;; Tests of the reader
(load "reader/load.lsp")

;;; Tests of system construction
(load "system-construction/load.lsp")

;;; Tests of environment
(load "environment/load.lsp")

;;; Miscellaneous tests, mostly tests that failed in random testing
;;; on various implementations
(load "misc/load.lsp")
