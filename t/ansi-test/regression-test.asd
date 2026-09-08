(cl:in-package #:asdf-user)

(defsystem :regression-test
  :serial t
  :components
  ((:cl-source-file.lsp "rt-package")
   (:cl-source-file.lsp "rt")))
