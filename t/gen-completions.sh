#! /usr/bin/env sbcl
(let (symbols)
  (do-all-symbols (sym)
    (let ((package (symbol-package sym)))
      (cond
        ((not (fboundp sym)))
        ((or (eql #.(find-package :cl) package)
             (eql #.(find-package :cl-user) package))
         (pushnew (symbol-name sym) symbols))
        ((eql #.(find-package :keyword) package)
         (pushnew (concatenate 'string ":" (symbol-name sym)) symbols))
        (package
          (pushnew (concatenate 'string (package-name package)
                               ":"
                               (symbol-name sym))
                  symbols)))))
  (with-open-file (output #.(concatenate 'string (posix-getenv "HOME")
                                         "/.sbcl_completions")
                          :direction :output :if-exists :overwrite
                          :if-does-not-exist :create)
    (format output "~{~(~A~)~%~}" (sort symbols #'string<))))
