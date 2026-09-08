(in-package :cl-test)

;; This is the one helper needed to register iteration/loop.lsp without
;; loading all of ansi-aux.lsp.  The generated tests still depend on
;; SIGNALS-ERROR and are not selected until WAM-CL has condition handlers.
(defmacro def-macro-test (test-name macro-form)
  (let ((macro-name (car macro-form)))
    `(deftest ,test-name
       (values
        (signals-error (funcall (macro-function ',macro-name))
                       program-error)
        (signals-error (funcall (macro-function ',macro-name)
                                ',macro-form)
                       program-error)
        (signals-error (funcall (macro-function ',macro-name)
                                ',macro-form nil nil)
                       program-error))
       t t t)))
