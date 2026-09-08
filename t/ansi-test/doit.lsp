(let* ((wd (or *compile-file-truename* *load-truename*))
       (*default-pathname-defaults* (if wd
                                        (make-pathname :name nil :type nil :version nil
                                                       :defaults wd)
                                        *default-pathname-defaults*)))
  (load "init.lsp"))

;;; These two tests will misbehave if the tests are being
;;; invoked from a file that is being loaded, so remove them
(when *load-pathname*
  (mapc #'regression-test:rem-test '(cl-test::load-pathname.1 cl-test::load-truename.1)))

(time (regression-test:do-tests :exit t))
