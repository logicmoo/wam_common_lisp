;;; Uncomment the next line to make MAKE-STRING and MAKE-SEQUENCE
;;; tests require that a missing :initial-element argument defaults
;;; to a single value, rather than leaving the string/sequence filled
;;; with arbitrary legal garbage.
;; (pushnew :ansi-tests-strict-initial-element *features*)

#+allegro (run-shell-command "rm -f *.fasl")
#+cmu (run-program "rm" (list "-f"
			      (concatenate 'string "*."
					   (pathname-type (compile-file-pathname "a.lisp")))))

(load "gclload1.lsp")
(load "gclload2.lsp")

(setq rt::*compile-tests* t)

#+allegro
(progn
  (rt:disable-note :nil-vectors-are-strings)
  (rt:disable-note :standardized-package-nicknames)
  (rt:disable-note :type-of/strict-builtins)
  (rt:disable-note :assume-no-simple-streams)
  (rt:disable-note :assume-no-gray-streams))

(in-package :cl-test)

;;; These two tests will misbehave if the tests are being
;;; invoked from a file that is being loaded, so remove them
(when *load-pathname*
  (mapc #'regression-test:rem-test '(load-pathname.1 load-truename.1)))

;; We could use uiop:chdir here, but what about new implementations?
(setf *default-pathname-defaults* (truename #P"sandbox/"))

(time (regression-test:do-tests))

#+allegro :exit
#+(or cmu sbcl gcl) (quit)
