;; Standalone ANSI bootstrap tests: pathname syntax and directory resolution.
;; Run from the repository root:
;;   .\wam_cl.cmd < t\ansi-test-wamcl\bootstrap\05-pathnames-and-reader-literals.lisp

(in-package :cl-user)

(defparameter *ab-passed* 0)
(defparameter *ab-failed* 0)
(defparameter *ab-total* 10)
(unless (boundp '*ab-all-passed*) (setq *ab-all-passed* 0))
(unless (boundp '*ab-all-failed*) (setq *ab-all-failed* 0))
(unless (boundp '*ab-all-total*) (setq *ab-all-total* 0))

(defun ab-record (name expected actual)
  (if (equal expected actual)
      (progn
        (setq *ab-passed* (+ *ab-passed* 1))
        (print (list :ansi-bootstrap-test name :pass)))
      (progn
        (setq *ab-failed* (+ *ab-failed* 1))
        (print (list :ansi-bootstrap-test name :fail
                     :expected expected :actual actual))))
  nil)

(defun ab-test (name expected actual)
  (ab-record name expected actual))

(defun ab-summary (phase)
  (let ((failed (- *ab-total* *ab-passed*)))
    (setq *ab-all-passed* (+ *ab-all-passed* *ab-passed*))
    (setq *ab-all-failed* (+ *ab-all-failed* failed))
    (setq *ab-all-total* (+ *ab-all-total* *ab-total*))
    (print (list :ansi-bootstrap-summary phase
                 :passed *ab-passed* :failed failed :total *ab-total*))
    (print (list :ansi-bootstrap-total
                 :passed *ab-all-passed*
                 :failed *ab-all-failed*
                 :total *ab-all-total*)))
  nil)

(ab-test :pathname-reader-literal
         t
         (if (pathnamep #P"foo.txt") t nil))

(ab-test :quoted-pathname-literal
         t
         (if (pathnamep '#P"foo.txt") t nil))

(ab-test :nested-quoted-pathname-list
         t
         (if (pathnamep (car '(#P"foo.txt"))) t nil))

(ab-test :nested-quoted-pathname-vector
         t
         (if (pathnamep (aref '#(#P"foo.txt") 0)) t nil))

;; These spellings are WAM-CL adapter requirements, not portable ANSI
;; canonical-namestring requirements.
(ab-test :wam-pathname-namestring
         "foo.txt"
         (namestring #P"foo.txt"))

(ab-test :wam-relative-directory-namestring
         "./sandbox/"
         (namestring (pathname "sandbox/")))

(ab-test :wam-root-directory-namestring
         "/"
         (namestring (pathname "/")))

(ab-test :wam-windows-directory-namestring
         "C:/foo/"
         (namestring (pathname "C:/foo/")))

(ab-test :directory-probe
         t
         (if (pathnamep (probe-file #P"t/ansi-test/")) t nil))

(ab-test :directory-truename
         t
         (if (pathnamep (truename #P"t/ansi-test/")) t nil))

(ab-summary 'pathnames-and-reader-literals)
