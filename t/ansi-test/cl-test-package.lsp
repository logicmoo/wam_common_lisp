;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 14 10:13:21 1998
;;;; Contains: CL test case package definition

(defvar *extrinsic-symbols* nil
  "A list of symbols that will imported and shadowed in the CL-TEST package.
These symbols will be imported and shadowed before the COMMON-LISP package
is USEd. This makes it possible to test systems that implement part of the
ANSI standard without replacing the corresponding functionality in the host
CL implementation.

For example, to test a system FOO that implements the LOOP facility one
would set *EXTRINSIC-SYMBOLS* to '(FOO:LOOP FOO:LOOP-FINISH) before loading
ansi-test.

For more information see
https://gitlab.common-lisp.net/ansi-test/ansi-test/-/merge_requests/61")

(let* ((name :cl-test)
       (pkg (find-package name)))
  (unless pkg
    (setq pkg (make-package name :use '())))
  (let ((*package* pkg))
    (import (list* 'common-lisp-user::compile-and-load
                   'common-lisp-user::compile-and-load*
                   *extrinsic-symbols*))
    (shadow (list* '#:handler-case
                   '#:handler-bind
                   *extrinsic-symbols*))
    (use-package '(:cl :regression-test))
    (export (mapcar #'intern
                    (mapcar #'symbol-name
                            '(#:random-from-seq #:random-case #:coin
                              #:random-permute #:*universe* #:*mini-universe*
                              #:*cl-symbols*
                              #:signals-error #:typef)))))
  (let ((s (find-symbol "QUIT" "CL-USER")))
    (when s (import s :cl-test))))


