;;; Uncomment the next line to make MAKE-STRING and MAKE-SEQUENCE
;;; tests require that a missing :initial-element argument defaults
;;; to a single value, rather than leaving the string/sequence filled
;;; with arbitrary legal garbage.
;; (pushnew :ansi-tests-strict-initial-element *features*)

#+allegro (setq *enclose-printer-errors* nil)

(let ((wd (or *compile-file-pathname* *load-pathname*)))
  (when wd
    (setf *default-pathname-defaults*
          (make-pathname :name nil :type nil :version nil :defaults wd))))

;;; Remove compiled files
(let* ((fn (compile-file-pathname "doit.lsp"))
       (type (pathname-type fn))
       (dir-pathname (make-pathname :name :wild :type type))
       (subdir-pathname (make-pathname :directory '(:relative :wild)
                                       :name :wild :type type))
       (format-pathname (make-pathname :directory '(:relative "printer" "format")
                                       :name :wild :type type))
       (files (append (directory dir-pathname)
                      (directory subdir-pathname)
                      (directory format-pathname))))
  (assert type)
  (assert (not (string-equal type "lsp")))
  (mapc #'delete-file files))

(load "gclload1.lsp")
(load "gclload2.lsp")

#+allegro
(progn
  (rt:disable-note :nil-vectors-are-strings)
  (rt:disable-note :standardized-package-nicknames)
  (rt:disable-note :type-of/strict-builtins)
  (rt:disable-note :assume-no-simple-streams)
  (rt:disable-note :assume-no-gray-streams))

#+lispworks
(progn
  (rtest:disable-note :allow-nil-arrays)
  (rtest:disable-note :nil-vectors-are-strings))

#+cmu
(progn
  (setf ext:*ignore-extra-close-parentheses* nil)
  (rt:disable-note :nil-vectors-are-strings))

#+gcl(si::use-fast-links nil)

#+clisp
(progn                          ; see also clisp/utils/clispload.lsp
  ;; Paul Dietz assumes a particular implementation for sequence functions
  ;; (MAKE-SEQUENCE, CONCATENATE, MAP, ...) that rejects result types like
  ;; (OR (VECTOR BIT) (VECTOR T)) because the element type is ambiguous.
  ;; CLISP handles these ambiguous cases by computing the union type of the
  ;; possible element types and therefore does not need to give an error.
  (rt:disable-note :result-type-element-type-by-subtype)
  ;; for the pretty-printer
  (setq custom:*pprint-first-newline* nil)
  ;; for READ-BYTE.ERROR.3 READ-BYTE.ERROR.4 READ-BYTE.ERROR.6
  ;;  WRITE-BYTE.ERROR.3 OPEN.66 OPEN.OUTPUT.30
  (setq custom:*reopen-open-file* 'warn)
  ;; For ENSURE-DIRECTORIES-EXIST.8
  (when (ext:probe-directory "scratch/")
    (mapc #'delete-file (directory "scratch/*"))
    (ext:delete-dir "scratch/"))
  ;; A few tests call DISASSEMBLE. Make it work without user intervention.
  (setf (ext:getenv "PAGER") "cat")
  ;; One test exceeds the memory available in the SPVW_PURE_BLOCKS model.
  (when (and (= (logand (sys::address-of nil) #xffffff) 0) ; SPVW_PURE_BLOCKS ?
             (<= (integer-length most-positive-fixnum) 26)) ; 32-bit machine ?
    ;; Inhibit the CHAR-INT.2 test.
    (rt:rem-test 'cl-test:char-int.2)))

(in-package :cl-test)

;;; These two tests will misbehave if the tests are being
;;; invoked from a file that is being loaded, so remove them
(when *load-pathname*
  (mapc #'regression-test:rem-test '(load-pathname.1 load-truename.1)))

;; We could use uiop:chdir here, but what about new implementations?
(setf *default-pathname-defaults* (truename #P"sandbox/"))

(time (regression-test:do-tests))

#+allegro (cl-user::exit)
#+(or cmu sbcl gcl armedbear clisp) (cl-user::quit)
