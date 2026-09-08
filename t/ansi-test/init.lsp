;;; Uncomment the next line to make MAKE-STRING and MAKE-SEQUENCE
;;; tests require that a missing :initial-element argument defaults
;;; to a single value, rather than leaving the string/sequence filled
;;; with arbitrary legal garbage.
;; (pushnew :ansi-tests-strict-initial-element *features*)

#+allegro (setq *enclose-printer-errors* nil)

(defvar *root-path*
  (let ((wd (or *compile-file-truename* *load-truename*)))
    (if wd
        (make-pathname :name nil :type nil :version nil
                       :defaults wd)
        *default-pathname-defaults*)))

(let ((*default-pathname-defaults* *root-path*))
  ;;; Remove compiled files
  (let* ((fn (compile-file-pathname "init.lsp"))
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
  (load "gclload2.lsp"))

(let ((*default-pathname-defaults* *root-path*))
  #+allegro
  (rt:load-expected-failures #P"expected-failures/acl.sexp" :if-does-not-exist nil)

  #+clasp
  (rt:load-expected-failures #P"expected-failures/clasp.sexp" :if-does-not-exist nil)

  #+lispworks
  (rt:load-expected-failures #P"expected-failures/lispworks.sexp" :if-does-not-exist nil)

  #+cmucl
  (progn
    ;; Initialize the random state so that the random tests are
    ;; consistent when we run them.  (Provided we run all of them in the
    ;; same order.)
    (setf *random-state* (kernel::make-random-object :state (kernel::init-random-state)))
    (setf ext:*ignore-extra-close-parentheses* nil)
    #+linux
    (rt:load-expected-failures #P"expected-failures/cmucl-linux.sexp" :if-does-not-exist nil)
    #+darwin
    (rt:load-expected-failures #P"expected-failures/cmucl-darwin.sexp" :if-does-not-exist nil))

  #+gcl
  (si::use-fast-links nil)

  #+clisp
  (progn ; see also clisp/utils/clispload.lsp
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
      (rt:rem-test 'cl-test:char-int.2))))
