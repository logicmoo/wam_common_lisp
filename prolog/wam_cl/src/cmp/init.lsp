(defun lcs1 (file)
       (compile-file file
                     :c-file t :h-file t :data-file t :o-file t
                     :system-p t))

(setq *print-array* t *print-length* 6)

;;; ----------------------------------------------------------------------
;;; Macros only used in the code of the compiler itself:

(in-package 'compiler)
(import 'sys::arglist 'compiler)

;;; from cmpenv.lsp
(defmacro next-cmacro () '(incf *next-cmacro*))
(defmacro next-cfun () '(incf *next-cfun*))

;;; from cmplabel.lsp
(defmacro next-label () `(cons (incf *last-label*) nil))

(defmacro next-label* () `(cons (incf *last-label*) t))

(defmacro wt-label (label)
  `(when (cdr ,label) (wt-nl1 "L" (car ,label) ":")))

(defmacro wt-go (label)
  `(progn (rplacd ,label t) (wt "goto L" (car ,label) ";")))

;;; from cmplam.lsp
(defmacro ck-spec (condition)
  `(unless ,condition
           (cmperr "The parameter specification ~s is illegal." spec)))

(defmacro ck-vl (condition)
  `(unless ,condition
           (cmperr "The lambda list ~s is illegal." vl)))

;;; from cmpmain.lsp

(defmacro get-output-pathname (file ext)
  `(make-pathname :directory (or (and (not (null ,file))
                                      (not (eq ,file t))
                                      (pathname-directory ,file))
                                 dir)
                  :name (or (and (not (null ,file))
                                 (not (eq ,file t))
                                 (pathname-name ,file))
                            name)
                  :type ,ext))

;;; fromcmputil.sp
(defmacro safe-compile (&rest forms) `(when *safe-compile* ,@forms))
(defmacro cmpck (condition string &rest args)
  `(if ,condition (cmperr ,string ,@args)))

;;; from cmpwt.lsp
(defmacro wt (&rest forms &aux (fl nil))
  (dolist (form forms (cons 'progn (nreverse (cons nil fl))))
    (if (stringp form)
        (push `(princ ,form *compiler-output1*) fl)
        (push `(wt1 ,form) fl))))

(defmacro wt-h (&rest forms &aux (fl nil))
  (dolist (form forms)
    (if (stringp form)
      (push `(princ ,form *compiler-output2*) fl)
      (push `(wt-h1 ,form) fl)))
  `(progn (terpri *compiler-output2*) ,@(nreverse (cons nil fl))))

(defmacro princ-h (form) `(princ ,form *compiler-output2*))

(defmacro wt-nl (&rest forms)
  `(wt #\Newline #\Tab ,@forms))

(defmacro wt-nl1 (&rest forms)
  `(wt #\Newline ,@forms))
;;; ----------------------------------------------------------------------

(in-package 'user)

(defun compile-if-needed (file)
  (let ((cfile-date (file-write-date (merge-pathnames file #".c"))))
    (when (or (not cfile-date)
	      (> (file-write-date (merge-pathnames file #".lsp"))
		 cfile-date))
      (compile-file file :c-file t :h-file t :data-file t
		    :o-file nil :system-p t)))
  )
#|
(sys:allocate 'cons 200 t)
(sys:allocate 'symbol 50 t)

(load "load")
(compile-if-needed "cmputil")
(compile-if-needed "cmptype")
(compile-if-needed "cmpbind")
(compile-if-needed "cmpblock")
(compile-if-needed "cmpcall")
(compile-if-needed "cmpcatch")
(compile-if-needed "cmpenv")
(compile-if-needed "cmpeval")
(compile-if-needed "cmpexit")
(compile-if-needed "cmpflet")
(compile-if-needed "cmpfun")
(compile-if-needed "cmpif")
(compile-if-needed "cmpinline")
(compile-if-needed "cmplam")
(compile-if-needed "cmplet")
(compile-if-needed "cmploc")
(compile-if-needed "cmpmap")
(compile-if-needed "cmpmulti")
(compile-if-needed "cmpspecial")
(compile-if-needed "cmptag")
(compile-if-needed "cmptop")
(compile-if-needed "cmpvar")
(compile-if-needed "cmpwt")
(compile-if-needed "cmpmain")
(compile-if-needed "cmpdefs")
#-ecl(bye)
(quit)
|#
