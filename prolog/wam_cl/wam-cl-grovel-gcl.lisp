;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/sys-proclaim.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/sys-proclaim.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/dummy.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/dummy.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_packages.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_packages.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/dbind.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package 'si)

;(defun joe ()
;  (dbind ((a) b) (foo) (print (list a b))))

(defmacro destructuring-bind
  (al val &body body &aux  *dl* (*key-check* nil)
		    (*arg-check* nil) (sym (gensym)))
  (dm-vl al sym t)
  `(compiler::stack-let
     ((,sym (cons nil ,val)))
     (let* (,@ (nreverse *dl*))
       ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/dbind.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_cmpinit.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;(proclaim '(optimize (safety 2) (space 3)))

;(setq compiler::*eval-when-defaults* '(compile eval load))
;(or (fboundp 'si::get-&environment) (load "gcl_defmacro.lsp"))
;(or (get 'si::s-data 'si::s-data)
;    (progn (load "../lsp/setf.lsp") (load "../lsp/defstruct.lsp")))
;(if (probe-file "sys-proclaim.lisp")(load "sys-proclaim.lisp"))



;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_cmpinit.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_pathname_match_p.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun to-regexp (x &optional (rp t) &aux (px (pathname x))(lp (typep px 'logical-pathname)))
  (to-regexp-or-namestring (mlp px) rp lp))

(deftype compiled-regexp nil `(vector unsigned-char))

(defun pathname-match-p (p w &aux (s (namestring p)))
  (declare (optimize (safety 1)))
  (check-type p pathname-designator)
  (check-type w (or compiled-regexp pathname-designator))
  (and (zerop (string-match (if (typep w 'compiled-regexp) w (to-regexp w)) s))
       (eql (match-end 0) (length s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_pathname_match_p.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_desetq.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-




(defun desetq-consp-check (val)
  (or (consp val) (error "~a is not a cons" val)))

(defun desetq1 (form val)
  (cond ((symbolp form)
	 (cond (form			;(push form *desetq-binds*)
		`(setf ,form ,val))))
	((consp form)
	 `(progn
	    (desetq-consp-check ,val)
	    ,(desetq1 (car form) `(car ,val))
	    ,@ (if (consp (cdr form))
		   (list(desetq1 (cdr form) `(cdr ,val)))
		 (and (cdr form) `((setf ,(cdr form) (cdr ,val)))))))
	(t (error ""))))


(defmacro desetq (form val)
  (cond ((atom val) (desetq1 form val))
	(t (let ((value (gensym)))
	     `(let ((,value ,val)) , (desetq1 form value))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_desetq.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_stack-problem.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package :si)

(defvar *old-handler* #'si::universal-error-handler)

(defentry ihs_function_name (object) (object "ihs_function_name"))


(defun new-universal-error-handler
  (a b c d e &rest l &aux (i 0) (top (si::ihs-top)))
  (declare (fixnum  i top))
  (if (search "stack overflow" e)
      (progn (format t "~a in ~a" e d)
	     (format t "invocation stack:")
	     (loop (cond ((or (> i 20)
			      (< top 10))
			  (return nil)))
		   (setq i (+ i 1))
		   (setq top (- top 1))
		   (format t "< ~s " (ihs_function_name (si::ihs-fun top))))
	     (format t "Jumping to top")
	     (throw *quit-tag* nil)
	     )
    (apply *old-handler* a b c d e l)))


(setf (symbol-function 'si::universal-error-handler)
      #'new-universal-error-handler)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_stack-problem.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_doc-file.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(defun doc-file (file packages)
;;Write FILE of doc strings for all symbols in PACKAGES
;;This file is suitable for use with the find-doc function.
  #+kcl
  (and (member 'lisp packages)
       (not (documentation 'setq 'function))
       (load (format nil "~a../lsp/setdoc.lsp" si::*system-directory*)))
  (with-open-file (st file :direction :output)
   (sloop:sloop
    for v in packages
    do (setq v (if (packagep v) (package-name v) v))
    do (sloop:sloop
	for w in-package v
	when  (setq doc (documentation w 'function))
	do (format st "F~a~%~ain ~a package:~a" w
		   (cond ((special-operator-p w) "Special Form ")
			 ((functionp w) "Function ")
			 ((macro-function w) "Macro ")
			 (t ""))
		   v
		   doc)
	when (setq doc (documentation w 'variable))
	do (format st "V~a~%Variable in ~a package:~a" w v doc)
	))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_doc-file.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_dl.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(export '(mdlsym mdl lib-name))

(defun lib-name (p)
  (if (or (string= p "") (string= p "libc") (string= p "libm")) "" 
    (string-concatenate #+darwin "/usr/lib/system/" p #+darwin ".dylib" #-darwin ".so")));FIXME

(defun mdl (n p vad)
  (let* ((sym (mdlsym n (lib-name p)))
	 (ad (symbol-value sym))
	 (adp (aref %init vad)))
    (dladdr-set adp ad)
    (dllist-push %memory sym adp)))

(defun mdlsym (str &optional (n "" np))
  (let* ((pk (or (find-package "LIB") (make-package "LIB")))
	 (k  (if np (dlopen n) 0))
	 (ad (dlsym k str))
	 (p (or (dladdr ad t) ""));FIXME work around dladdr here, not posix
	 (psym (intern p pk))
	 (npk (or (find-package psym) (make-package psym :use '(:cl))))
	 (sym (and (shadow str npk) (intern str npk))))
    (export (list psym) pk)
    (export sym npk)
    (set psym k)(set sym ad)
    sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_dl.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_logical_pathname_translations.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defvar *pathname-logical* nil)

(defun (setf logical-pathname-translations) (v k)
  (declare (optimize (safety 1)))
  (check-type v list)
  (check-type k string)
  (setf (cdr (or (assoc k *pathname-logical* :test 'string-equal) (car (push (cons k t) *pathname-logical*)))) ;(cons k nil)
	(mapcar (lambda (x) (list (parse-namestring (car x) k) (parse-namestring (cadr x)))) v)))

;(defsetf logical-pathname-translations (x) (y) `(setf-logical-pathname-translations ,y ,x))
(remprop 'logical-pathname-translations 'si::setf-update-fn)

(defun logical-pathname-translations (k)
  (declare (optimize (safety 1)))
  (check-type k string)
  (cdr (assoc k *pathname-logical* :test 'string-equal)))


(defun load-logical-pathname-translations (k)
  (declare (optimize (safety 1)))
  (unless (logical-pathname-translations k)
    (error "No translations found for ~s" k)))

(defun logical-pathname-host-p (host)
  (when host
    (logical-pathname-translations host)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_logical_pathname_translations.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_merge_pathnames.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun merge-pathnames (p &optional (def *default-pathname-defaults*) (def-v :newest)
			  &aux dflt (pn (pathname p))(def-pn (pathname def)))
  (declare (optimize (safety 1)))
  (check-type p pathname-designator)
  (check-type def pathname-designator)
  (check-type def-v (or null (eql :newest) seqind))
  (labels ((def (x) (when x (setq dflt t) x)))
    (let ((h (or (pathname-host pn) (def (pathname-host def-pn))))
	  (c (or (pathname-device pn) (def (pathname-device def-pn))))
	  (d (let ((d (pathname-directory pn))(defd (pathname-directory def-pn)))
	       (or (def (when (and defd (eq (car d) :relative)) (append defd (cdr d)))) d (def defd))))
	  (n (or (pathname-name pn) (def (pathname-name def-pn))))
	  (p (or (pathname-type pn) (def (pathname-type def-pn))))
	  (v (or (pathname-version pn) (def (unless (pathname-name pn) (pathname-version def-pn))) (def def-v))))
      (if dflt
	  (make-pathname :host h :device c :directory d :name n :type p :version v)
	pn))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_merge_pathnames.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_wild_pathname_p.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun wild-namestring-p (x)
  (when (stringp x) (>= (string-match #v"(\\*|\\?|\\[|\\{)" x) 0)))

(defun wild-dir-element-p (x)
  (or (eq x :wild) (eq x :wild-inferiors) (wild-namestring-p x)))

(defun wild-path-element-p (x)
  (or (eq x :wild) (wild-namestring-p x)))

#.`(defun wild-pathname-p (pd &optional f)
     (declare (optimize (safety 1)))
     (check-type pd pathname-designator)
     (check-type f (or null (member ,@+pathname-keys+)))
     (case f
       ((nil) (or (wild-namestring-p (namestring pd))
		  (when (typep pd 'pathname);FIXME stream
		    (eq :wild (pathname-version pd)))))
       ;; ((nil) (if (stringp pd) (wild-namestring-p pd)
       ;; 		(let ((p (pathname pd)))
       ;; 		  (when (member-if (lambda (x) (wild-pathname-p p x)) +pathname-keys+) t))))
       ((:host :device) nil)
       (:directory (when (member-if 'wild-dir-element-p (pathname-directory pd)) t))
       (:name (wild-path-element-p (pathname-name pd)))
       (:type (wild-path-element-p (pathname-type pd)))
       (:version (wild-path-element-p (pathname-version pd)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_wild_pathname_p.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_loadcmp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package :compiler)


(format t "~%Loading the whole compiler...")
(let ((sysd (concatenate 'string si::*system-directory* "../cmpnew/")))
  (load (merge-pathnames (concatenate 'string si::*system-directory*
				      "../lsp/defstruct")))
  (dolist (v     '(
		  
		   "cmpinline" 
		   "cmputil" 
		   "cmptype" 
		   "cmpbind" 
		   "cmpblock" 
		   "cmpcall" 
		   "cmpcatch" 
		   "cmpenv" 
		   "cmpeval" 
		   "cmpflet" 
		   "cmpfun" 
		   "cmpif" 
		   "cmplabel" 
		   "cmplam" 
		   "cmplet" 
		   "cmploc" 
					; "cmpmain" 
		   "cmpmap" 
		   "cmpmulti" 
		   "cmpspecial" 
		   "cmptag" 
		   "cmptop" 
		   "cmpvar" 
		   "cmpvs" 
		   "cmpwt" 
	))
	 (load (merge-pathnames v sysd)))
  (load (merge-pathnames "cmpmain.lsp" sysd)))

(defun compile-file
    (&rest system::args &aux (*print-pretty* nil) (*package* *package*))
    (compiler::init-env) (apply 'compiler::compile-file1 system::args))
(defun compile (&rest system::args &aux (*print-pretty* nil))
    (apply 'compiler::compile1 system::args))
(defun disassemble (&rest system::args &aux (*print-pretty* nil))
    (apply 'compiler::disassemble1 system::args))
;(setf (symbol-function 'si:clear-compiler-properties)
;       (symbol-function 'compiler::compiler-clear-compiler-properties))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_loadcmp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gprof1.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package 'si)

;;  (load "gprof.o")
;;  On a sun in sun0S 3 or 4.0
;;  make a modified copy of /lib/gcrt0.o called gcrt0-mod.o
;;  then (cd unixport ; make "EXTRAS=../lsp/gcrt0-mod.o")
;;  after compiling some .o files with
;;  (cd o ; make  "CFLAGS = -I../h -pg -g -c")
;;   (invoke gprof-setup)
;;  and (monitor #x800 3000000)
;;  (monitor 0 0) to start and stop respectively
;; on suns the buffersize = (highpc- lowpc)/2   +6


(clines "#include \"gprof.hc\"")

(defun gprof-setup (&optional (n 800000) (m 1000000))
   (mymonstartup #x800 n)
   (set-up-monitor-array m)
(format t"   ;;  and (monitor #x800 3000000)
   ;;  (monitor 0 0) to start and stop respectively
	")
   nil)

(defentry mymonstartup (int int) (int "mymonstartup"))
;(defentry monitor1 (int int object) (int "mymonitor"))
(defentry monitor2 (int int int int) (int "mymonitor"))

(defentry write_outsyms () (int "write_outsyms"))

(defvar *monitor-array* nil)

(defun set-up-monitor-array (&optional (n 1000000))
  (unless *monitor-array* (setf *monitor-array*
				(make-array n :element-type 'string-char
					    :static t))
	  ;(mymonstartup 0 2000000)
	  nil
	  ))

(defun monitor (low high)
  (monitor1 low high *monitor-array*))

(defun write-syms.out ()
  (set-up-combined)
  (write_outsyms))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gprof1.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/fast-mv.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package 'compiler)

;; Author W. Schelter

;; Using fast-values in place of values, and fast-multiple-setq
;; allow functions to still be declared to have only 1 value, while
;; in effect returning several.   This allows a great speed up in
;; returning extra values.   Eventually we may incorporate this system
;; to allow similar code to be put out where multiple values are proclaimed
;; for the function.

;; The primitives set-mv and  mv-ref provide access to 10 storage places
;; directly by address, without the indirection of going through an array
;; or symbol.

;; Sample usage:

;;(proclaim '(function goo-fast-mv () t))
;;(proclaim '(function foo-fast-mv (t) t))
;;
;;(defun foo-fast-mv (n)
;;  (let (x y z)
;;   (sloop for i below n
;;	 do (fast-multiple-value-setq (x y z) (goo-fast-mv)))
;;  (list x y z)))
;;
;;(defun goo-fast-mv  () (fast-values 1 2 7))

(defmacro fast-values (a &rest l)
  (or (< (length l) 10) (error "too many values"))
  `(prog1 ,a ,@ (sloop::sloop for v in l
		       for i below 10
		       collect `(si::set-mv ,i ,v))))

(defmacro fast-multiple-value-setq ((x &rest l) form)
  (or (< (length l) 10) (error "too many values"))
  `(prog1 (setq ,x ,form)
     ,@ (sloop::sloop for i below 10
	       for v in l
	       collect `(setq ,v (si::mv-ref ,i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/fast-mv.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_rm.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun eval-feature (x)
  (cond ((atom x)
         (member x *features*))
        ((eq (car x) :and)
         (dolist (x (cdr x) t) (unless (eval-feature x) (return nil))))
        ((eq (car x) :or)
         (dolist (x (cdr x) nil) (when (eval-feature x) (return t))))
        ((eq (car x) :not)
	 (not (eval-feature (cadr x))))
	(t (error "~S is not a feature expression." x))))


(defun sharp-+-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (if (eval-feature (let ((*read-suppress* nil) 
			  (*read-base* 10.)
			  (*package* (load-time-value (find-package 'keyword))))
		      (read stream t nil t)))
      (values (read stream t nil t))
    (let ((*read-suppress* t)) (read stream t nil t) (values))))

(set-dispatch-macro-character #\# #\+ 'sharp-+-reader)
(set-dispatch-macro-character #\# #\+ 'sharp-+-reader
                              (si::standard-readtable))

(defun sharp---reader (stream subchar arg)
  (declare (ignore subchar arg))
  (if (eval-feature (let ((*read-suppress* nil)
			  (*read-base* 10.)
			  (*package* (load-time-value (find-package 'keyword))))
		      (read stream t nil t)))
      (let ((*read-suppress* t)) (read stream t nil t) (values))
    (values (read stream t nil t))))

(set-dispatch-macro-character #\# #\- 'sharp---reader)
(set-dispatch-macro-character #\# #\- 'sharp---reader
                              (si::standard-readtable))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_rm.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_namestring.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun namestring (x)
  (declare (optimize (safety 1)))
  (check-type x pathname-designator)
  (typecase x
    (string x)
    (pathname (c-pathname-namestring x))
    (stream (namestring (c-stream-object1 x)))))

(defun file-namestring (x &aux (px (pathname x)))
  (declare (optimize (safety 1)))
  (check-type x pathname-designator)
  (namestring (make-pathname :name (pathname-name px) :type (pathname-type px) :version (pathname-version px))))

(defun directory-namestring (x &aux (px (pathname x)))
  (declare (optimize (safety 1)))
  (check-type x pathname-designator)
  (namestring (make-pathname :directory (pathname-directory px))))

(defun host-namestring (x &aux (px (pathname x)))
  (declare (optimize (safety 1)))
  (check-type x pathname-designator)
  (or (pathname-host px) ""))

#.`(defun enough-namestring (x &optional (def *default-pathname-defaults*) &aux (px (pathname x))(pdef (pathname def)))
     (declare (optimize (safety 1)))
     (check-type x pathname-designator)
     (check-type def pathname-designator)
     ,(labels ((new? (k &aux (f (intern (concatenate 'string "PATHNAME-" (string k)) :si)))
		     `(let ((k (,f px))) (unless (equal k (,f pdef)) k))))
	`(namestring (make-pathname
	  ,@(mapcan (lambda (x) (list x (new? x))) +pathname-keys+)))))

(defun faslink (file name &aux (pfile (namestring (merge-pathnames (make-pathname :type "o") (pathname file))))(*package* *package*));FIXME
  (declare (optimize (safety 1)))
  (check-type file pathname-designator)
  (check-type name string)
  (faslink-int pfile name))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_namestring.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_stdlisp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-

;; Loading the following causes these non standard symbols in the LISP
;; package, to no longer be automatically exported to packages which
;; use LISP.  For example BYE will no longer be accessible from package
;; USER.   You will need to type (lisp::bye) to quit.   Of course references
;; to BYE before this file was loaded will mean the symbol BYE in the lisp
;; package.

;; Someday this file may be loaded by default in GCL, so you should
;; probably use the LISP:: prefix for these symbols, as protection
;; against that day.

(unexport
 '(LISP::LAMBDA-BLOCK-CLOSURE
   LISP::BYE LISP::QUIT LISP::EXIT LISP::IEEE-FLOATING-POINT
   LISP::DEFENTRY LISP::VOID LISP::ALLOCATE-CONTIGUOUS-PAGES
   LISP::UNSIGNED-SHORT
   LISP::DOUBLE
   LISP::BY
   LISP::GBC
   LISP::DEFCFUN
   LISP::SAVE
   LISP::MAXIMUM-CONTIGUOUS-PAGES
   LISP::SPICE
   LISP::DEFLA
   LISP::ALLOCATED-PAGES
   LISP::SUN
   LISP::INT
   LISP::USE-FAST-LINKS
   LISP::CFUN
   LISP::UNSIGNED-CHAR
   LISP::HELP
   LISP::HELP*
   LISP::MACRO
   LISP::*BREAK-ENABLE*
   LISP::CLINES
   LISP::LAMBDA-CLOSURE
   LISP::OBJECT
   LISP::FAT-STRING
   LISP::SIGNED-SHORT
   LISP::MC68020
   LISP::LAMBDA-BLOCK
   LISP::TAG
   LISP::PROCLAMATION
   LISP::ALLOCATED-CONTIGUOUS-PAGES
   LISP::*EVAL-WHEN-COMPILE*
   LISP::SIGNED-CHAR
   LISP::*IGNORE-MAXIMUM-PAGES*
   LISP::*LINK-ARRAY*
   LISP::KCL
   LISP::BSD
   LISP::ALLOCATE-RELOCATABLE-PAGES
   LISP::ALLOCATE
   LISP::UNIX
   LISP::MAXIMUM-ALLOCATABLE-PAGES
   LISP::ALLOCATED-RELOCATABLE-PAGES
   LISP::SYSTEM
   LISP::KYOTO
   LISP::CCLOSURE)
 'LISP
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_stdlisp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_autocmp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;;SAMPLE USAGE:
;;(def-autocomp foo (a b) (+ a b))
;;(def-autocomp goo (a b) (- a b))
;;
;;(foo 3 4) ==> 7 (after compiling foo and goo together..)
;;
;;Note:  Might want to have a *use-count* which only compiles
;;after *use-count* gets above say 10.  Thus it would only compile
;;the set of *new-definitions* when there were more than 10.
;;Would need to change the following slightly. Instead of storing the defun
;;store the lambda form, and have the autocomp do an apply of the lambda
;;form while incrementing the *use-count*.  This is probably much better,
;;since the *use-count* much more accurately reflects the cost of not compiling
;;This code is obsolete before being used!!  But I have to go now..

(require "SLOOP")
(use-package "SLOOP")


(defvar *new-definitions* nil)

(defun compile-new-definitions (name)
  (and name
       (or (member name *new-definitions*)
	   (error "~a is not in  *new-definitions*" name)))
  (let ((lisp-file "cmptemp.lisp")(o-file "cmptemp.o"))
    ;;in case somehow order matters..
    (setq *new-definitions* (nreverse *new-definitions*))
    (with-open-file (st lisp-file :direction :output)
		    (sloop for v in *new-definitions*
			   do (princ (get v 'new-definition) st)))
    (compile-file lisp-file :output-file o-file)
    (load o-file)
    (setq *new-definitions* nil)))

(defun autocomp (name args)
  (compile-new-definitions name)
  (apply name args))

(defmacro def-autocomp (fun args &rest body)
  (let ((defn (list* 'defun fun args body)))
  `(progn (push ',fun *new-definitions*)
	  (setf (get ',fun 'new-definition) ',defn)
	  (defun ,fun (&rest args)
	    (autocomp ',fun args)))))
	  





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_autocmp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_rename_file.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun set-path-stream-name (x y)
  (check-type x pathname-designator)
  (typecase x
    (synonym-stream (set-path-stream-name (symbol-value (synonym-stream-symbol x)) y))
    (stream (c-set-stream-object1 x y))))

(defun rename-file (f n &aux (pf (pathname f))(pn (merge-pathnames n pf nil))
		      (tpf (truename pf))(nf (namestring tpf))
		      (tpn (translate-logical-pathname pn))(nn (namestring tpn)))
  (declare (optimize (safety 1)))
  (check-type f pathname-designator)
  (check-type n (and pathname-designator (not stream)))
  (unless (rename nf nn)
    (error 'file-error :pathname pf :format-control "Cannot rename ~s to ~s." :format-arguments (list nf nn)))
  (set-path-stream-name f pn)
  (values pn tpf (truename tpn)))

(defun user-homedir-pathname (&optional (host :unspecific hostp))
  (declare (optimize (safety 1)))
  (check-type host (or string list (eql :unspecific)))
  (unless hostp
    (pathname (home-namestring "~"))))

(defun delete-file (f &aux (pf (truename f))(nf (namestring pf)))
  (declare (optimize (safety 1)))
  (check-type f pathname-designator)
  (unless (if (eq :directory (stat nf)) (rmdir nf) (unlink nf))
    (error 'file-error :pathname (pathname nf) :format-control "Cannot delete pathname."))
  t)

(defun file-write-date (spec)
  (declare (optimize (safety 1)))
  (check-type spec pathname-designator)
  (multiple-value-bind
      (tp sz tm) (stat (namestring (truename spec)))
    (+ tm (* (+ 17 (* 70 365)) (* 24 60 60)))))

  
(defun file-author (spec)
  (declare (optimize (safety 1)))
  (check-type spec pathname-designator)
  (multiple-value-bind
      (tp sz tm uid) (stat (namestring (truename spec)))
    (uid-to-name uid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_rename_file.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_truename.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun link-expand (str &optional (b 0)	(n (length str)) fr)
  (labels ((frame (b e) (make-array (- n b) :element-type 'character
				    :displaced-to str :displaced-index-offset b :fill-pointer (- e b)))
	   (set-fr (fr e &aux (fr (or fr (frame 0 b)))) (setf (fill-pointer fr) e) fr))
    (let* ((i (string-match #v"/" str b))
	   (fr (set-fr fr (if (eql i -1) n i)))
	   (l (when (eq (stat fr) :link) (readlinkat 0 fr))))
      (cond (l (let ((b (if (eql #\/ (aref l 0)) 0 b)))
		 (link-expand (concatenate 'string (set-fr fr b) l (frame (if (eql i -1) n i) n)) b)))
	    ((eql i -1) str)
	    ((link-expand str (1+ i) n fr))))))

(defun logical-pathname-designator-p (x)
  (typecase x
    (string (logical-pathname-parse x))
    (pathname (typep x 'logical-pathname))
    (stream (logical-pathname-designator-p (pathname x)))))

;(defvar *current-dir* (pathname (concatenate 'string (getcwd) "/"))) FIXME sync with chdir

(defun truename (pd &aux (ppd (translate-logical-pathname pd))(ns (namestring ppd)))
  (declare (optimize (safety 1)))
  (check-type pd pathname-designator)
  (when (wild-pathname-p ns)
    (error 'file-error :pathname pd :format-control "Pathname is wild"))
  (let* ((ns (ensure-dir-string (link-expand ns))))
    (unless (or (zerop (length ns)) (stat ns))
      (error 'file-error :pathname ns :format-control "Pathname does not exist"))
    (let* ((d (pathname-directory ppd))
	   (d1 (subst :back :up d))
	   (ppd (if (eq d d1) ppd (make-pathname :directory d1 :defaults ppd))))
      (if (eq (car d) :absolute) ppd (merge-pathnames ppd (concatenate 'string (getcwd) "/") nil)))))


(defun probe-file (pd &aux (pn (translate-logical-pathname pd)))
  (declare (optimize (safety 1)))
  (check-type pd pathname-designator)
  (when (wild-pathname-p pn)
    (error 'file-error :pathname pn :format-control "Pathname is wild"))
  (when (eq (stat (namestring pn)) :file)
    (truename pn)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_truename.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_bnum.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package :si)

(defun cnum-type (x)
  (let ((y (c-type x)))
    (if (/= y #.(c-type #c(0 1))) y
      (case (c-type (complex-real (the complex x)))
	    (#.(c-type 0.0s0) #.(1+ c-type-max))
	    (#.(c-type 0.0)   #.(+ 2 c-type-max))
	    (otherwise y)))))

;FIXME no declaim yet in default init position
(si::putprop 'cnum-type t 'compiler::cmp-inline)

(defun isnormal (x)
  (and (isfinite x)
       (>= (if (< x 0) (- x) x) least-positive-normalized-long-float)));FIXME abs bootstrap

(defun ratio-to-double (x &aux nx ny)
  (declare (inline isnormal))
  (multiple-value-bind 
   (r x) (round x)
   (+ (float r)
      (let ((y (denominator x))
	    (x (numerator x)))
	(do ((dx (float x)) (dy (float y)))
	    ((or (zerop dx) (zerop dy)
		 (progn (setq nx (isnormal dx) ny (isnormal dy))
			(and nx ny)))
	     (/ dx dy))
	    (if nx (setq dx (* 0.5 dx)) (setq x (ash x -1) dx (float x)))
	    (if ny (setq dy (* 0.5 dy)) (setq y (ash y -1) dy (float y))))))))

(defun float (x &optional z)
  (declare (optimize (safety 2)))
  (check-type x real)
  (check-type z (or null float))
  (let ((s (typep z 'short-float)))
    (etypecase 
     x
     (short-float (if (typep z 'long-float) (* 1.0 x) x))
     (long-float  (if s (long-to-short x) x))
     (fixnum      (if s (* 1.0s0 x) (* 1.0 x)))
     (bignum      (let ((z (big-to-double x)))   (if s (long-to-short z) z)))
     (ratio       (let ((z (ratio-to-double x))) (if s (long-to-short z) z))))))

(defun realpart (x)
  (declare (optimize (safety 2)))
  (check-type x number)
  (typecase
   x
   (real x)
   (otherwise (c-ocomplex-real x))))

(defun imagpart (x)
  (declare (optimize (safety 2)))
  (check-type x number)
  (typecase
   x
   (real (if (floatp x) (float 0 x) 0))
   (otherwise (c-ocomplex-imag x))))

(defun numerator (x)
  (declare (optimize (safety 2)))
  (check-type x rational)
  (typecase
   x
   (integer x)
   (otherwise (c-ratio-num x))))

(defun denominator (x)
  (declare (optimize (safety 2)))
  (check-type x rational)
  (typecase
   x
   (integer 1)
   (otherwise (c-ratio-den x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_bnum.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/ustreams.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;;;
;;; This file contains some macros for user defined streams
;;;
;;;
;;; probably need to add some fields to "define-user-stream-type"
;;;
;;;
;;; we probably need the ability for user-defined streams to declare
;;; whether they are input/output or both
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'lisp)

(export '(make-user-stream define-user-stream-type *user-defined-stream-types*))

(defvar *user-defined-stream-types* nil) ;;; list of user defined stream types

(defun make-user-stream (str-type)
  (let (struct)
    (unless (member str-type *user-defined-stream-types*)
	    (error "Make-user-stream: ~a undefined stream type" str-type))
    (setq struct (funcall (get str-type 'lisp::str-conc-name)))
    (allocate-stream-object str-type struct)))

(defmacro define-user-stream-type (str-name
				   str-data
				   str-read-char
				   str-write-char
				   str-peek-char
				   str-force-output
				   str-close
				   str-type
				   &optional str-unread-char)
   (let ((conc-name (intern (concatenate 'string "KCL-" 
					(symbol-name str-name)))))
     nil
     `(progn
	(setf (get ',str-name 'str-conc-name) ',conc-name)
	(setf (get ',str-name 'stream) t)
	(format t "Constructor ")
	(setq lisp::*user-defined-stream-types* (cons ',str-name lisp::*user-defined-stream-types*))
	(defstruct (,str-name (:constructor ,conc-name))
	  (str-data ,str-data)  		;0
	  (str-read-char ,str-read-char)	;1
	  (str-write-char ,str-write-char)	;2
	  (str-peek-char ,str-peek-char)	;3
	  (str-force-output ,str-force-output)	;4
	  (str-close ,str-close)		;5
	  (str-type ,str-type)			;6
	  (str-unread-char ,str-unread-char)	;7
	  (str-name ',str-name)))))		;8


;;;
;;;  allocate a stream-object and patch in the struct which holds
;;;  the goodies
;;;
(Clines

" object allocate_stream_object (stream_type, new_struct)

  object stream_type;
  object new_struct;          
 {
   object x;
   x = alloc_object(t_stream);
   x->sm.sm_mode = smm_user_defined;
   x->sm.sm_object1 = new_struct;
   x->sm.sm_object0 = stream_type;
   x->sm.sm_int0 = 0;
   x->sm.sm_fp = 0;
   x->sm.sm_int1 = 0;
   return x;
}"
)

(defentry allocate-stream-object (object object) (object allocate_stream_object)) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/ustreams.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_sharp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defstruct
  context
  (vec (make-array 0 :adjustable t :fill-pointer t) :type (vector t))
  (hash nil :type (or null hash-table))
  (spice (make-hash-table :test 'eq :rehash-size 2.0) :type hash-table))

(defun get-context (i)
  (declare (fixnum i))
  (when *sharp-eq-context*
    (let ((v (context-vec *sharp-eq-context*)))
      (if (< i (length v)) (aref v i)
	(let ((h (context-hash *sharp-eq-context*)))
	  (when h (gethash1 i h)))))))

(defun push-context (i)
  (declare (fixnum i))
  (unless *sharp-eq-context* (setq *sharp-eq-context* (make-context)))
  (let* ((v (context-vec *sharp-eq-context*))(l (length v))(x (cons nil nil)))
    (cond ((< i l) (error "#~s= multiply defined" i))
	  ((= i l) (vector-push-extend x v (1+ l)) x)
	  ((let ((h (context-hash *sharp-eq-context*)))
	     (if h (when (gethash1 i h) (error "#~s= multiply defined" i)) 
	       (setf (context-hash *sharp-eq-context*) (setq h (make-hash-table :test 'eql :rehash-size 2.0))))
	     (setf (gethash i h) x))))))

(defconstant +nil-proxy+ (cons nil nil))

(defun sharp-eq-reader (stream subchar i &aux (x (push-context i)))
  (declare (ignore subchar)(fixnum i))
  (let ((y (read stream t 'eof t)))
    (when (when y (eq y (cdr x))) (error "#= circularly defined"))
    (setf (car x) (or y +nil-proxy+))
    y))

(defun sharp-sharp-reader (stream subchar i &aux (x (get-context i)))
  (declare (ignore stream subchar)(fixnum i))
  (unless x (error "#~s# without preceding #~s=" i i))
  (or (cdr x) (let ((s (alloc-spice))) (setf (gethash s (context-spice *sharp-eq-context*)) x (cdr x) s))))

(defun patch-sharp (x) 
  (typecase
   x
   (cons (setf (car x) (patch-sharp (car x)) (cdr x) (patch-sharp (cdr x))) x)
   ((vector t)
    (dotimes (i (length x) x)
      (setf (aref x i) (patch-sharp (aref x i)))))
   ((array t)
    (dotimes (i (array-total-size x) x)
      (aset1 x i (patch-sharp (row-major-aref x i)))))
   (structure
    (let ((d (structure-def x))) 
      (dotimes (i (structure-length x) x)
	(declare (fixnum i))
	(structure-set x d i (patch-sharp (structure-ref x d i))))))
   (spice (let* ((y (gethash1 x (context-spice *sharp-eq-context*)))
		 (z (car y)))
	    (unless y (error "Spice ~s not defined" x))
	    (unless (eq z +nil-proxy+) z)))
   (otherwise x)))

(set-dispatch-macro-character #\# #\= #'sharp-eq-reader)
(set-dispatch-macro-character #\# #\# #'sharp-sharp-reader)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_sharp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_hash.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun make-hash-table (&key (test 'eql) (size 1024) (rehash-size 1.5s0) (rehash-threshold 0.7s0))
  (the hash-table (make-hash-table-int test size rehash-size rehash-threshold)))

(defun hash-table-p (x)
  (declare (optimize (safety 1)))
  (typecase x (hash-table t)))

(defun htent-key (e) (*fixnum e 0 nil nil))
(setf (get 'htent-key 'compiler::cmp-inline) t)
(defun htent-value (e) (*object e 1 nil nil))
(setf (get 'htent-value 'compiler::cmp-inline) t)
(defun set-htent-key (e y) (*fixnum e 0 t y))
(setf (get 'set-htent-key 'compiler::cmp-inline) t)
(defun set-htent-value (e y) (*object e 1 t y))
(setf (get 'set-htent-value 'compiler::cmp-inline) t)

(defun gethash (x y &optional z)
  (declare (optimize (safety 1)))
  (check-type y hash-table)
  (let ((e (gethash-int x y)))
    (if (eql +objnull+ (htent-key e))
	(values z nil)
      (values (htent-value e) t))))

(defun maphash (f h)
  (declare (optimize (safety 1)))
  (check-type h hash-table)
  (let ((n (hash-table-size h)))
    (dotimes (i n)
      (let* ((e (hashtable-self h i))
	     (k (htent-key e)))
	(unless (eql +objnull+ k)
	  (funcall f (nani k) (htent-value e)))))))

(defun remhash (x y)
  (declare (optimize (safety 1)))
  (check-type y hash-table)
  (let ((e (gethash-int x y)))
    (unless (eql +objnull+ (htent-key e))
      (set-htent-key e +objnull+)
      (c-set-hashtable-nent y (1- (c-hashtable-nent y)))
      t)))

(defun clrhash (h)
  (declare (optimize (safety 1)))
  (check-type h hash-table)
  (let ((n (hash-table-size h)))
    (dotimes (i n)
      (let ((e (hashtable-self h i)))
	(set-htent-key e +objnull+)
	(set-htent-value e (nani +objnull+))));FIXNE?
    (c-set-hashtable-nent h 0)
    h))

(defun sxhash (x)
  (declare (optimize (safety 1)))
  (typecase x
	    (symbol (c-symbol-hash x))
	    (otherwise (hash-equal x 0))))

(defun hash-set (k h v)
  (declare (optimize (safety 1)))
  (check-type h hash-table)
  (let ((n (c-hashtable-nent h)))
    (when (>= (1+ n) (c-hashtable-max_ent h))
      (extend-hashtable h))
    (let ((e (gethash-int k h)))
      (when (eql +objnull+ (htent-key e))
	(c-set-hashtable-nent h (1+ n)))
      (set-htent-key e (address k))
      (set-htent-value e v))))
(setf (get 'hash-set 'compiler::cmp-inline) t)

(setf (symbol-function 'hash-table-count) (symbol-function 'c-hashtable-nent))
(setf (symbol-function 'hash-table-size)  (symbol-function 'c-hashtable-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_hash.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_module.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;    module.lsp
;;;;
;;;;                            module routines


;; (in-package 'lisp)

;; (export '(*modules* provide require))
;; (export '(documentation variable function structure type setf compiler-macro))

(in-package :system)


(defvar *modules* nil)

(defun module-string (module-name)
  (string-downcase (string module-name)))

(defun provide (module-name)
  (pushnew (module-string module-name) *modules* :test 'string=))

(defun default-module-pathlist (module-name)
  (list (make-pathname :name (module-string module-name)
		       :directory (append (pathname-directory (pathname *system-directory*))
					  (list :up "modules")))))

(defun require (module-name &optional (pl (default-module-pathlist module-name))  &aux (*default-pathname-defaults* (make-pathname)))
  (unless (member (module-string module-name) *modules* :test 'string=)
    (when pl
      (load (pop pl))
      (require module-name pl))))
          

(defun documentation (object doc-type)
  (let ((x (typecase object
		     (function (function-name object))
		     (package (find-symbol (package-name object) :keyword))
		     ((cons (member setf) (cons symbol nil)) (setf-sym object))
		     (symbol object)))
	(p (ecase doc-type
	       (variable 'variable-documentation)
	       (function 'function-documentation)
	       (structure 'structure-documentation)
	       (type 'type-documentation)
	       (setf 'setf-documentation)
	       (compiler-macro 'compiler-macro-documentation)
	       (method-combination 'method-combination-documentation)
	       ((t) 'package-documentation))))
    (when x (get x p))))


(defun find-documentation (body)
  (if (or (endp body) (endp (cdr body)))
      nil
      (let ((form (macroexpand (car body))))
        (if (stringp form)
            form
            (if (and (consp form)
                     (eq (car form) 'declare))
                (find-documentation (cdr body))
                nil)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_module.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_make-declare.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; By W. Schelter
;; Usage: (si::proclaim-file "foo.lsp") (compile-file "foo.lsp")

(in-package :si)

;; You may wish to adjust the following to output the proclamations
;; for inclusion in a file.  All fixed arg functions should be proclaimed
;; before their references for maximum efficiency.

;; CAVEAT: The following code only checks for fixed args, it does
;; not check for single valuedness BUT does make a proclamation
;; to that effect.  Unfortunately it is impossible to tell about
;; multiple values without doing a full compiler type pass over 
;; all files in the relevant system.   However the GCL compiler should
;; warn if you inadvertantly proclaim foo to be single valued and then try
;; to use more than one value.  

(DEFVAR *DECLARE-T-ONLY* NIL)
(DEFUN PROCLAIM-FILE (NAME &OPTIONAL *DECLARE-T-ONLY*)
  (WITH-OPEN-FILE 
      (FILE NAME
            :DIRECTION :INPUT)
    (LET ((EOF (CONS NIL NIL)))
      (LOOP
       (LET ((FORM (READ FILE NIL EOF)))
         (COND ((EQ EOF FORM) (RETURN NIL))
               ((MAKE-DECLARE-FORM FORM ))))))))

(DEFVAR *DEFUNS* '(DEFUN))

(DEFUN MAKE-DECLARE-FORM (FORM)
; !!!
  (WHEN
        (LISTP FORM)
   (COND ((MEMBER (CAR FORM) '(EVAL-WHEN ))
          (DOLIST (V (CDDR FORM)) (MAKE-DECLARE-FORM V)))
         ((MEMBER (CAR FORM) '(PROGN ))
          (DOLIST (V (CDR FORM)) (MAKE-DECLARE-FORM V)))
         ((MEMBER (CAR FORM) '(IN-PACKAGE DEFCONSTANT))
          (EVAL FORM))
         ((MEMBER (CAR FORM) *DEFUNS*)
          (COND
           ((AND
             (CONSP (CADDR FORM))
             (NOT (MEMBER '&REST (CADDR FORM)))
             (NOT (MEMBER '&BODY (CADDR FORM)))
             (NOT (MEMBER '&KEY (CADDR FORM)))
             (NOT (MEMBER '&OPTIONAL (CADDR FORM))))
             ;;could print  declarations here.
	    ;(print (list (cadr form)(ARG-DECLARES (THIRD FORM)(cdddr FORM))))
            (FUNCALL 'PROCLAIM
                     (LIST  'FUNCTION
                            (CADR FORM)
			    (ARG-DECLARES (THIRD FORM) (cdddr FORM))
                            T))))))))

(DEFUN ARG-DECLARES (ARGS DECLS &AUX ANS)
  (COND ((STRINGP (CAR DECLS)) (SETQ DECLS (CADR DECLS)))
	(T (SETQ DECLS (CAR DECLS))))
  (COND ((AND (not *declare-t-only*)
	       (CONSP DECLS) (EQ (CAR DECLS ) 'DECLARE))
	 (DO ((V ARGS (CDR V)))
	     ((OR (EQ (CAR V) '&AUX)
		  (NULL V))
	      (NREVERSE ANS))
	     (PUSH (DECL-TYPE (CAR V) DECLS) ANS)))
	(T (MAKE-LIST (- (LENGTH args)
			 (LENGTH (MEMBER '&AUX args)))
		      :INITIAL-ELEMENT T))))

(DEFUN DECL-TYPE (V DECLS)
  (DOLIST (D (CDR DECLS))
	  (CASE (CAR D)
		(TYPE (IF (MEMBER V (CDDR D))
			(RETURN-FROM DECL-TYPE (SECOND D))))
		((FIXNUM CHARACTER FLOAT LONG-FLOAT SHORT-FLOAT )
		 (IF (MEMBER V (CDR D)) (RETURN-FROM DECL-TYPE (CAR D))))))
  T)
			    
	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_make-declare.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_nr.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)


(eval-when
 (compile eval)

 (defmacro defcomp ((fn fn2))
   `(defun ,fn (n1 &optional (n2 n1 n2p) &rest r) 
      (declare (:dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 ,(if (member fn '(= /=)) 'number 'real))
      (check-type n2 ,(if (member fn '(= /=)) 'number 'real))
      (cond ((not n2p))
	    ((not (,fn2 n1 n2)) nil)
	    ((not r))
	    ((apply ',fn n2 (car r) (cdr r))))))

 (defmacro defpt ((fn fn2) &aux (def (if (eq fn '+) 0 1)))
   `(defun ,fn (&optional (n1 ,def) (n2 ,def) &rest r) 
      (declare (:dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 number)
      (check-type n2 number)
      (let ((z (,fn2 n1 n2)))
	(if r (apply ',fn z (car r) (cdr r)) z))))

 (defmacro defmm ((fn c))
   `(defun ,fn (n1 &optional (n2 n1) &rest r) 
      (declare (:dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 real)
      (check-type n2 real)
      (let ((z (if (,c n1 n2) n1 n2)))
	(if r (apply ',fn z (car r) (cdr r)) z))))

 (defmacro defmd ((fn fn2))
   `(defun ,fn (n1 &optional (n2 n1 n2p) &rest r) 
      (declare (:dynamic-extent r))
      (declare (optimize (safety 1)))
      (check-type n1 number)
      (check-type n2 number)
      (let* ((n1 (if n2p n1 ,(if (eq fn '-) 0 1)))
	     (z (,fn2 n1 n2)))
	(if r (apply ',fn z (car r) (cdr r)) z)))))

(defcomp (<  <2))
(defcomp (<= <=2))
(defcomp (=  =2))
(defcomp (/= /=2))
(defcomp (>= >=2))
(defcomp (>  >2))

(defpt (+ number-plus))
(defpt (* number-times))
(defmm (max >=))
(defmm (min <=))

(defmd (- number-minus))
(defmd (/ number-divide))

(labels ((fgcd2 (x y &aux (tx (ctzl x))(ty (ctzl y))(tx (min tx ty)))
		(lgcd2 (>> x tx) (>> y ty) tx (if (oddp x) (- y) (>> x 1))))
	 (lgcd2 (x y tx tt &aux (tt (>> tt (ctzl tt))))
		(if (plusp tt) (setq x tt) (setq y (- tt)))
		(if (= x y) (<< x tx) (lgcd2 x y tx (- x y))))
	 (zgcd2 (x y) (cond ((= x 0) y) ((= y 0) x) ((fgcd2 x y)))))

  (defun gcd (&rest r &aux (s (if (cdr r) r (cons 0 r))))
    (declare (optimize (safety 1))(dynamic-extent r s))
    (labels ((gcd2 (x y &aux (tp `(integer #.(1+ most-negative-fixnum) #.most-positive-fixnum)))
		   (check-type x integer)
		   (check-type y integer)
		   (if (and (typep x tp) (typep y tp))
		       (zgcd2 (abs x) (abs y))
		     (mpz_gcd x y))))
      (reduce #'gcd2 s)))
  
  (defun lcm (&rest r &aux (s (if (cdr r) r (cons 1 r)))) 
    (declare (optimize (safety 1))(dynamic-extent r s))
    (labels ((lcm2 (x y &aux (tp `(integer #.(1+ most-negative-fixnum) #.most-positive-fixnum)))
		   (check-type x integer)
		   (check-type y integer)
		   (if (and (typep x tp) (typep y tp))
		       (let* ((x (abs x))(y (abs y))(g (zgcd2 x y)))
			 (if (= 0 g) g (* x (truncate y g))))
		     (mpz_lcm x y))))
      (reduce #'lcm2 s))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_nr.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_directory.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defconstant +d-type-alist+ (d-type-list))

(defun ?push (x tp)
  (when (and x (eq tp :directory) (vector-push-extend #\/ x)))
  x)

(defun wreaddir (x s &optional y (ls (length s) lsp) &aux (y (if (rassoc y +d-type-alist+) y :unknown)))
  (when lsp (setf (fill-pointer s) ls))
  (let ((r (readdir x (car (rassoc y +d-type-alist+)) s)))
    (typecase r
      (fixnum (wreaddir x (adjust-array s (+ 100 (ash (array-dimension s 0) 1))) y))
      (cons (let ((tp (cdr (assoc (cdr r) +d-type-alist+)))) (cons (?push (car r) tp) tp)))
      (otherwise (?push r y)))))

(defun dot-dir-p (r l) (member-if (lambda (x) (string= x r :start2 l)) '("./" "../")))

(defun vector-push-string (x s &optional (ss 0) (lx (length x)) &aux (ls (- (length s) ss)))
  (let ((x (if (> ls (- (array-dimension x 0) lx)) (adjust-array x (+ ls (ash lx 1))) x)))
    (setf (fill-pointer x) (+ lx ls))
    (replace x s :start1 lx :start2 ss)))

(defun walk-dir (s e f &optional (y :unknown) (d (opendir s)) (l (length s)) (le (length e))
		   &aux (r (wreaddir d s y l)))
  (cond (r (unless (dot-dir-p r l) (funcall f r (vector-push-string e r l le) l))
	   (walk-dir s e f y d l le))
	((setf (fill-pointer s) l (fill-pointer e) le) (closedir d))))

(defun recurse-dir (x y f)
  (funcall f x y)
  (walk-dir x y (lambda (x y l) (declare (ignore l)) (recurse-dir x y f)) :directory))

(defun make-frame (s &aux (l (length s)))
  (replace (make-array l :element-type 'character :adjustable t :fill-pointer l) s))

(defun expand-wild-directory (l f zz &optional (yy (make-frame zz)))
  (let* ((x (member-if 'wild-dir-element-p l))
	 (s (namestring (make-pathname :directory (ldiff l x))))
	 (z (vector-push-string zz s))
	 (l (length yy))
	 (y (link-expand (vector-push-string yy s) l))
	 (y (if (eq y yy) y (make-frame y))))
    (when (or (eq (stat z) :directory) (zerop (length z)))
      (cond ((eq (car x) :wild-inferiors) (recurse-dir z y f))
	    (x (walk-dir z y (lambda (q e l)
			       (declare (ignore l))
			       (expand-wild-directory (cons :relative (cdr x)) f q e)) :directory));FIXME
	    ((funcall f z y))))))

(defun directory (p &key &aux (p (translate-logical-pathname p))(d (pathname-directory p))
		    (c (unless (eq (car d) :absolute) (make-frame (concatenate 'string (getcwd) "/"))))
		    (lc (when c (length c)))
		    (filesp (or (pathname-name p) (pathname-type p)))
		    (v (compile-regexp (to-regexp p)))(*up-key* :back) r)
  (expand-wild-directory d
   (lambda (dir exp &aux (pexp (pathname (if c (vector-push-string c exp 0 lc) exp))))
     (if filesp
	 (walk-dir dir exp
		   (lambda (dir exp pos)
		     (declare (ignore exp))
		     (when (pathname-match-p dir v)
		       (push (merge-pathnames (parse-namestring dir nil *default-pathname-defaults* :start pos) pexp nil) r)))
		   :file)
       (when (pathname-match-p dir v) (push pexp r))))
   (make-frame (if c "./" "")))
  r)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_directory.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_c.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun car (x)
  (declare (optimize (safety 2)))
  (check-type x list)
;  (*object (address x) 1 nil nil)
  (lit :object (:object x) "->c.c_car")
  );  (cons-car x)

(defun cdr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
;  (*object (address x) 0 nil nil)
  (lit :object (:object x) "->c.c_cdr")
 );  (cons-cdr x)

(defun cadr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (cdr x)))

(defun caar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (car x)))

(defun cdar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (car x)))

(defun cddr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (cdr x)))

(defun caaar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (caar x)))
(defun caadr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (cadr x)))
(defun cadar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (cdar x)))
(defun cdaar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (caar x)))
(defun caddr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (cddr x)))
(defun cdadr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (cadr x)))
(defun cddar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (cdar x)))
(defun cdddr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (cddr x)))



(defun caaaar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (caaar x)))
(defun caaadr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (caadr x)))
(defun caadar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (cadar x)))
(defun cadaar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (cdaar x)))
(defun cdaaar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (caaar x)))
(defun caaddr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (caddr x)))
(defun cadadr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (cdadr x)))
(defun cdaadr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (caadr x)))
(defun caddar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (cddar x)))
(defun cdadar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (cadar x)))
(defun cddaar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (cdaar x)))
(defun cdddar (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (cddar x)))
(defun cddadr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (cdadr x)))
(defun cdaddr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (caddr x)))
(defun cadddr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (car (cdddr x)))
(defun cddddr (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (cdr (cdddr x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_c.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_assert.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;    assert.lsp


(in-package :si)

(defun read-evaluated-form nil
  (format *query-io* "~&type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defun check-type-symbol (symbol value type &optional type-string 
				 &aux (type-string (when type-string (concatenate 'string ": need a " type-string))))
  (restart-case 
   (cerror "Check type again." 'type-error :datum value :expected-type type)
   (store-value (v) 
		:report (lambda (stream) (format stream "Supply a new value of ~s. ~a" symbol (or type-string "")))
		:interactive read-evaluated-form
		(setf value v)))
  (if (typep value type) value (check-type-symbol symbol value type type-string)))

(defmacro check-type (place typespec &optional string)
  (declare (optimize (safety 2)))
  `(progn (,(if (symbolp place) 'setq 'setf) ,place 
	   (the ,typespec (if (typep ,place ',typespec) ,place (check-type-symbol ',place ,place ',typespec ',string)))) nil))


(defun assert-places (places values string &rest args)
  (declare (dynamic-extent args))
  (restart-case
   (apply 'cerror "Repeat assertion." string args)
   (store-value (&rest r)
		:report (lambda (stream) (format stream "Supply a new values for ~s (old values are ~s)." places values))
		:interactive (lambda nil
			       (mapcar (lambda (x)
					 (format *query-io* "~&type a form to be evaluated for ~s:~%" x)
					 (eval (read *query-io*))) places))
		:test (lambda (c) places)
		(declare (dynamic-extent r))
		(values-list r))))

(defmacro assert (test-form &optional places string &rest args)
  (declare (dynamic-extent args))
  `(do nil (,test-form nil)
     (multiple-value-setq
	 ,places
       (apply 'assert-places ',places (list ,@places)
	      ,@(if string `(,string (list ,@args)) `("The assertion ~:@(~S~) failed." ',test-form nil))))))

(defmacro ctypecase (keyform &rest clauses &aux (key (sgen "CTYPECASE")))
  (declare (optimize (safety 2)))
;  (check-type clauses (list-of proper-list))
  `(do nil (nil)
    (typecase ,keyform
      ,@(mapcar (lambda (l)
		  `(,(car l) (return (progn ,@(subst key keyform (cdr l))))))
		clauses))
    (check-type ,keyform (or ,@(mapcar 'car clauses)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_assert.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_fdecl.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package :si)

;; by William F. Schelter

;; Conveniently and economically make operators which declare the type
;; and result of numerical operations.  For example (def-op f+ fixnum +)
;; defines a macro f+ which will give optimal code for calling + on
;; several fixnum args expecting a fixnum result.

;; Details:
;; Note these will be macros and cannot be `funcalled'.  If you add the
;; feature :debug, then code to check the types of the arguments and
;; result will be inserted, and generic operations will be used.  This is
;; useful for checking that you did not insert the wrong type
;; declarations.  The code will continue running if *dbreak* is nil,
;; returning the correct result but printing out the type mismatch, as
;; well as the actual args given so that you may more easily locate the
;; bad call in the editor.

;; It is economical, beause all the macros defined are just variations
;; of one closure, and so code is not duplicated.

;; Sample usage (with :debug in *features*):
;; The call will generate warning messages if the args or result are bad.

;; (defun foo (x a) (f+ (* 2 x) a))
;; SYSTEM>(foo 7.0 9)

;; Bad call (F+ (* 2 X) A) types:(LONG-FLOAT FIXNUM)
;; 23.0

;; Without debug (f+ a b c) becomes
;; (the fixnum (+ (the fixnum a) (the fixnum
;;                                      (+ (the fixnum b) (the fixnum c)))))
;; which is painful to write by hand, but which will give the best code.


(defmacro def-op (name type op &optional return-type)
	    `(setf (macro-function ',name) (make-operation ',type ',op
							   ',return-type)))

(defun make-operation (.type .op .return)
  (or .return (setf .return .type))
  #'(lambda (bod env) env
      (sloop for v in (cdr bod)
	     when (eq t .type) collect v into body
	     else
	     collect `(the , .type ,v) into body
	     finally (setq body `(, .op ,@ body))
	     (return
	     (if (eq t .return) body
	       `(the , .return ,body))))))

#+debug
(progn
  ;; Enable this to insert type error checking code.
(defvar *dbreak* t)
(defun callchk-type (lis old na typ sho return-type &aux result)
  (setq result (apply old lis))
  (or (and (sloop for v in lis
		  always (typep v typ))
	   (or (null return-type) (typep result return-type)))
      (format t "~%Bad call ~a types:~a" (cons na sho)
	      (sloop:sloop for v in lis collect (type-of v)))
      (and *dbreak* (break "hi")))
  result)

;; debug version:	 
(defmacro def-op (name type old &optional return-type)
  `(defmacro ,name (&rest l)
     `(callchk-type (list ,@ l) ',',old ',',name ',',type ',l ',',return-type 
		     )))
)

(def-op f+ fixnum +)
(def-op f* fixnum *)
(def-op f- fixnum -)
(def-op +$ double-float +)
(def-op *$ double-float *)
(def-op -$ double-float -)
(def-op 1-$ double-float 1-)
(def-op 1+$ double-float 1+)
(def-op f1- fixnum 1-)
(def-op f1+ fixnum 1+)
(def-op //$ double-float quot)
(def-op ^ fixnum expt)
(def-op ^$ double-float expt)
(def-op f> fixnum > t)
(def-op f< fixnum <  t)
(def-op f= fixnum = t)
(def-op lsh fixnum ash)
(def-op fixnum-remainder fixnum rem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_fdecl.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_sym.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (in-package 'lisp)
;; (export '(macro-function))
(in-package :si)

(defun macro-function (x &optional env)
  (declare (optimize (safety 1)))
  (check-type x symbol)
  (check-type env proper-list)
  (cond ((when env
	   (let* ((l (cdr (assoc x (cadr env)))))
	     (when (eq (car l) 'macro) (cadr l)))))
	((when (/= 0 (c-symbol-mflag x)) (c-symbol-gfdef x)))))

(defun special-operator-p (x)
  (declare (optimize (safety 1)))
  (check-type x symbol)
  (when (/= (address nil) (c-symbol-sfdef x)) t))

(defun find-symbol (s &optional (p *package*) &aux r)
  (declare (optimize (safety 1)))
  (check-type s string)
  (check-type p (or package string symbol character))
  (labels ((inb (h p) (package-internal p (mod h (c-package-internal_size p))))
	   (exb (h p) (package-external p (mod h (c-package-external_size p))))
	   (coerce-to-package 
	    (p)
	    (cond ((packagep p) p)
		  ((find-package p))
		  (t 
		   (cerror "Input new package" 'package-error
			   :package p 
			   :format-control "~a is not a package"
			   :format-arguments (list p)) 
		   (coerce-to-package (eval (read))))))
	   (cns (s b) (member-if (lambda (x) (declare (symbol x)) (string= x s)) b)))
	(let* ((p (coerce-to-package p))
	       (h (pack-hash s)))
	  (cond ((setq r (cns s (inb h p)))
		 (values (car r) :internal))
		((setq r (cns s (exb h p)))
		 (values (car r) :external))
		((dolist (p (c-package-uselist p))
		   (when (setq r (cns s (exb h p)))
		     (return r)))
		 (values (car r) :inherited))
		(t (values nil nil))))))


(defun symbol-value (s)
  (declare (optimize (safety 1)))
  (check-type s symbol)
  (if (boundp s) (c-symbol-dbind s)
    (error 'unbound-variable :name s)))

(defun boundp (s)
  (declare (optimize (safety 1)))
  (check-type s symbol)
  (not (eq (nani +objnull+) (c-symbol-dbind s))))


(defun symbol-name (s)
  (declare (optimize (safety 1)))
  (check-type s symbol)
  (the string 
       (or (get s 'pname)
	   (setf (get s 'pname) 
		 (symbol-string s)))));FIXME

(defun symbol-function (s)
  (declare (optimize (safety 1)))
  (check-type s symbol)
  (or (let ((x (c-symbol-sfdef s)))
	(when (nani x) (cons 'special x)))
      (let ((x (c-symbol-gfdef s)))
	(when (zerop (address x))
	  (error 'undefined-function :name s))
	(if (zerop (c-symbol-mflag s)) x (cons 'macro x)))))

(defun remprop (s i)
  (declare (optimize (safety 1)))
  (check-type s symbol)
  (remf (symbol-plist s) i))

(defun makunbound (s)
  (declare (optimize (safety 1)))
  (check-type s symbol)
  (c-set-symbol-dbind s (nani 0))
  s)

(defun set (s y)
  (declare (optimize (safety 1)))
  (check-type s symbol)
  (c-set-symbol-dbind s y))

(defun get (s y &optional d)
  (declare (optimize (safety 1)))
  (check-type s symbol)
  (getf (symbol-plist s) y d))

#-pre-gcl(defun symbolp (x) (if x (typecase x (symbol t)) t))
#+pre-gcl(defun symbolp (x) (typecase x (list (not x)) (symbol t)))
(defun keywordp (x) (typecase x (keyword t)))

;; (setf (symbol-function 'symbol-plist)   (symbol-function 'c-symbol-plist))
;; (setf (symbol-function 'symbol-package) (symbol-function 'c-symbol-hpack))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_sym.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_translate_pathname.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun lenel (x lp)
  (case x (:wild 1)(:wild-inferiors 2)(:absolute (if lp -1 0))(:relative (if lp 0 -1))
	((:unspecific nil :newest) -1)(otherwise (length x))))

(defun next-match (&optional (i 1) (k -1) (m (1- (ash (length *match-data*) -1))))
  (cond ((< k (match-beginning i) (match-end i)) i)
	((< i m) (next-match (1+ i) k m))
	(i)))

(defun mme2 (s lel lp &optional (b 0) (i (next-match)) r el
	       &aux (e (+ b (lenel (car lel) lp)))(j (match-beginning i))(k (match-end i)))
  (cond
   ((< (- b 2) j k (+ e 2))
    (let* ((z (car lel))(b1 (max b j))(e1 (min k e))
	   (z (if (or (< b b1) (< e1 e)) (subseq z (- b1 b) (- e1 b)) z))
	   (r (if el r (cons nil r))))
      (mme2 s lel lp b (next-match i k) (cons (cons z (car r)) (cdr r)) (or el (car lel)))))
   ((< (1- j) b e (1+ k))
    (let ((r (if el r (cons nil r))))
      (mme2 s (cdr lel) lp (1+ e) i (cons (cons (car lel) (car r)) (cdr r)) (or el (list (car lel))))))
   ((consp el)
    (let* ((cr (nreverse (car r))))
      (mme2 s lel lp b (next-match i k) (cons (cons (car el) (list cr)) (cdr r)))))
   (el
    (let* ((cr (nreverse (car r))))
      (mme2 s (cdr lel) lp (1+ e) i (cons (cons el cr) (cdr r)))))
   (lel (mme2 s (cdr lel) lp (1+ e) i (cons (car lel) r)))
   ((nreverse r))))

(defun do-repl (x y)
  (labels ((r (x l &optional (b 0) &aux (f (string-match #v"\\*" x b)))
	      (if (eql f -1) (if (eql b 0) x (subseq x b))
		(concatenate 'string (subseq x b f) (or (car l) "") (r x (cdr l) (1+ f))))))
    (r y x)))

(defun dir-p (x) (when (consp x) (member (car x) '(:absolute :relative))))

(defun source-portion (x y)
  (cond
   ((or (dir-p x) (dir-p y))
    (mapcan (lambda (z &aux (w (source-portion
				(if y (when (wild-dir-element-p z) (setf x (member-if 'listp x)) (pop x)) z)
				(when y z))))
   	      (if (listp w) w (list w))) (or y x)))
   ((if y (eq y :wild-inferiors) t) (if (listp x) (if (listp (cadr x)) (cadr x) (car x)) x));(or  y)
   ((eq y :wild) (if (listp x) (car x) x));(or  y)
   ((stringp y) (do-repl (when (listp x) (unless (listp (cadr x)) (cdr x))) y))
   (y)))

(defun list-toggle-case (x f)
  (typecase x
    (string (funcall f x))
    (cons (mapcar (lambda (x) (list-toggle-case x f)) x))
    (otherwise x)))

(defun mme3 (sx px flp tlp)
  (list-toggle-case
   (lnp (mme2 sx (pnl1 (mlp px)) flp))
   (cond ((eq flp tlp) 'identity)
	 (flp 'string-downcase)
	 (tlp 'string-upcase))))

(defun translate-pathname (source from to &key
				  &aux (psource (pathname source))
				  (pto (pathname to))
				  (match (pathname-match-p source from)))
  (declare (optimize (safety 1)))
  (check-type source pathname-designator)
  (check-type from pathname-designator)
  (check-type to pathname-designator)
  (check-type match (not null))
  (apply 'make-pathname :host (pathname-host pto) :device (pathname-device pto)
	 (mapcan 'list +pathname-keys+
		 (mapcar 'source-portion
			 (mme3 (namestring source) psource (typep psource 'logical-pathname) (typep pto 'logical-pathname))
			 (mlp pto)))))

(defun translate-logical-pathname (spec &key &aux (p (pathname spec)))
  (declare (optimize (safety 1)))
  (check-type spec pathname-designator)
  (typecase p
    (logical-pathname
     (let ((rules (assoc p (logical-pathname-translations (pathname-host p)) :test 'pathname-match-p)))
       (unless rules
	 (error 'file-error :pathname p :format-control "No matching translations"))
       (translate-logical-pathname (apply 'translate-pathname p rules))))
    (otherwise p)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_translate_pathname.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_profile.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-

(in-package :si)
(use-package "SLOOP")

;; Sample Usage:
;;    (si::set-up-profile 1000000) (si::prof 0 90)
;;     run program
;;    (si::display-prof)
;;    (si::clear-profile)
;;    profile can be stopped with (si::prof 0 0) and restarted with 
;;start-address will correspond to the beginning of the profile array, and
;;the scale will mean that 256 bytes of code correspond to scale bytes in the
;;profile array.
;;Thus if the profile array is 1,000,000  bytes long and the code segment is 
;;5 megabytes long you can profile the whole thing using a scale of 50
;;Note that long runs may result in overflow, and so an understating of the
;;time in a function.  With a scale of 128 it takes 6,000,000 times through 
;;a loop to overflow the sampling in one part of the code.



;(defun sort-funs (package)
;  (sloop for v in-package package with tem
;	 when (and (fboundp v) (compiled-function-p
;				(setq tem (symbol-function v))))
;	 collect (cons (function-start v) v)  into all
;	 finally (loop-return (sort all #'(lambda (x y)
;				       (< (the fixnum (car x))
;					  (the fixnum (car y))))))))
(defvar si::*profile-array*
		      (make-array 20000 :element-type 'character
				  :static t
				  :initial-element
				  (code-char 0)))

(defun create-profile-array (&optional (n 100000))
  (if *profile-array* (profile 0 0))
  (setq *profile-array*	      (make-array n :element-type 'character
				  :static t
				  :initial-element
				  (code-char 0)))
   n
  )


(defvar *current-profile* nil)

(defun pr (&optional n)
  (sloop
   with ar = si::*profile-array* declare (string ar)
   for i below (if n (min n (array-total-size ar))   (array-total-size ar))
   
   do 
   (cond ((not (= 0 i))(if (= 0 (mod i 20)) (terpri))))
   (princ (char-code (aref ar i))) (princ " "))
  (values))

(defun fprofile(fun &optional (fract 1000) offset)
  (setq *current-profile* (list  (+ (function-start (symbol-function fun))
				    (or offset 0))
				 fract))
  (apply 'profile  *current-profile* ))

;(defun foo (n) (sloop for i below n do nil))

;;problem: the counter will wrap around at 256, so that it really is not valid
;;for long runs if the functions are heavily used.  This means that
;;Remove all previous ticks from the profile array.

(defun clear-profile () (sloop  with ar = *profile-array* 
			declare (string ar)
                        for i below (array-total-size ar)
			do (setf (aref  ar i) (code-char 0))))


(defun prof-offset (addr) (* (/ (float (cadr *current-profile*)) #x10000)
			        (- addr (car *current-profile*))))

(defun prof (a b)
  (setf *current-profile* (list a b))
  (profile a b))

(defun display-prof()
   (profile 0 0)
   (apply 'display-profile *current-profile*)
   (apply 'profile *current-profile*))


(defun set-up-profile (&optional (array-size 100000)(max-funs 6000)
;			 (name "saved_kcl")(dir *system-directory*)&aux sym
			 )
;  (compiler::safe-system  (format nil "(cd ~a ; rsym ~a \"#sym\")" dir name))
;  (or (probe-file (setq sym  (format nil "~a#sym" dir))) (error "could not find ~a" sym))
;  (read-externals sym)
  (set-up-combined max-funs)
  (unless (and *profile-array*
	       (>= (array-total-size *profile-array*) array-size))
	  (print "making new array")
	  (setq *profile-array*  (make-array array-size
					     :element-type 'character
					     :static t
					     :initial-element
					     (code-char 0))))
  (format t "~%Loaded c and other function addresses~
   ~%Using profile-array length ~a ~
    ~%Use (si::prof 0 90) to start and (prof 0 0) to stop:~
    ~%This starts monitoring at address 0 ~
    ~%thru byte (256/90)*(length *profile-array*)~
    ~%(si::display-prof) displays the results" (length *profile-array*)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_profile.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_fle.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (in-package :lisp)

;; (export '(function-lambda-expression))

(in-package :si)
(export 'fle)

;; (export '(blocked-body-name parse-body-header))


;; (defun parse-body-header (x &optional doc decl ctps &aux (a (car x)))
;;   (cond 
;;    ((unless (or doc ctps) (and (stringp a) (cdr x))) (parse-body-header (cdr x) a decl ctps))
;;    ((unless ctps (when (consp a) (eq (car a) 'declare)))  (parse-body-header (cdr x) doc (cons a decl) ctps))
;;    ((when (consp a) (eq (car a) 'check-type)) (parse-body-header (cdr x) doc decl (cons a ctps)))
;;    (t (values doc (nreverse decl) (nreverse ctps) x))))

;; (defun parse-body-header (x &optional doc decl ctps)
;;   (let* ((a (car x))
;; 	 (q (macroexpand a)));FIXME is this correct?  clisp doesn't seem to think so
;;   (cond 
;;    ((unless (or doc ctps) (and (stringp q) (cdr x))) (parse-body-header (cdr x) q decl ctps))
;;    ((unless ctps (when (consp q) (eq (car q) 'declare)))  (parse-body-header (cdr x) doc (cons q decl) ctps))
;;    ((when (consp a) (eq (car a) 'check-type)) (parse-body-header (cdr x) doc decl (cons a ctps)))
;;    (t (values doc (nreverse decl) (nreverse ctps) x)))))

;; (defun make-blocked-lambda (ll decls ctps body block)
;;   (let ((body (if (eq block (blocked-body-name body)) body `((block ,block ,@body)))))
;;     `(lambda ,ll ,@decls ,@ctps ,@body)))

(defun block-lambda (ll block body)
  (multiple-value-bind
   (doc decls ctps body)
   (parse-body-header body)
   (declare (ignore doc))
   (make-blocked-lambda ll decls ctps body block)))
       
;; (defun find-doc (x &optional y)
;;   (declare (ignore y))
;;   (multiple-value-bind
;;    (doc decls ctps body)
;;    (parse-body-header x)
;;    (values doc decls (nconc ctps body))))

;; (defun blocked-body-name (body)
;;   (when (and (not (cdr body))
;; 	     (consp (car body))
;; 	     (eq (caar body) 'block))
;;     (cadar body)))

(defun get-blocked-body-name (x)
  (multiple-value-bind
   (doc decls ctps body)
   (parse-body-header (cddr x))
   (declare (ignore doc decls ctps))
   (blocked-body-name body)))


(defun compress-src (src)
  (let* ((w (make-string-output-stream))
	 (ss (si::open-fasd w :output nil nil)))
    (si::find-sharing-top src (aref ss 1))
    (si::write-fasd-top src ss)
    (si::close-fasd ss)
    (get-output-stream-string w)))

(defun uncompress-src (fun)
  (let* ((h   (call fun))
	 (fas (when h (call-src h)))
	 (fas (unless (fixnump fas) fas))
	 (ss  (if (stringp fas) (open-fasd (make-string-input-stream fas) :input 'eof nil) fas))
	 (out (if (vectorp ss) (read-fasd-top ss) ss))
	 (es  (when (eq (car out) 'lambda-closure) (mapcar 'car (cadr out))))
	 (env (when es (function-env fun 0))));(*object (c-function-env fun) 0 nil nil)
    (when env
;      (assert (= (length env) (length es))) ;FIXME closure var order
      (setf (cadr out) (mapcar 'list es env)))
    (when (vectorp ss)
      (close-fasd ss))
    out))

(defun fle (x) 
  (typecase
   x
   (function (function-lambda-expression x))
   (symbol (when (fboundp x) (unless (special-operator-p x)
			       (unless (macro-function x) (function-lambda-expression (symbol-function x))))))))


(defun function-lambda-expression (y &aux z) 
  (declare (optimize (safety 1)))
  (check-type y function)
  (let ((x (uncompress-src y)))
    (case (car x)
	  (lambda (values x nil (get-blocked-body-name x)))
	  (lambda-block (values (block-lambda (caddr x) (cadr x) (cdddr x)) nil (cadr x)))
	  (lambda-closure (values (setq z (cons 'lambda (cddr (cddr x))))  (cadr x) (get-blocked-body-name z)))
	  (lambda-block-closure (values
				 (block-lambda (caddr (cdddr x)) (cadr (cdddr x)) (cddr (cddr (cddr x)))) 
				 (cadr x) (fifth x)))
	  (otherwise (values nil t nil)))))

(defun function-src (sym)
  (let ((fun (if (symbolp sym) (symbol-to-function sym) sym)));FIXME
    (values (function-lambda-expression fun))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_fle.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/ucall.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package 'compiler)
(import 'si::switch)
(import 'sloop::sloop)
(provide "UCALL")

;;ucall is like funcall, except it assumes
;;1) its first arg has an inline-always property.
;;2) the order of evaluation of the remaining args is unimportant.

;;This can be useful when we know that the side effects caused by evaluating
;;the args do not affect the order of evaluation.
;;It also returns an indeterminate value.

(defun c1ucall (args &aux funob (info (compiler::make-info)))
  (setq funob (compiler::c1funob (car args)))
  (compiler::add-info info (cadr funob))
  (list 'ucall info funob (compiler::c1args (cdr args) info))
  )

(defun c2ucall (funob args &aux (*inline-blocks* 0)(*vs* *vs*))
  (let* ((fname (caddr funob))
	(props (car (get fname 'inline-always)))
	new-args
	)
    (or props (error "no inline-always prop"))
    (do ((v args (cdr v))
	 (types (car props) (cdr types)))
	((null v) (setq new-args (nreverse new-args)))
	(setq new-args
	      (append (inline-args (list (car v)) (list (car types)))
		    new-args)))
    (wt-nl)
    (wt-inline-loc (nth 4 props) new-args)
    (wt ";")
    (unwind-exit "Cnil")
    (close-inline-blocks)
    ))


;;Usage (comment "hi there") ; will insert a comment at that point in
;;the program.
(defun c1comment (args)
  (list 'comment (make-info) args))
(defun c2comment (args)
  (let ((string (car args)))
    (if (find #\/ string) (setq string (remove #\/ string)))
    (wt "/* "string " */")))

(defmacro comment (a) a nil)

;;Usage: (tlet (char *) jack ....)
;;--> {char * V1; ...V1..

(defun c1tlet (args &aux  (info (make-info)) (*vars* *vars*))
  (let ((sym (cadr args))
	(type (car args))
	form )
    (let ((var (c1make-var sym nil nil nil)))
      (cond ((subtypep type 'fixnum)
	     (setf (var-type var) 'fixnum)))
      (push var *vars*)
      (setq form (c1expr* (cons 'progn (cddr args)) info))
      (list 'tlet (second form) type var form))))

(defun c2tlet (type var orig &aux (stype type))
  (setf (var-loc var) (next-cvar))
  (or (stringp type) (setq stype (format nil "~(~a~)" type)))
  (setf (var-kind var)
	(cond ((subtypep type 'fixnum)
	       (setf (var-type var) 'fixnum))
	      (t 'object)))
  (if (listp type) (setq stype (string-trim "()" stype)))
  (wt-nl "{"  stype " V" (var-loc var) ";" )
  (c2expr orig)
  (wt "}"))

(si::putprop 'tlet 'c1tlet 'c1special)
(si::putprop 'tlet 'c2tlet 'c2)


(defun c1clet (args)
  (let ((string (car args))
	(form (c1expr (cons 'progn (cdr args)))))
    (list 'clet (second form) string form)))

(defun c2clet (string orig )
  (wt-nl "{" string)
  (c2expr orig)
    (wt "}"))

;;Usage: Takes a STRING and BODY.  Acts like progn
;;on the body, but the c code will have {string . c code for body}
;;Sample (clet "int jack; char *jane;" ....)
(defmacro clet (string &rest body) string `(progn ,@ body))

(si::putprop 'clet 'c1clet 'c1special)
(si::putprop 'clet 'c2clet 'c2)


(si::putprop 'comment 'c1comment 'c1special)
(si::putprop 'comment 'c2comment 'c2)


  


(si::putprop 'ucall 'c1ucall 'c1)
(si::putprop 'ucall 'c2ucall 'c2)



(defmacro def-inline (name args return-type &rest bod)
  (let* ((side-effect-p (if (member (car bod)
				    '(:side-effect nil t))
			    (prog1  (and (car bod) t) (setq bod (cdr bod)))
			  nil))
	 (inline (list args return-type side-effect-p nil (car bod))))
    `(car (push ',inline
		(get ',name 'inline-always)))))




(defmacro defun-inline (name args return-type &rest bod)
  (let* ((sym (gensym))
	 (named-args
	  (nthcdr (- 10 (length args)) '(X9 X8 X7 X6 X5 X4 X3 X2 X1 X0)))
	 (inline (eval `(def-inline ,sym ,args ,return-type ,@ bod))))
    `(progn
       (defun ,name  ,named-args
	 (declare ,@ (sloop for v in named-args for w in args
			    when (not (eq t v))
			    collect (list w v)))
	 (the ,return-type (,sym ,@ named-args)))
       (push  ',inline
	      (get ',name 'inline-always)))))

(defmacro def-ucall (fun args string)
  (let ((sym (gensym)))
    `(progn
    (def-inline ,sym ,args t t ,string)
    (defmacro ,fun (&rest args) `(ucall ',',sym ,@ args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/ucall.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_gprof.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package :si)

;;  (load "gprof.o")
;;  You must have a kcl image with profiling information and monstartup
;;  typically saved_kcp.   NOTE: if monstartup calls sbrk (true in
;;  most 4.3bsd's except sun >= OS 4.0)  you must be very careful to
;;  allocate all the space you will use prior to calling monstartup.
;;  If subsequent storage allocation causes the hole to move you will
;;  most certainly lose.  See below for instructions
;;  on how to construct saved_kcp.

;; If you want function invocation counts to be kept do
;; (setq compiler::*cc* (concatentate 'string compiler::*cc* " -pg "))
;; before compiling the relevant files.  (This is done when you load
;; lsp/gprof.o)

;; In the image saved_kcp Load in your files.  Load in gprof.o: (load
;; "lsp/gprof.o") Invoke monstartup once to setup buffers: (monstartup
;; lowpc highpc) eg. (monstartup #x800 3000000) [highpc should be a bit
;; bigger than the highest address you have seen when loading your files]
;; Use moncontrol to toggle profiling on and off: (moncontrol 0) to turn
;; profiling off, and (moncontrol 1) to turn it on.  Use
;; (wrtie-gmons+syms) to terminate with writing a gmon.out and syms.out
;; in the current directory.   I know of no way of clearing the buffers,
;; since secret routines set up the buffers, and we don't know where they
;; are or how large.  Thus all information is cumulative.

;; % gprof syms.out
;; will display the output (add -b) to make it briefer.

;; A sample session on rascal:
#|
/usr2/skcl/unixport/saved_kcp
GCL (Austin Kyoto Common Lisp)  Version(1.147) Sun May 14 15:26:07 CDT 1989
Contains Enhancements by W. Schelter

>(load "/tmp/fo")
Loading /tmp/fo.o
start address -T 1d04e0 Finished loading /tmp/fo.o
528

>(load "/usr2/skcl/lsp/gprof")
Loading gprof.o

 Adding -pg to the *cc* commandstart address -T 1d0800 
Finished loading gprof.o
2112

;; NOTE:  If the following calls sbrk [eg 4.3bsd or sun OS3 ] but not Sun OS4,
;; then you MUST make sure to allocate sufficient memory before doing
;; monstartup, so that the hole will not have to be moved.   
>(si::monstartup #x800 2000000)
2584576

>(si::goo)(si::goo)                  ;;defined in /tmp/foo.lisp
NIL

>NIL

>(si::write-gmon+syms)
writing syms..
0
[NOTE: The safest way to exit the lisp is to stop it with Ctrl-Z
and then kill it.   We do NOT want to run the exit code which
normally writes out a monitoring file].

rascal% gprof -b syms.out
...
                                  called/total       parents 
index  %time    self descendents  called+self    name    	index
                                  called/total       children

                0.00        0.00       1/200         _call_or_link [8]
                0.02        0.02     199/200         GOO [2]
[1]     49.6    0.02        0.02     200         FOO [1]
                0.02        0.00     200/203         _make_cons [4]

...  Interpretation: Foo is called 199 times by (parent) goo and once
by (parent) call_or_link (the setting up of the fast link).  Foo
itself calls (child) make_cons 200 of the 203 times that make_cons is
called...  Lower down we would see that goo is called twice.

-- /tmp/fo.lisp --
 (defun foo () (cons nil nil))
 (defun goo () (sloop::sloop for i below 100 do (foo)))
-- end of file --

|#

;;  Creating saved_gcp
;;  
;;  cd gcl
;;  make go
;;  (cd unixport ; make gcp-sun)
;;  (cd go ; ln -s ../o/makefile ../o/*.o ../o/*.c ../o/*.d ../o/*.ini  .)
;;  remove a few .o files and do
;;  (cd go ; make  "CFLAGS = -I../h -I../gcl-tk -pg -g -c")

;;  then (cd unixport ; make kcp)

(clines
 #-aix3 "#include \"gprof.hc\""
 #+aix3 "#include \"aix_gprof.hc\""
 )

(eval-when (load)
(progn (setq compiler::*cc* (CONCATENATE 'string compiler::*cc* " -pg "))
        (format t "~% Adding -pg to the *cc* command"))
)
(defun write-gmon+syms()
  (monitor2 0 0 0 0)
  (princ "writing syms..")
  (set-up-combined)
  (write_outsyms)
  )


(defentry monstartup (int int) (int "mymonstartup"))

(defentry monitor2 (int int int int) (int "mymonitor"))

(defentry moncontrol (int) (int "moncontrol"))

(defentry write_outsyms () (int "write_outsyms"))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_gprof.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_numlib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;    numlib.lsp
;;;;
;;;;                           number routines


(in-package :system)

(defconstant imag-one #C(0 1))

(defun isqrt (i)
  (declare (optimize (safety 1)))
  (check-type i (integer 0))
  (if (zerop i)
      0
    (let ((n (integer-length i)))
      (do ((x (ash 1 (ceiling n 2)))
	   (y))
	  (nil)
	(setq y (floor i x))
	(when (<= x y)
	  (return x))
	(setq x (floor (+ x y) 2))))))

(deftype bytespec nil `(cons (integer 0) (integer 0)))

(defun byte (size position)
  (declare (optimize (safety 1)))
  (check-type size (integer 0))
  (check-type position (integer 0))
  (cons size position))

(defun byte-position (bytespec)
  (declare (optimize (safety 1)))
  (check-type bytespec cons)
  (cdr bytespec))

(defun byte-size (bytespec)
  (declare (optimize (safety 1)))
  (check-type bytespec cons)
  (car bytespec))

(defun ldb (bytespec integer)
  (declare (optimize (safety 1)))
  (check-type bytespec bytespec)
  (check-type integer integer)
  (logand (ash integer (- (byte-position bytespec)))
	  (1- (ash 1 (byte-size bytespec)))))

(defun ldb-test (bytespec integer)
  (declare (optimize (safety 1)))
  (check-type bytespec bytespec)
  (check-type integer integer)
  (not (zerop (ldb bytespec integer))))

(defun dpb (newbyte bytespec integer &aux (z (1- (ash 1 (byte-size bytespec)))))
  (declare (optimize (safety 1)))
  (check-type newbyte integer)
  (check-type bytespec bytespec)
  (check-type integer integer)
  (logior (logandc2 integer (ash z (byte-position bytespec)))
	  (ash (logand newbyte z) (byte-position bytespec))))

(defun deposit-field (newbyte bytespec integer &aux (z (ash (1- (ash 1 (byte-size bytespec))) (byte-position bytespec))))
  (declare (optimize (safety 1)))
  (check-type newbyte integer)
  (check-type bytespec bytespec)
  (check-type integer integer)
  (logior (logandc2 integer z) (logand newbyte z)))

(defun mask-field (bytespec integer)
  (declare (optmize (safety 1)))
  (check-type bytespec bytespec)
  (check-type integer integer)
  (logand integer (ash (1- (ash 1 (byte-size bytespec))) (byte-position bytespec))))


(defun phase (x)
  (declare (optimize (safety 1)))
  (check-type x number)
  (if (= 0 x) 0.0
    (atan (imagpart x) (realpart x))))

(defun signum (x) 
  (declare (optimize (safety 1)))
  (check-type x number)
  (if (zerop x) x (/ x (abs x))))

(defun cis (x) 
  (declare (optimize (safety 1)))
  (check-type x real)
  (exp (* #c(0 1) (float x))))


(defun ffloor (x &optional (y 1.0s0))
  (declare (optimize (safety 1)))
  (check-type x real)
  (check-type y real)
  (multiple-value-bind (i r) (floor x y)
    (values (float i (if (floatp x) x 1.0)) r)))

(defun fceiling (x &optional (y 1.0s0))
  (declare (optimize (safety 1)))
  (check-type x real)
  (check-type y real)
  (multiple-value-bind (i r) (ceiling x y)
    (values (float i (if (floatp x) x 1.0)) r)))

(defun ftruncate (x &optional (y 1.0s0))
  (declare (optimize (safety 1)))
  (check-type x real)
  (check-type y real)
  (multiple-value-bind (i r) (truncate x y)
    (values (float i (if (floatp x) x 1.0)) r)))

(defun fround (x &optional (y 1.0s0))
  (declare (optimize (safety 1)))
  (check-type x real)
  (check-type y real)
  (multiple-value-bind (i r) (round x y)
    (values (float i (if (floatp x) x 1.0)) r)))


(defun logtest (x y) 
  (declare (optimize (safety 1)))
  (check-type x integer)
  (check-type y integer)
  (not (zerop (logand x y))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_numlib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/fasd.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package 'si)

(require "FASDMACROS" "../cmpnew/fasdmacros.lsp")
;; (test '(a (1)) 2 12.0) -->   ((a (1)) 2 12.0)

(defmacro dprint (x)
  `(if (and (boundp 'debug) debug)
    (format t "~%The value of ~a is ~s" ',x ,x)))



(defun keep (x) (setq sil x))
(defun test (&rest l &aux tab)
  (with-open-file (st "/tmp/foo.l"
		      :direction :output )
    (let* ((fd (open-fasd st :output nil (setq tab (make-hash-table :test 'eq)))))
      (declare (special *fd*))
      (si::find-sharing-top l tab)
;      (preprocess l tab)
      (sloop::sloop for v in l
		    do
		    (write-fasd-top v fd)
		    finally (close-fasd fd))))
  (test-in))

(defun preprocess1(lis table)
  (cond ((symbolp lis)
	 (and lis
	      (let ((tem (gethash lis table)))
		(cond (tem
		       (if (< (the fixnum tem) 0)
			 (setf (gethash lis table) (the fixnum (+ (the fixnum tem) -1)))))
		      (t (setf (gethash lis table) -1))))))
	((consp lis)
	 (preprocess1 (car lis) table)
	 (preprocess1 (cdr lis) table))
	((and (arrayp lis)
	      (eq (array-element-type lis) t))
	 (sloop::sloop for i below (length lis)
		       do (preprocess1 (aref (the (array t) lis) i) table)))
	((and (arrayp lis)
	      (eq (array-element-type lis) t))
	 (sloop::sloop for i below (length lis)
		       do (preprocess1 (aref (the (array t) lis) i) table)))
	(t nil)))

(defun preprocess (lis table &aux freq)
  (preprocess1 lis table)
  (sloop:sloop for (ke val) in-table table
	       with m = 0 declare (fixnum m)
	       do ;(print (list ke val))
	       (cond((> (the fixnum val) 0)
		     (SETQ m (the fixnum (+ 1 m))))
		    ((< (the fixnum val) -1)
		     (remhash ke table)
		     (push (cons val ke) freq)))
	       finally (sloop::loop-return
			(sort freq '> :key 'car ))))

(defun test-in ()
  (with-open-file (st "/tmp/foo.l" :direction :input)
      (let ((fdin (open-fasd st :input (setq eof '(nil)) (keep (make-array 10)))))
	(sloop while (not (eq eof (setq tem (read-fasd-top fdin))))
	       collect tem
	       finally
	       (dprint fdin)
	       (close-fasd fdin)))))

(defun try-write (file &aux (tab (make-hash-table :test 'eq)) (eof '(nil)))
  (with-open-file (st file)
        (with-open-file (st1 "/tmp/foo.l" :direction :output)
	  (sloop  while (not (eq eof (setq tem (read st nil eof)))) with fd
		  collect (file-position st1)
		  do(clrhash tab)

		  (setq fd (open-fasd st1 :output nil tab))
;		  (let ((prp (preprocess tem tab)))
;		    (dprint  prp))
		  (write-fasd-top tem fd)
		  (close-fasd fd)
		  (dprint tab)
		  ))))
(defvar *differed* nil)

(defun try-read (file pos &aux (tab (make-array 10)) (eof '(nil)))
  (with-open-file (st file)
        (with-open-file (st1 "/tmp/foo.l")
	  (sloop  while (not (eq eof (setq tem (read st nil eof)))) with fd with re
		  for u in pos
		  do (file-position st1 u)
		  (setq fd (open-fasd st1 :input eof tab))
		  (sloop::sloop for i below (length tab) do (setf (aref (the (array (t)) tab) i) nil))
		  (setq re (read-fasd-top fd))
		  (dprint re)
		  (unless (equalp tem re)
			  (push (list tem re) *differed*))
		 ; (assert (eq eof (read-fasd-top fd)))
		  (close-fasd fd)))))

(defun try (file)
  (let ((pos (try-write file)))
    (try-read file pos)
    (print file)
    (system (format nil "cat ~a | wc ; cat /tmp/foo.l | wc " (namestring file)))
    ))

(defvar *table* (make-hash-table :test 'eq))
(defun do-share (x)
  (si::find-sharing x *table*))




(defun read-data-file (file)
  (let ((pack-ops))
    (set-dispatch-macro-character #\# #\!
				  #'(lambda (st a b ) (setq pack-ops (read st nil nil) )))
    (with-open-file (st file)
      (let ((tem (read st nil nil)))
	(list pack-ops tem)))))


(defun write-out-data (lis fil)
  (with-open-file (st fil :direction :output)
    (let ((fd (open-fasd st :output nil (setq tab (make-hash-table :test 'eq)))))
      (find-sharing-top lis (fasd-table fd))
      (write-fasd-top (car lis) fd)
      (write-fasd-top (second lis) fd)
;      (close-fasd fd)
      fd)))

;; To convert an ascii .data file to a fasd one. 
;(setq bil (si::read-data-file "vmlisp.data") her nil)
;(SI::WRITE-OUT-DATA1 (SECOND BIL) (FIRST BIL) "/tmp/foo.l")
(defun write-out-data1 (data-vec pack-ops fil)
  (with-open-file (st fil :direction :output)
    (let ((compiler::*data* (list data-vec nil        pack-ops))
	  (compiler::*compiler-output-data* st)
	  (compiler::*fasd-data* (list (open-fasd st :output nil nil))))
      (compiler::wt-fasd-data-file)
      (car compiler::*fasd-data*))))


	     
	 
	 
  
;(setq dirs (directory "/public/spad/libraries/A*/index.KAF*"))
;(mapcar 'try dirs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/fasd.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_typecase.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defmacro typecase (keyform &rest clauses &aux (key (if (symbolp keyform) keyform (sgen))))
  (declare (optimize (safety 2)))
  (labels ((l (x &aux (c (pop x))(tp (pop c))(fm (if (cdr c) (cons 'progn c) (car c)))(y (when x (l x))))
	      (if (or (eq tp t) (eq tp 'otherwise)) fm `(if (typep ,key ',tp) ,fm ,y))))
	  (let ((x (l clauses)))
	    (if (eq key keyform) x `(let ((,key ,keyform)) ,x)))))

(defmacro etypecase (keyform &rest clauses &aux (key (if (symbolp keyform) keyform (gensym))))
  (declare (optimize (safety 2)))
;  (check-type clauses (list-of proper-list))
  (let ((tp `(or ,@(mapcar 'car clauses))))
    `(typecase ,keyform ,@clauses (t (error 'type-error :datum ,key :expected-type ',tp)))))

(defmacro infer-tp (x y z) (declare (ignore x y)) z)

(defun mkinfm (f tp z &aux (z (?-add 'progn z)))
  `(infer-tp ,f ,tp ,z))

(defun branch (tpsff x f &aux (q (cdr x))(x (car x))(z (cddr (assoc x tpsff))))
  (if q
      (mapcar (lambda (q) `((typep ,f ',q) ,(mkinfm f q z)))
	      (if (when (consp q) (eq (car q) 'or)) (cdr q) (list q)))
    `((t ,(?-add 'progn z)))))

(defun ?-add (x tp) (if (atom tp) tp (if (cdr tp) (cons x tp) (car tp))))

(defun branch1 (x tpsff f o)
  (let* ((z (mapcan (lambda (x) (branch tpsff x f)) (cdr x)))
	 (s (lremove nil (mapcar 'cdr (cdr x))))
	 (z (if s (nconc z `((t ,(mkinfm f `(not ,(?-add 'or s)) (cdar o))))) z)))
    (cons 'cond z)))
;    (if (member t z :test-not 'eq :key 'car) `(cond ,@z) (cadar z))))

(defun branches (f tpsff fnl o c)
  (mapcar (lambda (x)
	    `(,(lremove-duplicates (mapcar (lambda (x) (cdr (assoc x fnl))) (car x)))
	      ,(mkinfm f (?-add 'or (car x)) (list (branch1 x tpsff f o))))) c))

(define-compiler-macro typecase (&whole w x &rest ff)
  (let* ((bind (unless (symbolp x) (list (list (gensym) x))));FIXME sgen?
	 (f (or (caar bind) x))
	 (o (member-if (lambda (x) (or (eq (car x) t) (eq (car x) 'otherwise))) ff))
	 (ff (if o (ldiff ff o) ff))
	 (o (list (cons t (cdar o))))
	 (tps (mapcar 'cmp-norm-tp (mapcar 'car ff)))
	 (z nil) (tps (mapcar (lambda (x) (prog1 (type-and x (cmp-norm-tp `(not ,z))) (setq z (type-or1 x z)))) tps))
	 (a (type-and-list tps))(c (calist2 a))
	 (fn (best-type-of c))
	 (fm `(case (,fn ,f)
		    ,@(branches f (mapcar 'cons tps ff) (cdr (assoc fn +rs+)) o c)
		    (otherwise ,(mkinfm f `(not (or ,@(apply 'append (mapcar 'car c)))) (cdar o))))))
    (if bind `(let ,bind ,fm) fm)))

(defun funcallable-symbol-function (x) (c-symbol-gfdef x))



(defun funid-sym-p (funid &optional err)
  (typecase
   funid
   (symbol funid)
   ((cons (member setf) (cons symbol null)) (setf-sym (cadr funid)))
   (otherwise (when err (error 'type-error :datum funid :expected-type 'function-name)))))

(defun funid-sym (funid)
  (funid-sym-p funid t))

(defun funid-p (funid &optional err)
  (typecase
   funid
   (symbol funid)
   ((cons (member lambda) t) funid)
   ((cons (member setf) (cons symbol null)) (setf-sym (cadr funid)))
   (otherwise (when err (error 'type-error :datum funid :expected-type 'function-identifier)))))

(defun funid (funid)
  (funid-p funid t))






(defconstant +xi+ (let* ((a (type-and-list (list (cmp-norm-tp `(and number (not immfix))))))
			 (rl (cdr (assoc 'tp8 +rs+)))
			 (i (lremove-duplicates (mapcar (lambda (x) (cdr (assoc (cadr x) rl))) a)))
			 (mi (apply 'min i))(xi (apply 'max i))(m (apply '+ i)))
;		    (assert (= mi 1))
;		    (assert (= m (/ (* xi (1+ xi)) 2)))
		    xi))


(eval-when
 (compile eval)
 (defun mtp8b (tpi &aux (rl (cdr (assoc 'tp8 +rs+)))
		   (tp (lreduce 'type-or1 (mapcar 'car (lremove-if-not (lambda (x) (eql tpi (cdr x))) rl)) :initial-value nil)))
   `(infer-type
     'x ',tp
     (infer-type
      'y ',tp
      ,(let ((x (car (member-if (lambda (x) (eql tpi (cdr (assoc (cmp-norm-tp `(and ,(get x 'lisp-type) (not immfix))) rl))))
				'(:fixnum :float :double :fcomplex :dcomplex)))))
	 (if x `(,(intern (string-upcase (strcat "C-" x "-=="))) x y)
	   (cond ((eq tp (cmp-norm-tp 'bignum)) `(eql 0 (mpz_cmp x y)))
		 ((eq tp (cmp-norm-tp 'ratio)) `(and (eql (numerator x) (numerator y)) (eql (denominator x) (denominator y))))
		 ((eq tp (cmp-norm-tp '(complex rational))) 
		  `(and (eql (realpart x) (realpart y))
			(eql (imagpart x) (imagpart y))))
		 ((error "Unknown tp")))))))))
			   
#.`(defun num-comp (x y tp)
     (declare (fixnum tp))
     (case tp
	    ,@(let (r) (dotimes (i +xi+) (push `(,(1+ i) ,(mtp8b (1+ i))) r)) (nreverse r))))
(setf (get 'num-comp 'cmp-inline) t)

(defun eql (x y)
  (or (eq x y)
      (let ((tx (tp8 x))(ty (tp8 y))) 
	(when (= tx ty)
	  (num-comp x y tx)))))

(defun eql-with-tx (x y tx)
  (declare (fixnum tx))
  (or (eq x y)
      (let ((ty (tp8 y))) 
	(when (= tx ty)
	  (num-comp x y tx)))))
(setf (get 'eql-with-tx 'cmp-inline) t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_typecase.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_parse_namestring.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun match-beginning (i &aux (v *match-data*))
  (declare ((vector fixnum) v)(seqind i))
  (the (or (integer -1 -1 ) seqind) (aref v i)))
(defun match-end (i &aux (v *match-data*))
  (declare ((vector fixnum) v)(seqind i))
  (the (or (integer -1 -1 ) seqind) (aref v (+ i (ash (length v) -1)))))

(declaim (inline match-beginning match-end))

(defun dir-conj (x) (if (eq x :relative) :absolute :relative))

(defvar *up-key* :up)

(defun mfr (x b i) (subseq x b i));  (make-array (- i b) :element-type 'character :displaced-to x :displaced-index-offset b)

(defvar *sym-sub-alist* '((:host . nil)
			  (:device . nil)
			  (:directory . (("." . nil)(".." . :up)("*" . :wild)("**" . :wild-inferiors)))
			  (:name . (("*" . :wild)))
			  (:type . (("*" . :wild)))
			  (:version . (("*" . :wild)("NEWEST" . :newest)))))

(defun element (x b i key)
  (let* ((z (when (> i b) (mfr x b i)))
	 (w (assoc z (cdr (assoc key *sym-sub-alist*)) :test 'string-equal))
	 (z (if w (cdr w) z)))
    (if (eq z :up) *up-key* z)))

(defun dir-parse (x sep sepfirst &optional (b 0))
  (when (stringp x)
    (let ((i (search sep x :start2 b)));string-match spoils outer match results
      (when i
	(let* ((y (dir-parse x sep sepfirst (1+ i)))
	       (z (element x b i :directory))
	       (y (if z (cons z y) y)))
	  (if (zerop b)
	      (cons (if (zerop i) sepfirst (dir-conj sepfirst)) y)
	    y))))))

(defun match-component (x i k &optional (boff 0) (eoff 0))
  (element x (+ (match-beginning i) boff) (+ (match-end i) eoff) k))

(defun version-parse (x)
  (typecase x
    (string (version-parse (parse-integer x)))
;    (integer (locally (check-type x (integer 1)) x))
    (otherwise x)))

(defconstant +generic-logical-pathname-regexp+ (compile-regexp (to-regexp-or-namestring (make-list (length +logical-pathname-defaults+)) t t)))

(defun expand-home-dir (dir)
  (cond ((and (eq (car dir) :relative) (stringp (cadr dir)) (eql #\~ (aref (cadr dir) 0)))
	 (append (dir-parse (home-namestring (cadr dir)) "/" :absolute) (cddr dir)))
	(dir)))

(defun logical-pathname-parse (x &optional host def (b 0) (e (length x)))
  (when (and (eql b (string-match +generic-logical-pathname-regexp+ x b e)) (eql (match-end 0) e))
    (let ((mhost (match-component x 1 :host 0 -1)))
      (when (and host mhost)
	(unless (string-equal host mhost)
	    (error 'error :format-control "Host part of ~s does not match ~s" :format-arguments (list x host))))
      (let ((host (or host mhost (pathname-host def))))
	(when (logical-pathname-host-p host)
	  (let* ((dir (dir-parse (match-component x 2 :none) ";" :relative))
		 (edir (expand-home-dir dir)))
	  (make-pathname :host host
			 :device :unspecific
			 :directory edir
			 :name (match-component x 6 :name)
			 :type (match-component x 8 :type 1)
			 :version (version-parse (match-component x 11 :version 1))
			 :namestring (when (and mhost (eql b 0) (eql e (length x)) (eq dir edir)) x))))))))
  
(defconstant +generic-physical-pathname-regexp+ (compile-regexp (to-regexp-or-namestring (make-list (length +physical-pathname-defaults+)) t nil)))

(defun pathname-parse (x b e)
  (when (and (eql b (string-match +generic-physical-pathname-regexp+ x b e)) (eql (match-end 0) e))
    (let* ((dir (dir-parse (match-component x 1 :none) "/" :absolute))
	   (edir (expand-home-dir dir)))
      (make-pathname :directory edir
		     :name (match-component x 3 :name)
		     :type (match-component x 4 :type 1)
		     :namestring (when (and (eql b 0) (eql e (length x)) (eq dir edir)) x)))))


(defun path-stream-name (x)
  (check-type x pathname-designator)
  (typecase x
    (synonym-stream (path-stream-name (symbol-value (synonym-stream-symbol x))))
    (stream (path-stream-name (c-stream-object1 x)))
    (otherwise x)))

(defun parse-namestring (thing &optional host (default-pathname *default-pathname-defaults*) &rest r &key (start 0) end junk-allowed)
  (declare (optimize (safety 1))(dynamic-extent r))
  (check-type thing pathname-designator)
  (check-type host (or null (satisfies logical-pathname-translations)))
  (check-type default-pathname pathname-designator)
  (check-type start seqind)
  (check-type end (or null seqind))
  
  (typecase thing
    (string (let* ((e (or end (length thing)))
		   (l (logical-pathname-parse thing host default-pathname start e))
		   (l (or l (unless host (pathname-parse thing start e)))))
	      (cond (junk-allowed (values l (max 0 (match-end 0))))
		    (l (values l e))
		    ((error 'parse-error :format-control "~s is not a valid pathname on host ~s" :format-arguments (list thing host))))))
    (stream (apply 'parse-namestring (path-stream-name thing) host default-pathname r))
    (pathname
     (when host
       (unless (string-equal host (pathname-host thing))
	 (error 'file-error :pathname thing :format-control "Host does not match ~s" :format-arguments (list host))))
     (values thing start))))

(defun pathname (spec)
  (declare (optimize (safety 1)))
  (check-type spec pathname-designator)
  (if (typep spec 'pathname) spec (values (parse-namestring spec))))

(defun sharp-p-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((x (parse-namestring (read stream)))) x))

(defun sharp-dq-reader (stream subchar arg);FIXME arg && read-suppress
  (declare (ignore subchar arg))
  (unread-char #\" stream)
  (let ((x (parse-namestring (read stream)))) x))

(set-dispatch-macro-character #\# #\p 'sharp-p-reader)
(set-dispatch-macro-character #\# #\P 'sharp-p-reader)
(set-dispatch-macro-character #\# #\" 'sharp-dq-reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_parse_namestring.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_fpe.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :fpe)

(import 'si::(disassemble-instruction feenableexcept fedisableexcept fld *fixnum *float *double
				      +fe-list+ +mc-context-offsets+ floating-point-error 
				      function-by-address clines defentry))
(export '(break-on-floating-point-exceptions read-instruction))

(eval-when
    (eval compile)

  (defconstant +feallexcept+ (reduce 'logior (mapcar 'caddr +fe-list+)))


  (defun moff (i r) (* i (cdr r)))
  
  (defun stl (s &aux (s (if (stringp s) (make-string-input-stream s) s))(x (read s nil 'eof)))
    (unless (eq x 'eof) (cons x (stl s))))

  (defun ml (r) (when r (make-list (truncate (car r) (cdr r)))))

  (defun mcgr (r &aux (i -1))
    (mapcar (lambda (x y) `(defconstant ,x ,(moff (incf i) r))) (when r (stl (pop r))) (ml r)))
  
  (defun mcr (p r &aux (i -1))
    (mapcar (lambda (x) `(defconstant ,(intern (concatenate 'string p (write-to-string (incf i))) :fpe) ,(moff i r)))
	    (ml r)))

  (defmacro deft (n rt args &rest code)
  `(progn
     (clines ,(nstring-downcase 
	       (apply 'concatenate 'string
			   (symbol-name rt) " " (symbol-name n) "("
			   (apply 'concatenate 'string 
				  (mapcon (lambda (x) (list* (symbol-name (caar x)) " " (symbol-name (cadar x)) 
							     (when (cdr x) (list ", ")))) args))
			   ") "
			   code)))
     (defentry ,n ,(mapcar 'car args) (,rt ,(string-downcase (symbol-name n)))))))

#.`(progn ,@(mcgr (first +mc-context-offsets+)))
#.`(progn ,@(mcr "ST" (second +mc-context-offsets+)))
#.`(progn ,@(mcr "XMM" (third +mc-context-offsets+)))


(defconstant +top-readtable+ (let ((*readtable* (copy-readtable)))
			       (set-syntax-from-char #\, #\Space)
			       (set-syntax-from-char #\; #\a)
			       (set-macro-character #\0 '0-reader)
			       (set-macro-character #\$ '0-reader)
			       (set-macro-character #\- '0-reader)
			       (set-macro-character #\% '%-reader)
			       (set-macro-character #\( 'paren-reader)
			       *readtable*))
(defconstant +sub-readtable+ (let ((*readtable* (copy-readtable +top-readtable+)))
			       (set-syntax-from-char #\0 #\a)
			       *readtable*))
(defvar *offset* 0)
(defvar *insn* nil)
(defvar *context* nil)


(defun rf (addr w)
  (ecase w (4 (*float addr 0 nil nil)) (8 (*double addr 0 nil nil))))

(defun ref (addr p w &aux (i -1)) 
  (if p 
      (map-into (make-list (truncate 16 w)) (lambda nil (rf (+ addr (* w (incf i))) w)))
    (rf addr w)))

(defun gref (addr &aux (z (symbol-name *insn*))(lz (length z))(lz (if (eql (aref z (- lz 3)) #\2) (- lz 3) lz))
		  (f (eql #\F (aref z 0))))
  (ref addr (unless f (eql (aref z (- lz 2)) #\P)) (if (or f (eql (aref z (1- lz)) #\D)) 8 4)))

(defun reg-lookup (x) (*fixnum (+ (car *context*) (symbol-value x)) 0 nil nil))

(defun st-lookup (x) (fld (+ (cadr *context*) (symbol-value x))))
(defun xmm-lookup (x) (gref (+ (caddr *context*) (symbol-value x))))


(defun lookup (x &aux (z (symbol-name x)))
  (case (aref z 0)
    (#\X (xmm-lookup x))
    (#\S (st-lookup x))
    (otherwise (reg-lookup x))))

(defun %-reader (stream subchar &aux (*readtable* +sub-readtable+)(*package* (find-package :fpe)))
  (declare (ignore subchar))
  (let ((x (read stream)))
    (lookup (if (eq x 'st)
		(intern (concatenate 'string (symbol-name x)
				     (write-to-string
				      (if (eql (peek-char nil stream nil 'eof) #\()
					  (let ((ch (read-char stream))(x (read stream))(ch (read-char stream)))
					    (declare (ignore ch))
					    x)
					0))) :fpe) x))))

(defun 0-reader (stream subchar &aux a (s 1)(*readtable* +sub-readtable+))

  (when (eql subchar #\$) (setq a t subchar (read-char stream)))
  (when (eql subchar #\-) (setq s -1 subchar (read-char stream)))
  (assert (eql subchar #\0))
  (assert (eql (read-char stream) #\x))

  (let* ((*read-base* 16)(x (* s (read stream))))
    (if a x (let ((*offset* x)) (read stream)))))

(defun paren-reader (stream subchar &aux (*readtable* +sub-readtable+))
  (declare (ignore subchar))
  (let* ((x (read-delimited-list #\) stream)))
    (gref (+ *offset* (pop x) (if x (* (pop x) (car x)) 0)))))

(defun read-operands (s context &aux (*context* context))
  (read-delimited-list #\; s))

(defun read-instruction (addr context &aux (*readtable* +top-readtable+)
			      (i (car (disassemble-instruction addr)))(s (make-string-input-stream i))
			      (*insn* (read s)))
  (cons i (cons *insn* (when context (read-operands s context)))))


(defun fe-enable (a)
  (declare (fixnum a))
  (fedisableexcept)
  (feenableexcept a))


#.`(let ((fpe-enabled 0))
     (defun break-on-floating-point-exceptions 
       (&key suspend ,@(mapcar (lambda (x) `(,(car x) (logtest ,(caddr x) fpe-enabled))) +fe-list+) &aux r)
       (fe-enable
	(if suspend 0
	  (setq fpe-enabled 
		(logior
		 ,@(mapcar (lambda (x)
			     `(cond (,(car x) (push ,(intern (symbol-name (car x)) :keyword) r) ,(caddr x))
				    (0))) +fe-list+)))))
       r))

(defun subclasses (class)
  (when class
    (cons class (mapcan 'subclasses (si::si-class-direct-subclasses class)))))

(defun code-condition (code)
  (or (reduce (lambda (y x) (if (subtypep y x) (si::si-class-name x) y))
	  (reduce 'intersection
		  (mapcar (lambda (x) (subclasses (si::si-find-class (car x))))
			  (remove code +fe-list+ :key 'caddr :test-not 'logtest)))
	  :initial-value nil)
      'arithmetic-error))
	 
(defun floating-point-error (code addr context)
  (break-on-floating-point-exceptions :suspend t)
  (unwind-protect
    (let* ((fun (function-by-address addr))(m (read-instruction addr context)))
      ((lambda (&rest r) (apply 'error (if (find-package :conditions) r (list (format nil "~s" r)))))
       (code-condition code) 
       :operation (list :insn (pop m) :op (pop m) :fun fun :addr addr) :operands m))
    (break-on-floating-point-exceptions)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_fpe.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_s.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :s)

(export '(lisp-type defdlfun +ks+ +fl+ strcat))
(si::import-internal 'si::(\| & ^ ~ c+ c* << >> object double
			   c-object-== c-fixnum-== c-float-== c-double-== c-fcomplex-== c-dcomplex-== fcomplex dcomplex
			   string-concatenate lit seqind fixnum-length char-length cref address short int
			   cnum unsigned-char unsigned-short unsigned-int
			   package-internal package-external array-dims cmp-norm-tp tp0 tp1 tp2 tp3 tp4 tp5 tp6 tp7 tp8))

(dolist (l '((:float      "make_shortfloat"      short-float     cnum);FIXME repetitive with gcl_cmpopt.lsp
	     (:double     "make_longfloat"       long-float      cnum)
	     (:character  "code_char"            character       cnum)
	     (:char       "make_fixnum"          char            cnum)
	     (:short      "make_fixnum"          short           cnum)
	     (:int        "make_fixnum"          int             cnum)
	     (:uchar      "make_fixnum"          unsigned-char   cnum)
	     (:ushort     "make_fixnum"          unsigned-short  cnum)
	     (:uint       "make_fixnum"          unsigned-int    cnum)
	     (:fixnum     "make_fixnum"          fixnum          cnum)
	     (:long       "make_fixnum"          fixnum          cnum)
	     (:fcomplex   "make_fcomplex"        fcomplex        cnum)
	     (:dcomplex   "make_dcomplex"        dcomplex        cnum)
	     (:string     "make_simple_string"   string)
	     (:object     ""                     t)

	     (:stdesig    ""                     (or symbol string character))
	     (:longfloat  ""                     long-float)
	     (:shortfloat ""                     short-float)
	     (:hashtable  ""                     hash-table)
	     (:ocomplex   ""                     complex)
	     (:bitvector  ""                     bit-vector)
	     (:random     ""                     random-state)
	     (:ustring    ""                     string)
	     (:fixarray   ""                     (array fixnum))
	     (:sfarray    ""                     (array short-float))
	     (:lfarray    ""                     (array long-float))

	     (:real       ""                     real)

	     (:float*     nil                    nil             (array short-float) "->sfa.sfa_self")
	     (:double*    nil                    nil             (array long-float)  "->lfa.lfa_self")
	     (:long*      nil                    nil             (array fixnum)      "->fixa.fixa_self")
	     (:void*      nil                    nil             (or array symbol character) "->v.v_self")))
  (setf (get (car l) 'lisp-type) (if (cadr l) (caddr l) (cadddr l))))

(si::*make-constant '+fl+ (- (integer-length fixnum-length) 4))
(si::*make-constant '+ks+ 
		 `((:char 0 0)(:uchar 0 1)(:short 1 0)(:ushort 1 1)(:int 2 0) ,@(when (member :64bit *features*) `((:uint 2 1)))
		   (:float 2 2) (:double 3 2) (:fcomplex 3 3) (:dcomplex 4 3) 
		   (:long ,+fl+ 0) (:fixnum ,+fl+ 0) (:object ,+fl+ 5)))

(eval-when 
  (compile)
  (defmacro idefun (n &rest args) `(progn (defun ,n ,@args) (si::putprop ',n t 'si::cmp-inline) (export ',n)))
  (defmacro mffe nil
    `(progn
       ,@(mapcar (lambda (z &aux (x (pop z))(s (pop z))(m (car z))(n (intern (string-concatenate "*" (string-upcase x)))))
		   `(idefun ,n (x o s y)
			    (declare (fixnum x o)(boolean s))
			    (if s (lit ,x "((" ,(strcat x) "*)" (:fixnum x) ")[" (:fixnum o) "]=" (,x y))
			     (lit ,x "((" ,(strcat x) "*)" (:fixnum x) ")[" (:fixnum o) "]")))) +ks+)))
  (defmacro mfff nil
   `(progn
      (idefun address (x) (lit :fixnum "((fixnum)" (:object x) ")"))
      (idefun ~ (x) (declare (fixnum x)) (lit :fixnum "(~" (:fixnum x) ")"))
      ,@(mapcar (lambda (x &aux (c (consp x))(n (if c (car x) x))(s (string (if c (cdr x) x))))
		  `(idefun ,n (x y) (declare (fixnum x y)) (lit :fixnum "(" (:fixnum x) ,s (:fixnum y) ")")))
		'(& \| ^ >> << (c+ . +) (c* . *) (c- . -) (c/ . /) %))
      (idefun tp0 (x) (lit :fixnum  "tp0(" (:object x) ")"))
      (idefun tp1 (x) (lit :fixnum  "tp1(" (:object x) ")"))
      (idefun tp2 (x) (lit :fixnum  "tp2(" (:object x) ")"))
      (idefun tp3 (x) (lit :fixnum  "tp3(" (:object x) ")"))
      (idefun tp4 (x) (lit :fixnum  "tp4(" (:object x) ")"))
      (idefun tp5 (x) (lit :fixnum  "tp5(" (:object x) ")"))
      (idefun tp6 (x) (lit :fixnum  "tp6(" (:object x) ")"))
      (idefun tp7 (x) (lit :fixnum  "tp7(" (:object x) ")"))
      (idefun tp8 (x) (lit :fixnum  "tp8(" (:object x) ")"))
      ,@(mapcan (lambda (x)
	    (mapcan (lambda (y)
		      (mapcar (lambda (z)
				(let ((n (intern (string-upcase (strcat "C-" (string x) "-" (string y) "-" (string z))))))
				  `(idefun ,n (x y) (lit :boolean "(" (,x x) ,(string z) (,y y) ")")))) 
			      '(>)))
		    '(:fixnum :float :double)))
	  '(:fixnum :float :double))
      ,@(mapcan (lambda (x)
	    (mapcan (lambda (y)
		      (mapcar (lambda (z)
				(let ((n (intern (string-upcase (strcat "C-" (string x) "-" (string y) "-" (string z))))))
				  `(idefun ,n (x y) (lit :boolean "(" (,x x) ,(string z) (,y y) ")")))) 
			      '(==)))
		    '(:fixnum :float :double :fcomplex :dcomplex)))
	  '(:fixnum :float :double :fcomplex :dcomplex))
      ,@(mapcar (lambda (x &aux (tp (intern (string x)))(tp (or (eq tp 'object) tp))(n (intern (string-upcase (strcat "C-" x "-=="))))) 
		  `(idefun ,n (x y) (declare (,tp x y))(lit :boolean (,x x) "==" (,x y))))
		'(:object :fixnum :float :double :fcomplex :dcomplex)))))

(eval-when 
  (eval)
  #.`(progn 
       ,@(mapcar #'(lambda (z &aux (x (pop z))(s (pop z))(m (car z))(n (intern (string-concatenate "*" (string-upcase x)))))
		     `(progn (defun ,n (x o s y)
			       (declare (fixnum x o)(boolean s))
			       (cref (c+ x (<< o ,s)) ,(<< 1 s) ,m (if s 1 0) y))
			     (si::putprop ',n t 'si::cmp-inline)
			     (export ',n)))
		 +ks+))
  (defun mffe nil nil)
  (defun mfff nil nil))

(mffe)
(mfff)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_s.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_seq.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;   seq.lsp
;;;;
;;;;                           sequence routines


;; (in-package 'lisp)

;; (export '(make-sequence concatenate map map-into))

(in-package :system)

(defun or-sequence-tp (tp &aux (l (load-time-value `(list ,@(mapcar (lambda (x) `(vector ,x)) +array-types+) vector))))
  (let ((x (remove-duplicates (mapcar (lambda (x) (car (member x l :test 'subtypep))) (cdr tp)))))
    (unless (cdr x) (car x))))

(defun make-sequence (type size &key initial-element &aux (atp (listp type)))
  (declare (optimize (safety 1)))
  (flet ((chk (res) (unless (typep res type) (error 'type-error :datum res :expected-type type)) res))
	(case (if atp (car type) type)
	      (or (chk (make-sequence (or-sequence-tp type) size :initial-element initial-element)))
	      ((list cons member) (chk (make-list size :initial-element initial-element)))
	      ((vector array) (chk (make-vector
				    (upgraded-array-element-type (or (when atp (cadr type)) t))
				    size nil nil nil 0 nil initial-element)))
	      (otherwise (let ((ntype (expand-deftype type)))
			   (if ntype (make-sequence ntype size :initial-element initial-element)
			     (check-type type (member list vector))))))))

(defun concatenate (rt &rest seqs)
  (declare (optimize (safety 1)) (:dynamic-extent seqs))
  (macrolet
   ((++ (x) `(prog1 ,x (incf ,x))))
   (let* ((rs (make-sequence rt (reduce (lambda (y x) (+ y (length x))) seqs :initial-value 0)))
	  (rt (unless (listp rs) (array-element-type rs)))(rh rs)(i 0))
     (dolist (seq seqs rs)
       (let* ((st (unless (listp seq) (array-element-type seq)))(sh seq)(j 0)
	      (ls (if st (length seq) array-dimension-limit)))
	 (do nil ((or (>= j ls) (unless st (endp sh))))
	     (if (when rt (eq rt st)) (set-array rs (++ i) seq (++ j))
	       (let ((tmp (if st (aref seq (++ j)) (pop sh))))
		 (if rt (setf (aref rs (++ i)) tmp)
		   (setf (car rh) tmp rh (cdr rh)))))))))))

(eval-when
 (compile eval)

 (defmacro locsym (f s) `(si::sgen (concatenate 'string (string ,f) ,s)))
 
 (defmacro dyncpl (x &aux (l (locsym 'dyncpl "-LOOP")));FIXME this can't cons in a labels as it might be a separate fn.  Get do to unroll too.
   `(labels ((,l (x y) (when x (setf (car x) (car y)) (,l (cdr x) (cdr y)))))
	    (declare (notinline make-list))
	    (let ((tmp (make-list (length ,x))))
	      (declare (:dynamic-extent tmp))
	      (,l tmp ,x);Can't be mapl, used by
	     tmp)))

 ;; (defmacro seqend (seq seqs &aux (l (locsym 'seqend "-LOOP"))(ll (locsym 'seqend "-LOOP")))
 ;;   `(labels ((,ll (x) (if (listp x) array-dimension-limit (length x)))
 ;; 	     (,l (s z) (if s (,l (cdr s) (min (,ll (car s)) z)) z)))
 ;; 	    (,l ,seqs (length ,seq))))
 
 (defmacro seqval (seq place i)
   `(if (listp ,seq) (pop ,place) (aref ,seq ,i)))

 (defmacro seqvals (vals ns i)
   `(mapl (lambda (x y &aux (yc (car y))) (setf (car x) (seqval yc (car y) ,i))) ,vals ,ns)))

(defun map (rt f seq &rest sqs &aux (f (coerce f 'function)) (l (listp seq));FIXME test array-dimension-limit instead of length for lists
	       (sl (reduce (lambda (y x) (min y (length x))) sqs :initial-value (length seq)))
	       (x (when rt (make-sequence rt sl))))
  (declare (optimize (safety 1))(dynamic-extent sqs))
  (check-type rt type-spec)
  (check-type f function-designator)
  (check-type seq sequence)
  (labels ((ml (i xp seq ns vals) 
	       (unless (>= i sl)
		 (let ((tmp (apply f (if l (car seq) (aref seq i)) (seqvals vals ns i))))
		   (cond (xp (setf (car xp) tmp)) (rt (setf (aref x i) tmp))))
		 (ml (1+ i) (cdr xp) (if l (cdr seq) seq) ns vals))))
	  (ml 0 (when (consp x) x) seq (dyncpl sqs) (dyncpl sqs)) x))

;; (defun map (rt f seq &rest sqs &aux (f (tofn f)) (l (listp seq)) (sl (seqend seq sqs))(x (when rt (make-sequence rt sl))))
;;   (declare (optimize (safety 2)) (dynamic-extent sqs))
;;   (check-type rt type-spec)
;;   (check-type f fn)
;;   (check-type seq sequence)
;;   (labels ((ml (i xp seq ns vals) 
;; 	       (unless (or (>= i sl) (when l (endp seq)) (member-if (lambda (x) (when (listp x) (endp x))) ns))
;; ;	       (when (and (< i sl) (if l (not (endp seq)) t) (not (member-if (lambda (x) (when (listp x) (endp x))) ns)))
;; 		 (let ((tmp (apply f (if l (car seq) (aref seq i)) (seqvals vals ns i))))
;; 		   (cond (xp (setf (car xp) tmp)) (rt (setf (aref x i) tmp))))
;; 		 (ml (1+ i) (cdr xp) (if l (cdr seq) seq) ns vals))))
;; 	  (ml 0 (when (consp x) x) seq (dyncpl sqs) (dyncpl sqs)) x))

;; (defun map (rt f seq &rest sqs &aux (seqs (cons seq sqs)) (l (length seqs)) (vals (make-list l)))
;;   (declare (optimize (safety 2)))
;;   (declare (:dynamic-extent sqs seqs vals))
;;   (let ((sl (reduce (lambda (y x) (min y (length x))) seqs :initial-value array-dimension-limit)))
;;     (do* ((x (when rt (make-sequence rt sl)))(xc (consp x))(xp x)(i 0 (1+ i)))
;; 	 ((or (when xc (endp xp)) (>= i sl)) x)
;; 	 (mapl (lambda (x y &aux (z (car x)))
;; 		 (setf (car y) (if (listp z) (pop (car x)) (aref z i)))) seqs vals)
;; 	 (let ((tmp (apply f vals)))
;; 	   (cond (xc (setf (car xp) tmp xp (cdr xp))) (rt (setf (aref x i) tmp)))))))

(defun map-into (rs g &rest seqs &aux 
		    (h rs) (lp (unless (listp rs) (array-total-size rs))) 
		    (fp (when lp (array-has-fill-pointer-p rs)))(j 0))
  (declare (optimize (safety 1))(:dynamic-extent seqs))
  (check-type rs proper-sequence)
  (when fp (setf (fill-pointer rs) lp))
  (block exit
	 (apply 'map nil
		(lambda (x &rest r) 
		  (when (if lp (= j lp) (endp h)) (return-from exit))
		  (let ((tmp (apply g r))) 
		    (if lp (setf (aref rs j) tmp j (1+ j)) (setf (car h) tmp h (cdr h))))) rs seqs))
  (when fp (setf (fill-pointer rs) j))
  rs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_seq.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_lr.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(eval-when
 (compile)

 ;; (dolist (l '(^ \| & ~ >> <<))
 ;;   (unintern l)(import (find-symbol (symbol-name l) 'c) 'si))
 );FIXME

(eval-when
 (compile)

 (defmacro defbltin (n)
  `(progn
     (defun ,n (x)
       (declare (fixnum x))
       (the (integer 0 ,(integer-length most-positive-fixnum))
	    (lit :fixnum ,(strcat "__builtin_" n "(") (:fixnum x) ")")))
     (declaim (inline ,n))))

(defmacro defp nil
  (labels ((lcf (shft &optional res)
		(if (> (abs shft) (integer-length most-positive-fixnum)) 
		    (nreverse res)
		  (lcf (ash shft 1) (cons `(x (+ x (ash x ,shft))) res))))
	   (lc (pat shft)
	       (if (> shft (integer-length most-positive-fixnum)) 
		   pat
		 (lc (logior pat (ash pat shft)) (ash shft 1)))))
	  `(progn
	     (defun popcount (x)
	       (declare (non-negative-fixnum x))
	       (let* ((x (- x (logand (ash x -1) ,(lc 5 4))))
		      (x (+ (logand x ,(lc 3 4)) (logand (ash x -2) ,(lc 3 4))))
		      (x (logand ,(lc 15 8) (+ x (ash x -4))))
		      ,@(lcf -8))
		 (logand x ,(1- (ash (1+ (integer-length most-positive-fixnum)) 1)))))
	     (declaim (inline popcount)))))


 (defmacro defl* ((f d &optional r c c1 c2 (nn f)))
   (let* ((b (symbol-name nn))
	  (fb (intern (concatenate 'string (symbol-name f) "B2")))
	  (ls (intern (concatenate 'string "LOG" b)))
	  (s (cdr (assoc f '((eqv . &)(and . &)(ior . \|)(xor . ^)))))
	  sa;FIXME
;	  (sa (eq s '&))
	  (f `(,s n1 n2))
	  (f (if c `(~ ,f) f))
	  (q `(let* ((n1f (typep n1 'fixnum))(n2f (typep n2 'fixnum))
		     ,@(when sa 
			 `((n1 (if (and n2f (not n1f)) (mpz_get_si n1) n1))
			   (n2 (if (and n1f (not n2f)) (mpz_get_si n2) n2)))))
		(if (,(if sa 'or 'and) n1f n2f) ,f (the integer (,fb n1 n2 ,c)))))
	  (q (if r `(let ((z ,q)) (if r (apply ',ls z (car r) (cdr r)) z)) q)))
     `(defun ,ls ,(if r `(&optional (n1 ,d) (n2 ,d) &rest r) `(n1 n2))
	,@(when r `((declare (:dynamic-extent r))))
	(declare (optimize (safety 1)))
	(check-type n1 integer)
	(check-type n2 integer)
	(let (,@(when c1 `((n1 (lognot n1))))
	      ,@(when c2 `((n2 (lognot n2)))))
	  ,q)))))


(defl* (and -1 t))
(defl* (ior  0 t))
(defl* (xor  0 t))
(defl* (xor -1 t t nil nil eqv))

(defl* (and -1 nil t nil nil nand))
(defl* (ior  0 nil t nil nil nor))

(defl* (and -1 nil nil t nil andc1))
(defl* (and -1 nil nil nil t andc2))

(defl* (ior 0 nil nil t nil orc1))
(defl* (ior 0 nil nil nil t orc2))
(defp)

(defun lognot (x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (if (typep x 'fixnum) (~ x) (mpz_com x)))

(defun boole (op n1 n2)
  (declare (optimize (safety 1)))
  (check-type op (integer 0 15))
  (check-type n1 integer)
  (check-type n2 integer)
  (case op
	(#.boole-and (logand n1 n2))
	(#.boole-ior (logior n1 n2))
	(#.boole-xor (logxor n1 n2))
	(#.boole-eqv (logeqv n1 n2))

	(#.boole-nand  (lognand n1 n2))
	(#.boole-nor   (lognor n1 n2))
	(#.boole-andc1 (logandc1 n1 n2))
	(#.boole-andc2 (logandc2 n1 n2))
	(#.boole-orc1  (logorc1 n1 n2))
	(#.boole-orc2  (logorc2 n1 n2))

	(#.boole-clr 0)
	(#.boole-set -1)
	(#.boole-1 n1)
	(#.boole-2 n2)
	(#.boole-c1 (lognot n1))
	(#.boole-c2 (lognot n2))))

(deftype shft-integer nil `(integer * ,most-positive-fixnum))

(defbltin clzl)
(defbltin ctzl)
;(defbltin popcountl)
(defbltin parityl)
(defbltin ffsl)

(defun ash (x y &aux (lw #.(- fixnum-length)))
  (declare (optimize (safety 1)))
  (check-type x integer)
  (check-type y shft-integer)
  (if (typep y 'fixnum)
      (let ((y y))
	(if (= y 0) x
	  (if (typep x 'fixnum)
	      (let ((x x))
		(if (< y 0)
		    (let ((y (if (= y most-negative-fixnum) y (- y))))
		      (if (/= 0 (logand y lw))
			  (if (< x 0) -1 0)
			(>> x y)))
		  (if (< y (clzl x)) (<< x y) (mpz_mul_2exp x y))))
	    (if (< y 0) (mpz_fdiv_q_2exp x (if (= y most-negative-fixnum) y (- y))) 
	      (mpz_mul_2exp x y)))))
    (if (< x 0) -1 0)))

(defun integer-length (x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (if (typep x 'fixnum)
      (let ((x (if (minusp x) (lognot x) x)))
	(if (= x 0) x
	  (- fixnum-length (clzl x))))
      (mpz_sizeinbase (if (minusp x) (lognot x) x) 2)))
  
(defun logcount (x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (if (typep x 'fixnum)
      (popcount (if (< x 0) (lognot x) x))
    (mpz_popcount  (if (< x 0) (lognot x) x))))
  
(defun logbitp (y x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (check-type y (integer 0))
  (if (typep y 'fixnum)
      (if (typep x 'fixnum)
	  (if (<= y #.(1- (integer-length most-positive-fixnum)))
	      (/= 0 (logand x (ash 1 y)))
	    (< x 0))
	(/= 0 (mpz_tstbit x y)))
    (< x 0)))

(defun immfixp (x)
  (lit :boolean "is_imm_fixnum(" (:object x) ")"))
(putprop 'immfixp t 'compiler::cmp-inline)
;(declaim (inline immfixp))
(setf (get 'immfix 'si::type-predicate) 'immfixp)
(setf (get 'immfixp 'si::predicate-type) 'immfix)

(defun mpz_sgn (x)
  (declare (optimize (safety 1)))
  (check-type x bignum)
  (lit :fixnum "mpz_sgn(&(" (:object x) "->big.big_mpz_t))"))
(putprop 'mpz_sgn t 'compiler::cmp-inline)
;(declaim (inline mpz_sgn))
(defun mpz_odd_p (x)
  (declare (optimize (safety 1)))
  (check-type x bignum)
  (lit :fixnum "mpz_odd_p(&(" (:object x) "->big.big_mpz_t))"))
(putprop 'mpz_odd_p t 'compiler::cmp-inline)
;(declaim (inline mpz_odd_p))
(defun mpz_even_p (x)
  (declare (optimize (safety 1)))
  (check-type x bignum)
  (lit :fixnum "mpz_even_p(&(" (:object x) "->big.big_mpz_t))"))
(putprop 'mpz_even_p t 'compiler::cmp-inline)
;(declaim (inline mpz_even_p))

(defun plusp (x)
  (declare (optimize (safety 1)))
  (check-type x real)
  (typecase x
	    (fixnum (> x 0))
	    (bignum (> (mpz_sgn x) 0))
	    (ratio (plusp (numerator x)))
	    (short-float (> x 0))
	    (long-float (> x 0))))

(defun minusp (x)
  (declare (optimize (safety 1)))
  (check-type x real)
  (typecase x
	    (fixnum (< x 0))
	    (bignum (< (mpz_sgn x) 0))
	    (ratio (minusp (numerator x)))
	    (short-float (< x 0))
	    (long-float (< x 0))))

(defun zerop (x)
  (declare (optimize (safety 1)))
  (check-type x number)
  (typecase x
	    (fixnum (= x 0))
	    (short-float (= x 0))
	    (long-float  (= x 0))
	    (fcomplex (= x 0))
	    (dcomplex (= x 0))))

(defun oddp (x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (typecase x
	    (fixnum (/= 0 (logand 1 x)))
	    (bignum (/= 0 (mpz_odd_p x)))))
		      
(defun evenp (x)
  (declare (optimize (safety 1)))
  (check-type x integer)
  (typecase x
	    (fixnum (= 0 (logand 1 x)))
	    (bignum (/= 0 (mpz_even_p x)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_lr.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_littleXlsp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;;This file is included as a demonstration of how to link in low level
;;C code.   It is also useful! [comments by wfs]
;;Author: Mark Ring  (ring@cs.utexas.edu)

;; In the next comment we explain how to link in the file
;; and then a sample usage.

#|
If you have si::faslink you may use: 
(si::faslink "/public/gcl/lsp/littleXlsp.o" "/public/gcl/o/littleXwin.o -lX11 -lc")

To avoid using faslink which is much less portable, 
when building the gcl image you may add
EXTRAS=${ODIR}/littleXwin.o
LIBS= -lX -lm -lg
to the h/machine.defs, redo the add-defs machine, and remake so that
the low level X code will be linked in.
Then you may simply
(load "/public/gcl/lsp/littleXlsp.o")

;;Now you may try the following examples: 



(setq W1 (open-window))
(setq W2 (open-window))
(resize-window w1 200 150)
(resize-window w2 240 225)

(set-background w1 "red")
(clear-window w1)
(set-foreground "blue")

(draw-line w1 5 5 100 5)
(draw-line w1 100 100 100 5)
(draw-line w1 100 100 5 100)
(dotimes (i 20)
  (draw-line w1 5 (* i 5) (* i 5) (* i 5)))
(dotimes (i 20)
  (erase-line w1 (+ 7 (* i 5)) 10 (+ 7 (* i 5)) 95))
(use-font "fixed")
(draw-text w1 "A Design" 10 112)
(clear-text w1 "De" 22 112)

(dotimes (i 25)
  (set-background w2 (format nil "#~2,'0X~2,'0X~2,'0X" (* i 10) (* i 10) (* i 10)))
  (clear-window w2))

(set-foreground "black")
(dotimes (i 20)
  (draw-arc w2 5 100 100 (- 100 (* i 5)) 0 (* 360 64)))
(dotimes (i 20)
  (draw-arc w2 5 (- 100 (* i 5)) 100 (* i 5) 0 (* 360 64)))
(set-arc-mode 'pie)
(dotimes (i 10)
  (fill-arc w2 115 5 100 100 (* i 64 36) (* 64 18)))
(set-arc-mode 'chord)
(dotimes (i 10)
  (fill-arc w2 115 105 100 100 (* i 64 36) (* 64 36 3)))
(dotimes (i 5)
  (clear-arc w2 115 105 100 100 (* i 64 36 2) (* 64 36 2)))
(use-font "-b&h-lucidabright-demibold-i-normal--18-180-75-75-p-107-iso8859-1")
(draw-text w2 "A Bunch of Wierd Things" 2 220)

(clear-window w1)

(close-window w1)
(close-window w2)
|#





;;; Open a window.  Return window ID as an Integer
(defentry open-window () (int open_window))

;;; Close given window.
;;;   Parameter 1:  Window ID.
(defentry close-window (int) (int close_window))

;;; Clear a window of its contents.
;;;   Parameter 1:  Window ID.
(defentry clear-window (int) (int clear_window))

;;; Draw a line in a window.
;;;   Parameter 1:  Window ID.
;;;   Parameter 2:  left-most x coordinate
;;;   Parameter 3:  top-most y coordinate
;;;   Parameter 4:  right-most x coordinate
;;;   Parameter 5:  bottom-most y coordinate
(defentry draw-line (int int int int int) (int draw_line))

;;; Erase a line from a window.
;;;   Parameter 1:  Window ID.
;;;   Parameter 2:  left-most x coordinate
;;;   Parameter 3:  top-most y coordinate
;;;   Parameter 4:  right-most x coordinate
;;;   Parameter 5:  bottom-most y coordinate
(defentry erase-line (int int int int int) (int erase_line))

;;; Draw an arc in a window. (See X Documentation).
;;;   Parameter 1:  Window ID.
;;;   Parameter 2:  left-most x coordinate
;;;   Parameter 3:  top-most y coordinate
;;;   Parameter 4:  width of square
;;;   Parameter 5:  height of square
;;;   Parameter 6:  starting position: angle 1 (from 3:00)
;;;   Parameter 7:  ending position: angle 2 (from angle 1)
;;;        Angles are specified in 64ths of a degree and go counter-clockwise
(defentry draw-arc (int int int int int int int) (int draw_arc))

;;; Clear an arc from a window. (See X Documentation).
;;;   Parameter 1:  Window ID.
;;;   Parameter 2:  left-most x coordinate
;;;   Parameter 3:  top-most y coordinate
;;;   Parameter 4:  width of square
;;;   Parameter 5:  height of square
;;;   Parameter 6:  starting position: angle 1 (from 3:00)
;;;   Parameter 7:  ending position: angle 2 (from angle 1)
;;;        Angles are specified in 64ths of a degree and go counter-clockwise
(defentry clear-arc (int int int int int int int) (int clear_arc))

;;; Draw a filled arc in a window. (See X Documentation).
;;;   Parameter 1:  Window ID.
;;;   Parameter 2:  left-most x coordinate
;;;   Parameter 3:  top-most y coordinate
;;;   Parameter 4:  width of square
;;;   Parameter 5:  height of square
;;;   Parameter 6:  starting position: angle 1 (from 3:00)
;;;   Parameter 7:  ending position: angle 2 (from angle 1)
;;;        Angles are specified in 64ths of a degree and go counter-clockwise
(defentry fill-arc  (int int int int int int int) (int fill_arc))

;;; Resize a window.
;;;   Parameter 1:  Window ID.
;;;   Parameter 2:  new width
;;;   Parameter 3:  new height
(defentry resize-window (int int int) (int resize_window))

;;; Raise a window to the front.
;;;   Parameter 1:  Window ID.
(defentry raise-window (int) (int raise_window))

;;; Draw Text in a window.
;;;   Parameter 1:  Window ID.
;;;   Parameter 2:  text string
;;;   Parameter 3:  left-most x coordinate
;;;   Parameter 4:  top-most y coordinate 
(defentry draw-text-2 (int object int int) (int draw_text))
(defun    draw-text (window string x y)
  (draw-text-2 window (get-c-string string) x y))

;;; Clear text from a window
;;;   Parameter 1:  Window ID
;;;   Parameter 2:  text string
;;;   Parameter 3:  left-most x coordinate
;;;   Parameter 4:  top-most y coordinate 
(defentry clear-text-2 (int object int int) (int erase_text))
(defun    clear-text (window string x y)
  (clear-text-2 window (get-c-string string) x y))

;;; Set arc-mode to be Pie or Chord
;;;   Parameter 1:  'PIE or 'CHORD
(defentry set-arc-mode-2 (int) (int set_arc_mode))
(defun    set-arc-mode (pie-or-chord)
  (if (eq pie-or-chord 'pie)
      (set-arc-mode-2 1)
      (set-arc-mode-2 0)))

;;; Use a particular font in a given window
;;;   Parameter 1:  font name
(defentry use-font-2 (object) (int use_font))
(defun    use-font (string)
  (use-font-2 (get-c-string string)))

;;; Set background color of window
;;;   Parameter 1:  Window ID
;;;   Parameter 2:  color name (string)
(defentry set-background-2 (int object) (int set_background))
(defun    set-background   (window string)
  (set-background-2 window (get-c-string string)))

;;; Set foreground color 
;;;   Parameter 1:  color name (string)
(defentry set-foreground-2 (object) (int set_foreground))
(defun    set-foreground   (string)
  (set-foreground-2 (get-c-string string)))

;;;----------------------------------------------------------------------
;;; General routines.
(defCfun "object get_c_string(s) object s;" 0
  " return(s->st.st_self);"
  )
(defentry get_c_string_2 (object) (object get_c_string))

/* make sure string is null terminated */
(defun get-c-string (string)
  (get_c_string_2 (concatenate 'string string " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_littleXlsp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_mnum.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package :si)

#+c99
(progn

(eval-when
    (compile eval)
  (defmacro deflibmfun (x)
    `(progn
       (defdlfun (:float    ,(strcat x "f")     ) :float)
       (defdlfun (:double   ,x                  ) :double)
       (defdlfun (:fcomplex ,(strcat "c" x "f") ) :fcomplex)
       (defdlfun (:dcomplex ,(strcat "c" x)     ) :dcomplex)))
  
  (defmacro defrlibmfun (x)
    `(progn
       (defdlfun (:float    ,(strcat x "f")     ) :float :float)
       (defdlfun (:double   ,x                  ) :double :double)))

  (defmacro defalibmfun (x)
    `(progn
       (defdlfun (:float    ,(strcat "f" x "f") ) :float)
       (defdlfun (:double   ,(strcat "f" x)     ) :double)
       (defdlfun (:fixnum   ,(strcat "l" x)     ) :fixnum)
       (defdlfun (:float    ,(strcat "c" x "f") ) :fcomplex)
       (defdlfun (:double   ,(strcat "c" x)     ) :dcomplex))))

(defalibmfun "abs")

(deflibmfun "exp")
(deflibmfun "log")
(deflibmfun "sqrt")

(deflibmfun "sin")
(deflibmfun "cos")
(deflibmfun "tan")
(deflibmfun "sinh")
(deflibmfun "cosh")
(deflibmfun "tanh")
(deflibmfun "asin")
(deflibmfun "acos")
(deflibmfun "atan")
(defrlibmfun "atan2")
(deflibmfun "asinh")
(deflibmfun "acosh")
(deflibmfun "atanh")


(eval-when 
 (compile eval)
 

 (defmacro defmfun (x &optional n protect-real)
   (let* ((b (mdlsym x))
	  (f (mdlsym (string-concatenate x "f")))
	  (c (mdlsym (string-concatenate "c" x)))
	  (cf (mdlsym (string-concatenate "c" x "f")))
	  (ts (intern (string-upcase x)))
	  (tp (get ts 'compiler::type-propagator))
	  (body `(typecase x
		   (long-float  (,b x))
		   (short-float (,f x))
;                  (fixnum      (,b (float x 0.0)))
		   (rational    (,b (float x 0.0)))
		   (dcomplex    (,c x))
		   (fcomplex    (,cf x))
		   (otherwise   (,c (complex (float (realpart x) 0.0) (float (imagpart x) 0.0)))))))
     `(progn
	(mdlsym ,x)
	(mdlsym (string-concatenate ,x "f"))
	(mdlsym (string-concatenate "c" ,x))
	(mdlsym (string-concatenate "c" ,x "f"))
	(setf (get ',b 'compiler::type-propagator)  ',tp)
	(setf (get ',f 'compiler::type-propagator)  ',tp)
	(setf (get ',c 'compiler::type-propagator)  ',tp)
	(setf (get ',cf 'compiler::type-propagator) ',tp)
	(defun ,(or n (intern (string-upcase x))) (x)
	  ,@(unless (and n (not (string= (string-upcase n) (string-upcase x))))
	      `((declare (optimize (safety 2)))
		(check-type x number)))
	  ,(if protect-real
	       `(if (and (realp x) ,protect-real)
		    ,body
		  (let ((x (cond ((not (realp x)) x) 
				 ((floatp x) (complex x (float 0.0 x)))
				 ((complex (float x 0.0) 0.0)))))
		    ,body))
	     body)))))

 (defmacro defmabs (x &optional n)
   (let* ((i 'babs);(mdlsym (string-concatenate "l" x)))
	  (b (mdlsym (string-concatenate "f" x)))
	  (f (mdlsym (string-concatenate "f" x "f")))
	  (c (mdlsym (string-concatenate "c" x)))
	  (cf (mdlsym (string-concatenate "c" x "f")))
	  (ts (intern (string-upcase x)))
	  (tp (get ts 'compiler::type-propagator)))
     `(progn
	(mdlsym ,x)
	(mdlsym (string-concatenate "f" ,x))
	(mdlsym (string-concatenate "c" ,x))
	(setf (get ',i 'compiler::type-propagator)  ',tp)
	(setf (get ',b 'compiler::type-propagator)  ',tp)
	(setf (get ',f 'compiler::type-propagator)  ',tp)
	(setf (get ',c 'compiler::type-propagator)  ',tp)
	(setf (get ',cf 'compiler::type-propagator)  ',tp)
	(defun ,(or n (intern (string-upcase x))) (x)
	  ,@(unless n `((declare (optimize (safety 2)))
			(check-type x number)))
	  (typecase x
			 (long-float  (,b x))
			 (short-float (,f x))
			 (fixnum      (if (= x most-negative-fixnum) (- most-negative-fixnum) (,i x)))
			 (rational    (if (minusp x) (- x) x))
			 (dcomplex    (,c x))
			 (fcomplex    (,cf x))
			 (otherwise   (,c (complex (float (realpart x) 0.0) (float (imagpart x) 0.0)))))))))

 (defmacro defrmfun (x &optional n)
   (let ((b (mdlsym x))
	 (f (mdlsym (string-concatenate x "f"))))
     `(progn
	(mdlsym ,x)
	(mdlsym (string-concatenate ,x "f"))
	(defun ,(or n (intern (string-upcase x))) (x z)
	  ,(unless n `((declare (optimize (safety 2)))
		       (check-type x real)
		       (check-type z real)))
	  (typecase 
	   z
	   (long-float (typecase 
			x
			(long-float  (,b x z))
			(short-float (,b (float x z) z))
			(fixnum      (,b (float x z) z))
			(rational    (,b (float x z) z))))
	   (short-float (typecase 
			 x
			 (long-float  (,b x (float z x)))
			 (short-float (,f x z))
			 (fixnum      (,f (float x z) z))
			 (rational    (,f (float x z) z))))
	   (fixnum (typecase 
		    x
		    (long-float  (,b x (float z x)))
		    (short-float (,f x (float z x)))
		    (fixnum      (,b (float x 0.0) (float z 0.0)))
		    (rational    (,b (float x 0.0) (float z 0.0)))))
	   (rational (typecase 
		      x
		      (long-float  (,b x (float z x)))
		      (short-float (,f x (float z x)))
		      (fixnum      (,b (float x 0.0) (float z 0.0)))
		      (rational    (,b (float x 0.0) (float z 0.0)))))))))))


(defun babs (x) (declare (fixnum x)) (lit :fixnum "labs(" (:fixnum x) ")"));this is a builtin in recent gcc
(setf (get 'babs 'compiler::cmp-inline) t)
 
(defmabs "abs")

(defmfun "sin")	
(defmfun "cos")	
(defmfun "tan")
(defmfun "asinh")
(defmfun "sinh")
(defmfun "cosh")
(defmfun "tanh")

(defmfun "exp" rawexp)
(defun exp (x)
  (declare (inline rawexp))
  (check-type x number)
  (rawexp x))

(defrmfun "atan2"  rawatan2)
(defmfun "atan" rawatan)
(defun atan (x &optional (z 0.0 zp))
  (declare (optimize (safety 2)) (inline rawatan2 rawatan))
  (check-type x number)
  (check-type z real)
  (cond (zp 
	 (check-type x real)
	 (rawatan2 x z))
	((rawatan x))))

(defmfun "log" rawlog (>= x 0))
(defun log (x &optional b)
  (declare (optimize (safety 2)) (inline rawlog))
  (check-type x number)
  (check-type b (or null number))
  (if b 
      (/ (log x) (log b))
    (rawlog x)))
  
(defmfun "acosh" acosh (>= x 1))
(defmfun "atanh" atanh (and (>= x -1) (<= x 1)))
(defmfun "acos"  acos (and (>= x -1) (<= x 1)))
(defmfun "asin"  asin (and (>= x -1) (<= x 1)))
(defmfun "sqrt"  sqrt (>= x 0))
)

#-c99
(defun abs (z)
  (declare (optimize (safety 2)))
  (check-type z number)
  (if (complexp z)
      ;; Compute (sqrt (+ (* x x) (* y y))) carefully to prevent
      ;; overflow!
      (let* ((x (abs (realpart z)))
	     (y (abs (imagpart z))))
	(if (< x y)
	    (rotatef x y))
	(if (zerop x)
	    x
	  (let ((r (/  y x)))
	    (* x (sqrt (+ 1 (* r r))))))))
  (if (minusp z) (- z) z))


;; (defdlfun (:fixnum "__gmpz_cmp") :fixnum :fixnum)
;; #.(let ((x (truncate fixnum-length char-length)))
;;     `(defun mpz_cmp (x y) (|libgmp|:|__gmpz_cmp| (+ ,x (address x)) (+ ,x (address y)))));FIXME
;; (setf (get 'mpz_cmp 'compiler::cmp-inline) t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_mnum.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_auto.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package :si)
;;; Autoloaders.


;;; DEFAUTOLOAD definitions. for lsp directory files normally loaded.
(if (fboundp 'abs) (push :numlib *features*))
;;hack to avoid interning all the :symbols if the files are loaded..
#-numlib
(progn
(autoload 'abs '|numlib|)
(autoload 'acos '|numlib|)
(autoload 'acosh '|numlib|)
(autoload 'adjust-array '|arraylib|)
(autoload 'apropos '|packlib|)
(autoload 'apropos-list '|packlib|)
(autoload 'array-dimensions '|arraylib|)
(autoload 'array-in-bounds-p '|arraylib|)
(autoload 'array-row-major-index '|arraylib|)
(autoload 'asin '|numlib|)
(autoload 'asinh  '|numlib|)
(autoload 'atanh '|numlib|)
;(autoload 'best-array-element-type '|arraylib|)
(autoload 'bit '|arraylib|)
(autoload 'bit-and '|arraylib|)
(autoload 'bit-andc1 '|arraylib|)
(autoload 'bit-andc2 '|arraylib|)
(autoload 'bit-eqv '|arraylib|)
(autoload 'bit-ior '|arraylib|)
(autoload 'bit-nand '|arraylib|)
(autoload 'bit-nor '|arraylib|)
(autoload 'bit-not '|arraylib|)
(autoload 'bit-orc1 '|arraylib|)
(autoload 'bit-orc2 '|arraylib|)
(autoload 'bit-xor '|arraylib|)
(autoload 'byte '|numlib|)
(autoload 'byte-position '|numlib|)
(autoload 'byte-size '|numlib|)
(autoload 'cis '|numlib|)
(autoload 'coerce '|predlib|)
(autoload 'compile-file '|loadcmp|)
(autoload 'compile '|loadcmp|)
(autoload 'disassemble '|loadcmp|)
(autoload 'concatenate '|seq|)
(autoload 'cosh '|numlib|)
(autoload 'count '|seqlib|)
(autoload 'count-if '|seqlib|)
(autoload 'count-if-not '|seqlib|)
(autoload 'decode-universal-time '|mislib|)
(autoload 'delete '|seqlib|)
(autoload 'delete-duplicates '|seqlib|)
(autoload 'delete-if '|seqlib|)
(autoload 'delete-if-not  '|seqlib|)
(autoload 'deposit-field '|numlib|)
(autoload 'describe '|describe|)
(autoload 'dpb '|numlib|)
(autoload 'dribble '|iolib|)
(autoload 'encode-universal-time '|mislib|)
(autoload 'every '|seq|)
(autoload 'fceiling '|numlib|)
(autoload 'ffloor '|numlib|)
(autoload 'fill '|seqlib|)
(autoload 'find '|seqlib|)
(autoload 'find-all-symbols '|packlib|)
(autoload 'find-if '|seqlib|)
(autoload 'find-if-not '|seqlib|)
(autoload 'fround '|numlib|)
(autoload 'ftruncate '|numlib|)
#-unix (autoload 'get-decoded-time '|mislib|)
#+aosvs (autoload 'get-universal-time '|mislib|)
(autoload 'get-setf-method '|setf|)
(autoload 'get-setf-method-multiple-value '|setf|)
(autoload 'inspect '|describe|)
(autoload 'intersection '|listlib|)
(autoload 'isqrt '|numlib|)
(autoload 'ldb '|numlib|)
(autoload 'ldb-test '|numlib|)
(autoload 'logandc1 '|numlib|)
(autoload 'logandc2 '|numlib|)
(autoload 'lognand '|numlib|)
(autoload 'lognor '|numlib|)
(autoload 'lognot '|numlib|)
(autoload 'logorc1 '|numlib|)
(autoload 'logorc2 '|numlib|)
(autoload 'logtest '|numlib|)
(autoload 'make-array '|arraylib|)
(autoload 'make-sequence '|seq|)
(autoload 'map '|seq|)
(autoload 'mask-field '|numlib|)
(autoload 'merge '|seqlib|)
(autoload 'mismatch '|seqlib|)
(autoload 'nintersection '|listlib|)
(autoload 'notany '|seq|)
(autoload 'notevery '|seq|)
(autoload 'si::normalize-type ':predlib)
(autoload 'nset-difference '|listlib|)
(autoload 'nset-exclusive-or '|listlib|)
(autoload 'nsubstitute '|seqlib|)
(autoload 'nsubstitute-if '|seqlib|)
(autoload 'nsubstitute-if-not '|seqlib|)
(autoload 'nunion '|listlib|)
(autoload 'phase '|numlib|)
(autoload 'position '|seqlib|)
(autoload 'position-if '|seqlib|)
(autoload 'position-if-not '|seqlib|)
(autoload 'prin1-to-string '|iolib|)
(autoload 'princ-to-string '|iolib|)
(autoload 'rational '|numlib|)
(autoload 'rationalize '|numlib|)
(autoload 'read-from-string '|iolib|)
(autoload 'reduce '|seqlib|)
(autoload 'remove '|seqlib|)
(autoload 'remove-duplicates '|seqlib|)
(autoload 'remove-if '|seqlib|)
(autoload 'remove-if-not '|seqlib|)
(autoload 'replace '|seqlib|)
(autoload 'sbit '|arraylib|)
(autoload 'search '|seqlib|)
(autoload 'set-difference '|listlib|)
(autoload 'set-exclusive-or '|listlib|)
(autoload 'signum '|numlib|)
(autoload 'sinh '|numlib|)
(autoload 'some '|seq|)
(autoload 'sort '|seqlib|)
(autoload 'stable-sort '|seqlib|)
(autoload 'subsetp '|listlib|)
(autoload 'substitute '|seqlib|)
(autoload 'substitute-if '|seqlib|)
(autoload 'substitute-if-not '|seqlib|)
(autoload 'subtypep '|predlib|)
(autoload 'tanh '|numlib|)
(autoload 'typep '|predlib|)
(autoload 'union '|listlib|)
(autoload 'vector '|arraylib|)
(autoload 'vector-pop '|arraylib|)
(autoload 'vector-push '|arraylib|)
(autoload 'vector-extend '|arraylib|)
(autoload 'write-to-string '|iolib|)
(autoload 'y-or-n-p '|iolib|)
(autoload 'yes-or-no-p '|iolib|)


(set-dispatch-macro-character #\# #\a 'si::sharp-a-reader)
(set-dispatch-macro-character #\# #\A 'si::sharp-a-reader)
(autoload 'si::sharp-a-reader '"iolib")
(set-dispatch-macro-character #\# #\s 'si::sharp-s-reader)
(set-dispatch-macro-character #\# #\S 'si::sharp-s-reader)
(autoload 'si::sharp-s-reader '|iolib|)


;;; DEFAUTOLOADMACRO definitions.

(autoload-macro 'assert '|assert|)
(autoload-macro 'ccase '|assert|)
(autoload-macro 'check-type '|assert|)
(autoload-macro 'ctypecase '|assert|)
(autoload-macro 'decf '|setf|)
(autoload-macro 'define-modify-macro '|setf|)
(autoload-macro 'define-setf-method '|setf|)
(autoload-macro 'defsetf '|setf|)
(autoload-macro 'defstruct '|defstruct|)
(autoload-macro 'si::define-structure '|defstruct|)
(autoload-macro 'deftype '|predlib|)
(autoload-macro 'do-all-symbols '|packlib|)
(autoload-macro 'do-external-symbols '|packlib|)
(autoload-macro 'do-symbols '|packlib|)
(autoload-macro 'ecase '|assert|)
(autoload-macro 'etypecase '|assert|)
(autoload-macro 'incf '|setf|)
(autoload-macro 'pop '|setf|)
(autoload-macro 'push '|setf|)
(autoload-macro 'pushnew '|setf|)
(autoload-macro 'remf '|setf|)
(autoload-macro 'rotatef '|setf|)
(autoload-macro 'setf '|setf|)
(autoload-macro 'shiftf '|setf|)
(autoload-macro 'step '|trace|)
(autoload-macro 'time '|mislib|)
(autoload-macro 'trace '|trace|)
(autoload-macro 'typecase '|assert|)
(autoload-macro 'untrace '|trace|)
(autoload-macro 'with-input-from-string '|iolib|)
(autoload-macro 'with-open-file '|iolib|)
(autoload-macro 'with-open-stream '|iolib|)
(autoload-macro 'with-output-to-string '|iolib|)
)   ;;end autoloads of normally loaded files.j
(if (find-package "COMPILER") (push :compiler *features*))
#+compiler
(autoload 'compiler::emit-fn '|../cmpnew/gcl_collectfn|)
(autoload 'compiler::init-fn '|../cmpnew/gcl_collectfn|)
(autoload 'si::monstartup '"gprof")
(autoload  'si::set-up-profile '"profile")

(AUTOLOAD 'IDESCRIBE '|info|)
(AUTOLOAD 'INFO '|info|)
(AUTOLOAD 'LIST-MATCHES '|info|)
(AUTOLOAD 'get-match '|info|)
(AUTOLOAD 'print-node '|tinfo|)
(AUTOLOAD 'offer-choices '|tinfo|)
(AUTOLOAD 'tkconnect '|tkl|)




;; the sun has a broken ypbind business, if one wants to save.
;; So to stop users from invoking this
#+sun
(defun user-homedir-pathname ()
 (let* ((tem (si::getenv "HOME"))
	(l (- (length tem) 1)))
   (cond ((null tem) nil)
	 (t 
	  (or (and (>= l 0)
		   (eql (aref tem l) #\/))
	      (setq tem (concatenate 'string tem "/")))
	  (pathname tem)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_auto.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_restart.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: "CONDITIONS"; Base: 10 -*-

(in-package :si)

(defvar *restarts* nil)
(defvar *restart-condition* nil)

(defmacro restart-bind (bindings &body forms)
  (declare (optimize (safety 2)))
  `(let ((*restarts* 
	  (list* ,@(mapcar (lambda (x) `(cons (make-restart :name ',(pop x) :function ,(pop x) ,@x) *restart-condition*)) bindings)
		 *restarts*)))
	 ,@forms))


(defmacro with-condition-restarts (condition-form restarts-form &body body)
  (declare (optimize (safety 1)))
  (let ((n-cond (gensym)))
    `(let* ((,n-cond ,condition-form)
	    (*restarts* (nconc (mapcar (lambda (x) (cons x ,n-cond)) ,restarts-form) *restarts*)))
       ,@body)))

(defun condition-pass (condition restart &aux b (f (restart-test-function restart)))
  (when (if f (funcall f condition) t)
    (mapc (lambda (x) 
	    (when (eq (pop x) restart)
	      (if (if condition (eq x condition) t)
		  (return-from condition-pass t)
		(setq b (or b x))))) *restarts*)
    (not b)))

(defvar *kcl-top-restarts* nil)

(defun make-kcl-top-restart (quit-tag)
  (make-restart :name 'gcl-top-restart
		:function (lambda () (throw (car (list quit-tag)) quit-tag))
		:report-function 
		(lambda (stream) 
		    (let ((b-l (if (eq quit-tag si::*quit-tag*)
				   si::*break-level*
				   (car (or (find quit-tag si::*quit-tags*
						  :key #'cdr)
					    '(:not-found))))))
		      (cond ((eq b-l :not-found)
			     (format stream "Return to ? level."))
			    ((null b-l)
			     (format stream "Return to top level."))
			    (t
			     (format stream "Return to break level ~D."
				     (length b-l))))))))

(defun find-kcl-top-restart (quit-tag)
  (cdr (or (assoc quit-tag *kcl-top-restarts*)
	   (car (push (cons quit-tag (make-kcl-top-restart quit-tag))
		      *kcl-top-restarts*)))))

(defun kcl-top-restarts ()
  (let* (;(old-tags (ldiff si::*quit-tags* (member nil si::*quit-tags* :key 'cdr)))
	 (old-tags si::*quit-tags*)
	 (old-tags (mapcan (lambda (e) (when (cdr e) (list (cdr e)))) old-tags))
	 (tags (if si::*quit-tag* (cons si::*quit-tag* old-tags) old-tags))
	 (restarts (mapcar 'find-kcl-top-restart tags)))
    (setq *kcl-top-restarts* (mapcar 'cons tags restarts))
    restarts))

(defun compute-restarts (&optional condition)
  (remove-if-not (lambda (x) (condition-pass condition x)) (remove-duplicates (nconc (mapcar 'car *restarts*) (kcl-top-restarts)))))

(defun find-restart (name &optional condition &aux (sn (symbolp name)))
  (car (member name (compute-restarts condition) :key (lambda (x) (if sn (restart-name x) x)))))

(defun transform-keywords (&key report interactive test 
				&aux rr (report (if (stringp report) `(lambda (s) (write-string ,report s)) report)))
  (macrolet ((do-setf (x) 
		      `(when ,x 
			 (setf (getf rr ,(intern (concatenate 'string (symbol-name x) "-FUNCTION") :keyword))
			       (list 'function ,x)))))
	    (do-setf report)
	    (do-setf interactive)
	    (do-setf test)
	    rr))

(defun rewrite-restart-case-clause (r &aux (name (pop r))(ll (pop r)))
  (labels ((l (r) (if (member (car r) '(:report :interactive :test)) (l (cddr r)) r)))
	  (let ((rd (l r)))
	    (list* name (gensym) (apply 'transform-keywords (ldiff r rd)) ll rd))))


(defun restart-case-expression-condition (expression env c &aux (e (macroexpand expression env))(n (when (listp e) (pop e))))
  (case n
	(cerror (let ((ca (pop e))) `((process-error ,(pop e) (list ,@e)) (,n ,ca ,c))))
	(error `((process-error ,(pop e) (list ,@e)) (,n ,c)))
	(warn `((process-error ,(pop e) (list ,@e) 'simple-warning) (,n ,c)))
	(signal `((coerce-to-condition ,(pop e) (list ,@e) 'simple-condition ',n) (,n ,c)))))


(defmacro restart-case (expression &body clauses &environment env)
  (declare (optimize (safety 2)))
  (let* ((block-tag (gensym))(args (gensym))(c (gensym))
	 (data (mapcar 'rewrite-restart-case-clause clauses))
	 (e (restart-case-expression-condition expression env c)))
    `(block 
      ,block-tag
      (let* (,args (,c ,(car e)) (*restart-condition* ,c))
	(tagbody
	 (restart-bind
	  ,(mapcar (lambda (x) `(,(pop x) (lambda (&rest r) (setq ,args r) (go ,(pop x))) ,@(pop x))) data)
	  (return-from ,block-tag ,(or (cadr e) expression)))
	 ,@(mapcan (lambda (x &aux (x (cdr x)))
		     `(,(pop x) (return-from ,block-tag (apply (lambda ,(progn (pop x)(pop x)) ,@x) ,args)))) data))))))


(defvar *unique-id-table* (make-hash-table))
(defvar *unique-id-count* -1)

(defun unique-id (obj)
  "generates a unique integer id for its argument."
  (or (gethash obj *unique-id-table*)
      (setf (gethash obj *unique-id-table*) (incf *unique-id-count*))))

(defun restart-print (restart stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<~s.~d>" (type-of restart) (unique-id restart))
      (restart-report restart stream)))

(defstruct (restart (:print-function restart-print))
  name
  function
  report-function
  interactive-function
  (test-function (lambda (c) (declare (ignore c)) t)))

(defun restart-report (restart stream &aux (f (restart-report-function restart)))
  (if f (funcall f stream)
    (format stream "~s" (or (restart-name restart) restart))))

(defun invoke-restart (restart &rest values)
  (let ((real-restart (or (find-restart restart)
			  (error 'control-error :format-control "restart ~s is not active." :format-arguments (list restart)))))
       (apply (restart-function real-restart) values)))

(defun invoke-restart-interactively (restart)
  (let ((real-restart (or (find-restart restart)
			  (error "restart ~s is not active." restart))))
    (apply (restart-function real-restart)
	   (let ((interactive-function (restart-interactive-function real-restart)))
	     (when interactive-function
		 (funcall interactive-function))))))


(defmacro with-simple-restart ((restart-name format-control &rest format-arguments)
			       &body forms)
  (declare (optimize (safety 1)))
  `(restart-case (progn ,@forms)
     (,restart-name nil
        :report (lambda (stream) (format stream ,format-control ,@format-arguments))
	(values nil t))))

(defun abort (&optional condition)
  "Transfers control to a restart named abort, signalling a control-error if
   none exists."
  (invoke-restart (find-restart 'abort condition))
  (error 'abort-failure))


(defun muffle-warning (&optional condition)
  "Transfers control to a restart named muffle-warning, signalling a
   control-error if none exists."
  (invoke-restart (find-restart 'muffle-warning condition)))

(macrolet ((define-nil-returning-restart (name args doc)
	     (let ((restart (gensym)))
	       `(defun ,name (,@args &optional condition)
		  ,doc
		  (declare (optimize (safety 1)))
		  (let ((,restart (find-restart ',name condition))) (when ,restart (invoke-restart ,restart ,@args)))))))

  (define-nil-returning-restart continue nil
    "Transfer control to a restart named continue, returning nil if none exists.")
  
  (define-nil-returning-restart store-value (value)
    "Transfer control and value to a restart named store-value, returning nil if
   none exists.")
  
  (define-nil-returning-restart use-value (value)
    "Transfer control and value to a restart named use-value, returning nil if
   none exists."))

(defun show-restarts (&aux (i 0))
  (mapc (lambda (x)
	  (format t "~& ~4d ~a ~a ~%" 
		  (incf i)
		  (cond ((eq x *debug-abort*) "(abort)") ((eq x *debug-continue*) "(continue)") (""))
		  x)) *debug-restarts*)
  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_restart.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_make_pathname.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun pathnamep (x)
  (declare (optimize (safety 1)))
  (when (typep x 'pathname) t))

(defun regexp-conv (stream)

  (let ((tem (make-array 10 :element-type 'character :fill-pointer 0)))
    (or (eql (read-char stream) #\")
	(error "sharp-u-reader reader needs a \" right after it"))
    (loop
     (let ((ch (read-char stream)))
       (cond ((eql ch #\") (return tem))
	     ((eql ch #\\)
	      (setq ch (read-char stream))
	      (setq ch (or (cdr (assoc ch '((#\n . #\newline)
					    (#\t . #\tab)
					    (#\r . #\return))))
			   ch))))
       (vector-push-extend ch tem)))
    tem))
  
(defun sharp-u-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (regexp-conv stream))

(defun sharp-v-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(load-time-value (compile-regexp ,(regexp-conv stream))))

(set-dispatch-macro-character #\# #\u 'sharp-u-reader)
(set-dispatch-macro-character #\# #\v 'sharp-v-reader)



(defun msub (a x) (if a (msub (cdr a) (substitute (caar a) (cdar a) x)) x))

(defvar *glob-to-regexp-alist* (list (cons #v"{[^}]*}" (lambda (x) (msub '((#\| . #\,)(#\( . #\{)(#\) . #\})) x)))
				     (cons #v"\\[[^\\]*\\]" (lambda (x)
							      (concatenate 'string "("
									   (substitute #\^ #\! (subseq x 0 2))
									   (subseq x 2) ")")))
				     (cons #v"\\*" (lambda (x) "([^/.]*)"))
				     (cons #v"\\?" (lambda (x) "([^/.])"))
				     (cons #v"\\." (lambda (x) "\\."))))

(defun mglist (x &optional (b 0))
  (let* ((y (mapcan (lambda (z &aux (w (string-match (car z) x b)))
		      (unless (eql w -1)
			(list (list w (match-end 0) z))))
		    *glob-to-regexp-alist*))
	 (z (when y (reduce (lambda (y x) (if (< (car x) (car y)) x y)) y))))
    (when z
      (cons z (mglist x (cadr z))))))

(defun mgsub (x &optional (l (mglist x)) (b 0) &aux (w (pop l)))
  (if w
      (concatenate 'string
		   (subseq x b (car w))
		   (funcall (cdaddr w) (subseq x (car w) (cadr w)))
		   (mgsub x l (cadr w)))
    (subseq x b)))


(defun elsub (el x rp lp &aux (y x) (pref (pop y))(dflt (pop y))(post (pop y)))
;  (destructuring-bind (pref dflt post &rest y) x
    (etypecase el
      (string (let ((x (list pref el post))) (unless (zerop (length dflt)) (if rp (mapcar 'mgsub x) x))))
      (integer (elsub (write-to-string el) x rp lp))
      ((eql :wild-inferiors) (if rp (list "(" dflt "*)") (elsub "**" x rp lp)))
      ((eql :wild) (if rp (list dflt) (elsub "*" x rp lp)))
      ((eql :newest) (elsub (if rp "(newest|NEWEST)" "NEWEST") x rp lp))
      ((member :up :back) (elsub ".." x rp lp))
      ((member nil :unspecific) (when rp (list dflt)))
      (cons (cons
	     (if (eq (car el) :absolute) (if lp "" "/") (if lp ";" ""))
	     (mapcan (lambda (z) (elsub z y rp lp)) (cdr el)))))
;    )
)

(defconstant +physical-pathname-defaults+ '(("" "" "")
					    ("" "" "")
					    ("" "(/?([^/]+/)*)" "" "" "([^/]+/)" "/")
					    ("" "([^/.]*)" "")
					    ("." "(\\.[^/]*)?" "")
					    ("" "" "")))
(defconstant +logical-pathname-defaults+  '(("" "([-0-9A-Z]+:)?" ":")
					    ("" "" "")
					    ("" "(;?((\\*?([-0-9A-Z]+\\*?)+|\\*|\\*\\*);)*)" "" "" "((\\*?([-0-9A-Z]+\\*?)+|\\*);)" ";")
					    ("" "(\\*?([-0-9A-Z]+\\*?)+|\\*)?" "")
					    ("." "(\\.(\\*?([-0-9A-Z]+\\*?)+|\\*))?" "")
					    ("." "(\\.([1-9][0-9]*|newest|NEWEST|\\*))?" "")))

(defun to-regexp-or-namestring (x rp lp)
  (apply 'concatenate 'string
	 (mapcan (lambda (x y) (elsub x y rp lp))
		 x (if lp +logical-pathname-defaults+ +physical-pathname-defaults+))))

(defun directory-list-check (l)
  (when (listp l)
    (when (member (car l) '(:absolute :relative))
      (mapl (lambda (x &aux (c (car x))(d (cadr x)))
	      (when (and (member d '(:up :back)) (member c '(:absolute :wild-inferiors)))
		(return-from directory-list-check nil))) l))))
    
(defun canonicalize-pathname-directory (l)
  (cond ((eq l :wild) (canonicalize-pathname-directory '(:absolute :wild-inferiors)))
	((stringp l) (canonicalize-pathname-directory (list :absolute l)))
	((mapl (lambda (x &aux (c (car x)))
		 (when (and (or (stringp c) (eq c :wild)) (eq (cadr x) :back))
		   (return-from canonicalize-pathname-directory
		     (canonicalize-pathname-directory (nconc (ldiff l x) (cddr x)))))) l))))

(defvar *default-pathname-defaults* (init-pathname nil nil nil nil nil nil ""))
(declaim (type pathname *default-pathname-defaults*))

(defun toggle-case (x)
  (cond ((symbolp x) x)
	((listp x) (mapcar 'toggle-case x))
	((find-if 'upper-case-p x) (if (find-if 'lower-case-p x) x (string-downcase x)))
	((find-if 'lower-case-p x) (string-upcase x))
	(x)))

(defun logical-pathname (spec &aux (p (pathname spec)))
  (declare (optimize (safety 1)))
  (check-type spec pathname-designator)
  (check-type p logical-pathname)
  p)
  
(eval-when (compile eval)
  (defun strsym (p &rest r)
    (declare (:dynamic-extent r))
    (intern (apply 'concatenate 'string (mapcar 'string-upcase r)) p)))

#.`(defun make-pathname (&key (host nil hostp) (device nil devicep) (directory nil directoryp)
			      (name nil namep) (type nil typep) (version nil versionp)
			      defaults (case :local) namestring &aux defaulted (def (when defaults (pathname defaults))))
     (declare (optimize (safety 1)))
     (check-type host (or (member nil :unspecific) string))
     (check-type device (member nil :unspecific))
     (check-type directory (or (member nil :unspecific :wild) string list))
     (check-type name (or string (member nil :unspecific :wild)))
     (check-type type (or string (member nil :unspecific :wild)))
     (check-type version (or (integer 1) (member nil :unspecific :wild :newest)))
     (check-type defaults (or null pathname-designator))
     (check-type case (member :common :local))
     ,(flet ((def? (k) `(let* (,@(when (eq k 'host) `((def (or def *default-pathname-defaults*))))
			       (nk (if ,(strsym :si k "P") ,k (progn (setq defaulted t) (when def (,(strsym :si "C-PATHNAME-" k) def)))))
			       (nk (if (eq case :local) nk (progn (setq defaulted t) (toggle-case nk)))))
			nk)))
	`(let* ((h ,(def? 'host))
		(h (let ((h1 (when (logical-pathname-host-p h) h))) (unless (eq h h1) (setq defaulted t)) h1))
		(dev ,(def? 'device))
		(d ,(def? 'directory))
		(d (let ((d1 (canonicalize-pathname-directory d))) (unless (eq d d1) (setq defaulted t)) d1))
		(n ,(def? 'name))
		(typ ,(def? 'type))
		(v ,(def? 'version))
		(p (init-pathname h dev d n typ v
				  (or (unless defaulted namestring) (to-regexp-or-namestring (list h dev d n typ v) nil h)))))
	   (when h (c-set-t-tt p 1))
	   (unless (eq d (directory-list-check d))
	     (error 'file-error :pathname p :format-control "Bad directory list"))
	   p)))

(macrolet ((pn-accessor (k &aux (f (strsym :si "PATHNAME-" k)) (c (strsym :si "C-PATHNAME-" k)))
	      `(defun ,f (p &key (case :local) &aux (pn (pathname p)))
		 (declare (optimize (safety 1)))
		 (check-type p pathname-designator)
		 (let ((x (,c pn))) (if (eq case :local) x (toggle-case x))))))
  (pn-accessor host)
  (pn-accessor device)
  (pn-accessor directory)
  (pn-accessor name)
  (pn-accessor type)
  (pn-accessor version))

(defconstant +pathname-keys+ '(:host :device :directory :name :type :version))

#.`(defun mlp (p)
     (list ,@(mapcar (lambda (x) `(,(strsym :si "C-PATHNAME-" x) p)) +pathname-keys+)))

(defun pnl1 (x) (list* (pop x) (pop x) (append (pop x) x)))
(defun lnp (x) (list* (pop x) (pop x) (let ((q (last x 3))) (cons (ldiff x q) q))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_make_pathname.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_typep.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(defun infer-type (x y z) (declare (ignore x y)) z);avoid macroexpansion in bootstrap
(define-compiler-macro infer-type (x y z)
  `(infer-tp ,(cmp-eval x) ,(cmp-eval y) ,z))

(defun mkinf (f tp z &aux (z (if (cdr z) `(progn ,@z) (car z))))
  `(infer-type ',f ',tp ,z))

(defun ib (o l &optional f)
  (let* ((a (atom l))
	 (l (if a l (car l)))
	 (l (unless (eq '* l) l)))
    (or (not l) (if f (if a (<= l o) (< l o)) (if a (<= o l) (< o l))))))
(setf (get 'ib 'cmp-inline) t)

(defun db (o tp)
  (let* ((b (car tp))(i -1))
    (cond ((not tp))
	  ((eq b '*))
	  ((not (listp b)) (eql (c-array-rank o) b))
	  ((eql (length b) (c-array-rank o)) (not (member-if-not (lambda (x) (incf i) (or (eq x '*) (eql x (array-dims o i)))) b))))))
	 

(defun dbv (o tp)
  (let* ((b (car tp))(b (if (listp b) (car b) b)))
     (cond ((not tp))
	   ((eq b '*))
	   ((eql (c-array-dim o) b)))))
(setf (get 'db 'cmp-inline) t)
(setf (get 'dbv 'cmp-inline) t)


(defun ibb (o tp)
  (and (ib o (car tp) t) (ib o (cadr tp))))
(setf (get 'ibb 'cmp-inline) t)

(defun sdata-includes (x)
  (the (or s-data null) (*object (c-structure-self x) 4 nil nil)));FIXME s-data-name boostrap loop
(setf (get 'sdata-includes 'cmp-inline) t)
(defun sdata-included (x)
  (the proper-list (*object (c-structure-self x) 3 nil nil)));FIXME s-data-name boostrap loop
(setf (get 'sdata-included 'cmp-inline) t)
(defun sdata-name (x)
  (the symbol (*object (c-structure-self x) 0 nil nil)));FIXME s-data-name boostrap loop
(defun sdata-type (x)
  (the symbol (*object (c-structure-self x) 16 nil nil)));FIXME s-data-name boostrap loop
(setf (get 'sdata-name 'cmp-inline) t)

;; (defun mss (o sn) (or (eq o sn) (when (sdata-included sn) (let ((o (sdata-includes o))) (when o (mss o sn))))))
;; (setf (get 'mss 'cmp-inline) t)
(defun mss (o sn) (when o (or (eq (sdata-name o) sn) (mss (sdata-includes o) sn))))
(setf (get 'mss 'cmp-inline) t)

(defun structure-name (o) (sdata-name (c-structure-def o)))
(setf (get 'structure-name 'cmp-inline) t)


(eval-when
 (compile eval)
 (defun cfn (tp code) 
   (let* ((nc (cmp-norm-tp tp))
	  (a (type-and-list (list nc)))(c (calist2 a))
	  (f (best-type-of c))(it (caar c))(it (if (cdr it) (cons 'or it) (car it))))
     `(case (,f o)
	    (,(tps-ints a (cdr (assoc f +rs+))) ,(mkinf 'o it (list code)))
	    (otherwise ,(mkinf 'o `(not ,it) '(nil))))))
  (defun mksubb (o tp x)
   (case x
	 ((immfix bfix bignum integer ratio single-float double-float short-float long-float float rational real) `(ibb ,o ,tp))
	 (proper-cons `(unless (improper-consp ,o) t))
	 ((structure structure-object) `(if tp (mss (c-structure-def ,o) (car tp)) t))
;	 ((structure structure-object) `(if tp (when (member (structure-name ,o) tp) t) t))
	 (std-instance `(if tp (when (member (car tp) (si-class-precedence-list (si-class-of ,o))) t) t))
	 (mod `(let ((s (pop ,tp))) (<= 0 ,o (1- s))));FIXME error null tp
	 (signed-byte `(if tp (let* ((s (pop ,tp))(s (when s (ash 1 (1- s))))) (<= (- s) ,o (1- s))) t))
	 (unsigned-byte `(if tp (let* ((s (pop ,tp))(s (when s (ash 1 s)))) (<= 0 ,o (1- s))) (<= 0 ,o)))
	 (cons `(if tp (and (typep (car ,o) (car ,tp)) (typep (cdr ,o) (cadr ,tp)) t) t))
	 (otherwise t))))

#.`(defun listp (o) ,(cfn 'list t))

#.`(defun mtc (o tp &aux (otp (car tp))(tp otp)(lp (listp tp))(ctp (if lp (car tp) tp))(tp (when lp (cdr tp))))
     (case (when ctp (upgraded-complex-part-type ctp))
	   ,@(mapcar (lambda (x &aux (n (pop x)))
			 `(,n ,(cfn (car x) `(and (ibb (realpart o) tp) (ibb (imagpart o) tp))))) +ctps+)
	   (otherwise ,(cfn 'complex '(if tp (and (typep (realpart o) otp) (typep (imagpart o) otp) t) t)))))
					;FIXME the mutual recursion on typep prevents return type determination
(setf (get 'mtc 'cmp-inline) t)
		       

#.`(defun mta (o tp &aux (lp (listp tp))(ctp (if lp (car tp) tp))(tp (when lp (cdr tp))))
     (and (case (when ctp (upgraded-array-element-type ctp))
		,@(mapcar (lambda (x &aux (n (pop x))(n (if (eq t n) (list n) n)))
			    `(,n ,(cfn (car x) t))) (mapcar (lambda (x y) `(,(car x) (or ,(cadr x) ,(cadr y)))) +vtps+ +atps+))
		(otherwise ,(cfn 'array t)))
	  (db o tp)))
(setf (get 'mta 'cmp-inline) t)


#.`(defun mtv (o tp &aux (lp (listp tp))(ctp (if lp (car tp) tp))(tp (when lp (cdr tp))))
     (and (case (when ctp (upgraded-array-element-type ctp))
		,@(mapcar (lambda (x &aux (n (pop x))(n (if (eq t n) (list n) n)))
			    `(,n ,(cfn (car x) t))) +vtps+)
		(otherwise ,(cfn 'vector t)))
	  (dbv o tp)))
(setf (get 'mtv 'cmp-inline) t)

		       
(defun vtp (tp &aux (dims (cadr tp)))
  (cond ((eql 1 dims) `(,(car tp) *))
	((or (atom dims) (cdr dims)) nil)
	(tp)))
(setf (get 'vtp 'cmp-inline) t)

(defun valid-class-name (class &aux (name (si-class-name class)))
  (when (eq class (si-find-class name nil))
    name))
(setf (get 'valid-class-name 'cmp-inline) t)

(eval-when
 (compile eval)
 (defconstant +s+ `(list sequence function symbol boolean 
			 proper-cons
			 fixnum integer rational float real number;complex
			 character
			 hash-table pathname
			 stream 
			 double-float single-float
			 structure-object ;FIXME
			 unsigned-byte
			 signed-byte))
 (defconstant +rr+ (lremove-if (lambda (x) (type-and (cmp-norm-tp '(or complex array)) (cmp-norm-tp (car x)))) +r+)))

 (eval-when (eval compile) (defun tpc (&rest x) (tps-ints (type-and-list (mapcar 'cmp-norm-tp x)) (cdr (assoc 'tp7 +rs+)))))

#.`(defun type-spec-p (otp)
     (case (tp7 otp)
       (,(tpc 'std-instance) (si-classp otp))
       (,(tpc 'symbol) (not (eq otp 'values)))
       (,(tpc 'cons) (unless (improper-consp otp) 
		       (case (car otp) (function (not (cdr otp)))(values nil)(otherwise t))))
       (,(tpc 'structure) t)))
(setf (get 'type-spec-p 'cmp-inline) t)

#.`(defun typep (o otp &optional env &aux (lp (listp otp)))
     (declare (ignore env))
     (unless (type-spec-p otp);Cannot use check-type here
       (error 'type-error :datum otp :expected-type 'type-spec))
     (labels ((tpi (o ctp tp &aux (ntp (when (eq ctp 'array) (vtp tp)))(ctp (if ntp 'vector ctp))(tp (or ntp tp)))
		   (case ctp
			 ,@(mapcar (lambda (x &aux (c (if (atom x) x (car x)))) 
				     `(,c ,(cfn c (mksubb 'o 'tp c)))) (append +s+ +rr+))
			 (member (when (if (cdr tp) (member o tp) (when tp (eql o (car tp)))) t));FIXME
			 (eql (eql o (car tp)))
			 (complex (mtc o tp))
			 (vector (mtv o tp))
			 (array (mta o tp))
			 (or (when tp (or (typep o (car tp)) (tpi o ctp (cdr tp)))))
			 (and (if tp (and (typep o (car tp)) (tpi o ctp (cdr tp))) t))
			 (not (not (typep o (car tp))))
			 (satisfies (when (funcall (car tp) o) t))
			 ((nil t) (when ctp t));FIXME ctp not inferred here
			 (otherwise (let ((tem (expand-deftype otp))) (when tem (typep o tem)))))))
	     
	     (tpi o (if lp (car otp) otp) (when lp (cdr otp)))))


#.`(defun type-of (x)
     (typecase
      x
      (null 'null)(true 'true)
      ,@(mapcar (lambda (y) `(,y `(,',y ,x ,x))) +range-types+)
      ,@(mapcar (lambda (y &aux (b (pop y))) 
		 `(,(car y) (let ((r (realpart x))(i (imagpart x))) `(complex (,',b ,(min r i) ,(max r i)))))) +ctps+)
      ,@(mapcar (lambda (y &aux (b (car y))) `((array ,b) `(array ,',b ,(array-dimensions x)))) +vtps+)
      (std-instance (let* ((c (si-class-of x))) (or (valid-class-name c) c)))
      (structure (sdata-name (c-structure-def x)))
      ,@(mapcar (lambda (x) `(,x ',x)) (set-difference +kt+ 
						       (mapcar 'cmp-norm-tp '(boolean number array structure std-instance))
						       :test 'type-and))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_typep.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_sf.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :s)

(defun strcat (&rest r) (declare (:dynamic-extent r)) (nstring-downcase (apply 'string-concatenate r)))

(eval-when
 (eval compile) 
  
 (defun sferr (&rest r) (print r))

 (defun foo-reader (stream subchar)
   (declare (ignore subchar) (optimize (safety 2)))
   (let ((x (read-delimited-list #\} stream)))
     (let (zz z r) 
       (mapc #'(lambda (x) 
	       (cond ((member x '(|enum| |union| |struct| |unsigned|)) (setq zz x))
		     ((not z) (setq z (if zz (list zz x) x)))
		     ((integerp x) (setq r (cons (list z (cadar r) x) (cdr r))))
		     ((eq x '|;|) (setq z nil zz nil))
		     ((push (list z x) r)))) x) 
       (nreverse r))))
 
 (defun |;-reader| (stream subchar)
   (declare (ignore stream subchar) (optimize (safety 2)))
   '|;|)
 
 (defun readtable-h nil
   (si:set-readtable-case *readtable* :preserve)
   (set-macro-character #\{ 'foo-reader)
   (set-macro-character #\; '|;-reader|)
   (set-syntax-from-char #\# #\;)
   (set-syntax-from-char #\} #\))
   (dolist (l '(#\: #\| #\, #\. #\( #\)))
     (set-syntax-from-char l #\Space)))
 
 (defun get-com (f &aux x com td (*readtable* (copy-readtable)))
   
   (readtable-h)
   (let ((s (si::open-int f :input 'character nil nil nil nil :default)))
    (do ((y nil x)(z nil y)) 
	((eq 'eof (setq x (read s nil 'eof))) 
	 (unless (and com td) (sferr "h read error" x))
	 (list com td))
	(when (and (member z '(|struct| |union|)) (consp x)) 
	  (push (list z y x) com))
	(when (eq x '|typedef|) 
	  (push (read-delimited-list #\; s) td)))))
 
 (defun td (k l)
   (let* ((kn (when (symbolp k) (string-upcase (symbol-name k))))
	  (kk (when kn (mktp kn)))
	  (kk (when kk (intern kn :keyword)))
	  (x (car (member k l :key #'(lambda (x) (car (last x)))))))
     (cond (kk)
	   ((not x) k)
	   ((eq (car x) '|unsigned|) (cons (td (cadr x) l) (car x)))
	   ((not (cddr x)) (td (car x) l))
	   (x))))
 
 (defun mrin (f x &key key)
   (mapcan 'identity (maplist #'(lambda (x) (when (funcall f (funcall key (car x))) (list (car x)))) x)))

 (defun slist nil
   
   (let* ((com (get-com "../h/cmpinclude.h"))
	  (td (cadr com))
	  (com (car com))
	  (u (car (member-if #'(lambda (x) (and (eq (car x) '|union|) (eq (cadr x) '|lispunion|))) com)))
	  (u (mrin 'consp (caddr u) :key 'car)))
     (mapcar #'(lambda (x) 
	       (let ((y (car (member-if #'(lambda (z) 
					  (when (consp (car x))
					    (and  (eq (caar x) (car z)) (eq (cadar x) (cadr z))))) com)))) 
		 (list (car x) (cadr x)
		       (mapcar #'(lambda (z) (cons (td (car z) td) (cdr z))) (caddr y))))) u)))
 
 (defun bz (x) (ash 1 (+ x 3)))
 (defun ks (k &aux (x (or (cadr (assoc k +ks+)) +fl+))) (bz x))
 
 (defun bs (y &aux (w y)(k (pop y))(k (if (consp k) (car k) k)))
   (or (cadr y) (ks k)))
 
 (defun sb (c z &aux (q (load-time-value (mapcar 'bz '(0 1 2 3))))) ;FIXME dcomplex +kss+
   (or (car (member (+ (mod c (car q)) z) q :test '<=))
       (sferr "boo" c z)))
 
 (defun cmp-norm-tpp (x) x)

 (defun mtpp (k y &aux (zz (car y))(z (if (consp zz) (car zz) zz))(u (when (consp zz) (eq (cdr zz) '|unsigned|))))
   (cond ((caddr y) (unless u (sferr "bar" k y)) (cmp-norm-tpp `(unsigned-byte ,(caddr y))))
	 ((when (keywordp z) (eq k :object)) (mktp z));(get z 'lisp-type))
	 ((mktp k));((get k 'lisp-type))
	 (t)))
 
 (defun pp (y &aux (n (string (cadr y)))) (when (eql #\* (aref n 0)) (list :fixnum (intern (subseq n 1)))))
 
 (defun m& (x m) (if m `(& ,x ,m) x))
 (defun m<< (x s) (if (zerop s) x `(<< ,x ,s)))
 (defun m>> (x s) (if (zerop s) x `(>> ,x ,s)))
 (defun m\| (x m) (if m `(\| ,x ,m) x))
 (defun mm (m) (if (zerop (logand (ash 1 (1- fixnum-length)) m)) m (- m (ash 1 fixnum-length))))
 (defun m+ (a o) (if (zerop o) a `(c+ ,a ,o)))
 
 
 (defun gk (b y &aux (k (car y))(u (when (consp k) (eq (cdr k) '|unsigned|)))(k (if (consp k) (car k) k)))
   (cond ((< b (ks k)) (or (caar (member-if #'(lambda (x) (and (eql (bz (cadr x)) b) (eql (caddr x) (if u 1 0)))) +ks+)) (baboon)))
	 ((car (assoc k +ks+)))
	 ((keywordp k) :object)
	 (:fixnum)))
 
 (defun mktp (z &aux (z (string-upcase z))) (or (find-symbol z :cl) (get (find-symbol z :keyword) 'lisp-type)))
 
 (defun btp (z) (or (cmp-norm-tpp (mktp z)) t))
 
 (defun idefun (args &aux (n (pop args)))
   `(progn
      (defun ,n ,@args)
      (si::putprop ',n t 'si::cmp-inline)
      (export ',n)))
 
 (defun afn (n tp body &optional ytp) 
   (idefun `(,n (x ,@(when ytp `(y))) 
		(declare (optimize (safety 1)))
		,@(unless (eq tp t) `((check-type x ,tp))),@(when ytp `((check-type y ,ytp)))
	       ,@body)))
 
 (defun gbe (f tp o s sz b a)  `((the ,tp ,(m& (m>> `(,f ,a ,o nil nil) s) (when (< (+ s sz) b) (mm (1- (ash 1 sz))))))))
 (defun sbe (f    o s sz b a) 
   `((,f ,a ,o t ,(m\| (m<< 'y s) (when (< sz b) `(& (,f ,a ,o nil nil) ,(~ (mm (ash (1- (ash 1 sz)) s))))))) y))
 
 (defun fnk (k) (intern (string-concatenate "*" (string k))))
 
 (defun mnn (r z f) (intern (nstring-upcase (string-concatenate r z "-" f))))
 
 (defun mn (z p f &aux (f (strcat f))) (list (mnn "C-" z f) (mnn "C-SET-" z f)))
 
 (defun afn2 (z p c sz y &aux (b (sb c sz))(k (gk b y))(f (fnk k))(rtp (mtpp k y))(tp (btp z))(nl (mn z p (cadr y))))
   (multiple-value-bind
       (o s)
       (truncate c b)
     (multiple-value-bind
	 (bo s)
	 (truncate s 8)
       (let ((a (m+ `(address x) bo)))
	 (list (afn (pop nl) tp (gbe f rtp o s sz b a))
	       (afn (car nl) tp (sbe f o s sz b a) rtp))))))
 
 (defun nmf (x y &aux (p (strcat (cadr x) "_"))(f (strcat (cadr y)))(s (string= p (subseq f 0 (min (length f) (length p))))))
   (when s (rplaca (cdr y) (intern (subseq f (length p)))) t))
 
 (defun fp (c x y) 
   (cond ((nmf x y) x)
	 ((< c fixnum-length) (cons '(|struct| |t|) (cons '|t| (cddr x))))))
 
 (defun mrd (x &key test key)
   (mapcan 'identity
	   (maplist #'(lambda (x) (unless (member (funcall key (car x)) (cdr x) :test test :key key) (list (car x)))) x)))
 
 (defun macc nil 
   (mrd
    (mapcan #'(lambda (x &aux (c 0))
		(mapcan #'(lambda (y &aux (y (or (pp y) y))(sz (bs y))(c (prog1 c (incf c sz)))(x (fp c x y)))
			    (when x `((,(cadar x) ,(cadr x) ,c ,sz ,y)))) (caddr x))) (slist)) :test 'equal :key 'cddr)))

#.`(progn ,@(mapcan #'(lambda (x) (apply 'afn2 x)) (macc)))


;;FIXME these automatic, *c-stdesig-sdself, etc.
#.(idefun `(stdesig-self (s i) 
			 (declare (optimize (safety 1)))
			 (check-type i seqind)
			 (*char (c-stdesig-sdself s) i nil nil)))
#.(idefun `(set-stdesig-self (s i j) 
			     (declare (optimize (safety 1)))
			     (check-type i seqind)
			     (check-type j seqind)
			     (*char (c-stdesig-sdself s) i t j)))
#.(idefun `(function-env (fun i) 
			 (declare (optimize (safety 1)))
			 (check-type i seqind)
			 (*object (c-function-env fun) i nil nil)))
#.(idefun `(package-internal (p i) 
			     (declare (optimize (safety 1)))
			     (check-type i seqind)
			     (*object (c-package-internal p) i nil nil)))
#.(idefun `(package-external (p i) 
			     (declare (optimize (safety 1)))
			     (check-type i seqind)
			     (*object (c-package-external p) i nil nil)))
#.(idefun `(hashtable-self (h i) 
			   (declare (optimize (safety 1)))
			   (check-type i seqind)
			   (c+ (c-hashtable-self h) (<< i #.(integer-length (/ si::fixnum-length si::char-length))))))
#.(idefun `(array-dims (s i) 
		       (declare (optimize (safety 1)))
		       (check-type i seqind)
		       (the seqind (*fixnum (c-array-dims s) i nil nil))))
#.(idefun `(set-array-dims (s i j)
			   (declare (optimize (safety 1)))
			   (check-type i seqind)
			   (check-type j seqind)
			   (the seqind (*fixnum (c-array-dims s) i t j))))



;; #.(idefun `(funcallable-symbol-p 
;; 	    (s)
;; 	    (and (symbolp s)
;; 		 (/= (si::address (c-symbol-gfdef s)) 0)
;; 		 (= (c-symbol-mflag s) 0)
;; 		 (= (c-symbol-sfdef s) (si::address nil)))))

;; #.(idefun `(fsf
;; 	    (s)
;; 	    (declare (optimize (safety 1)))
;; 	    (assert (funcallable-symbol-p s));  (check-type s funcallable-symbol); FIXME
;; 	    (the function (c-symbol-gfdef s))));FIXME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_sf.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_auto_new.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package :si)
;;; Autoloaders.


;;; DEFAUTOLOAD definitions. for lsp directory files normally loaded.
(if (fboundp 'abs) (push :numlib *features*))
;;hack to avoid interning all the :symbols if the files are loaded..
#-numlib
(progn
;; (autoload 'abs '|gcl_numlib|)
;; (autoload 'acos '|gcl_numlib|)
;; (autoload 'acosh '|gcl_numlib|)
;; (autoload 'adjust-array '|gcl_arraylib|)
;; (autoload 'apropos '|gcl_packlib|)
;; (autoload 'apropos-list '|gcl_packlib|)
;; (autoload 'array-dimensions '|gcl_arraylib|)
;; (autoload 'array-in-bounds-p '|gcl_arraylib|)
;; (autoload 'array-row-major-index '|gcl_arraylib|)
;; (autoload 'asin '|gcl_numlib|)
;; (autoload 'asinh  '|gcl_numlib|)
;; (autoload 'atanh '|gcl_numlib|)
;; (autoload 'best-array-element-type '|gcl_arraylib|)
;; (autoload 'bit '|gcl_arraylib|)
;; (autoload 'bit-and '|gcl_arraylib|)
;; (autoload 'bit-andc1 '|gcl_arraylib|)
;; (autoload 'bit-andc2 '|gcl_arraylib|)
;; (autoload 'bit-eqv '|gcl_arraylib|)
;; (autoload 'bit-ior '|gcl_arraylib|)
;; (autoload 'bit-nand '|gcl_arraylib|)
;; (autoload 'bit-nor '|gcl_arraylib|)
;; (autoload 'bit-not '|gcl_arraylib|)
;; (autoload 'bit-orc1 '|gcl_arraylib|)
;; (autoload 'bit-orc2 '|gcl_arraylib|)
;; (autoload 'bit-xor '|gcl_arraylib|)
;; (autoload 'byte '|gcl_numlib|)
;; (autoload 'byte-position '|gcl_numlib|)
;; (autoload 'byte-size '|gcl_numlib|)
;; (autoload 'cis '|gcl_numlib|)
;; (autoload 'coerce '|gcl_predlib|)
;; (autoload 'compile-file '|gcl_loadcmp|)
;; (autoload 'compile '|gcl_loadcmp|)
;; (autoload 'disassemble '|gcl_loadcmp|)
;; (autoload 'concatenate '|gcl_seq|)
;; (autoload 'cosh '|gcl_numlib|)
;; (autoload 'count '|gcl_seqlib|)
;; (autoload 'count-if '|gcl_seqlib|)
;; (autoload 'count-if-not '|gcl_seqlib|)
;; (autoload 'decode-universal-time '|gcl_mislib|)
;; (autoload 'delete '|gcl_seqlib|)
;; (autoload 'delete-duplicates '|gcl_seqlib|)
;; (autoload 'delete-if '|gcl_seqlib|)
;; (autoload 'delete-if-not  '|gcl_seqlib|)
;; (autoload 'deposit-field '|gcl_numlib|)
;; (autoload 'describe '|gcl_describe|)
;; (autoload 'dpb '|gcl_numlib|)
;; (autoload 'dribble '|gcl_iolib|)
;; (autoload 'encode-universal-time '|gcl_mislib|)
;; (autoload 'every '|gcl_seq|)
;; (autoload 'fceiling '|gcl_numlib|)
;; (autoload 'ffloor '|gcl_numlib|)
;; (autoload 'fill '|gcl_seqlib|)
;; (autoload 'find '|gcl_seqlib|)
;; (autoload 'find-all-symbols '|gcl_packlib|)
;; (autoload 'find-if '|gcl_seqlib|)
;; (autoload 'find-if-not '|gcl_seqlib|)
;; (autoload 'fround '|gcl_numlib|)
;; (autoload 'ftruncate '|gcl_numlib|)
;; #-unix (autoload 'get-decoded-time '|gcl_mislib|)
;; #+aosvs (autoload 'get-universal-time '|gcl_mislib|)
;; (autoload 'get-setf-method '|gcl_setf|)
;; (autoload 'get-setf-method-multiple-value '|gcl_setf|)
;; (autoload 'inspect '|gcl_describe|)
;; (autoload 'intersection '|gcl_listlib|)
;; (autoload 'isqrt '|gcl_numlib|)
;; (autoload 'ldb '|gcl_numlib|)
;; (autoload 'ldb-test '|gcl_numlib|)
;; (autoload 'logandc1 '|gcl_numlib|)
;; (autoload 'logandc2 '|gcl_numlib|)
;; (autoload 'lognand '|gcl_numlib|)
;; (autoload 'lognor '|gcl_numlib|)
;; (autoload 'lognot '|gcl_numlib|)
;; (autoload 'logorc1 '|gcl_numlib|)
;; (autoload 'logorc2 '|gcl_numlib|)
;; (autoload 'logtest '|gcl_numlib|)
;; (autoload 'make-array '|gcl_arraylib|)
;; (autoload 'make-sequence '|gcl_seq|)
;; (autoload 'map '|gcl_seq|)
;; (autoload 'mask-field '|gcl_numlib|)
;; (autoload 'merge '|gcl_seqlib|)
;; (autoload 'mismatch '|gcl_seqlib|)
;; (autoload 'nintersection '|gcl_listlib|)
;; (autoload 'notany '|gcl_seq|)
;; (autoload 'notevery '|gcl_seq|)
;; (autoload 'si::normalize-type ':predlib)
;; (autoload 'nset-difference '|gcl_listlib|)
;; (autoload 'nset-exclusive-or '|gcl_listlib|)
;; (autoload 'nsubstitute '|gcl_seqlib|)
;; (autoload 'nsubstitute-if '|gcl_seqlib|)
;; (autoload 'nsubstitute-if-not '|gcl_seqlib|)
;; (autoload 'nunion '|gcl_listlib|)
;; (autoload 'phase '|gcl_numlib|)
;; (autoload 'position '|gcl_seqlib|)
;; (autoload 'position-if '|gcl_seqlib|)
;; (autoload 'position-if-not '|gcl_seqlib|)
;; (autoload 'prin1-to-string '|gcl_iolib|)
;; (autoload 'princ-to-string '|gcl_iolib|)
;; (autoload 'rational '|gcl_numlib|)
;; (autoload 'rationalize '|gcl_numlib|)
;; (autoload 'read-from-string '|gcl_iolib|)
;; (autoload 'reduce '|gcl_seqlib|)
;; (autoload 'remove '|gcl_seqlib|)
;; (autoload 'remove-duplicates '|gcl_seqlib|)
;; (autoload 'remove-if '|gcl_seqlib|)
;; (autoload 'remove-if-not '|gcl_seqlib|)
;; (autoload 'replace '|gcl_seqlib|)
;; (autoload 'sbit '|gcl_arraylib|)
;; (autoload 'search '|gcl_seqlib|)
;; (autoload 'set-difference '|gcl_listlib|)
;; (autoload 'set-exclusive-or '|gcl_listlib|)
;; (autoload 'signum '|gcl_numlib|)
;; (autoload 'sinh '|gcl_numlib|)
;; (autoload 'some '|gcl_seq|)
;; (autoload 'sort '|gcl_seqlib|)
;; (autoload 'stable-sort '|gcl_seqlib|)
;; (autoload 'subsetp '|gcl_listlib|)
;; (autoload 'substitute '|gcl_seqlib|)
;; (autoload 'substitute-if '|gcl_seqlib|)
;; (autoload 'substitute-if-not '|gcl_seqlib|)
;; (autoload 'subtypep '|gcl_predlib|)
;; (autoload 'tanh '|gcl_numlib|)
;; (autoload 'typep '|gcl_predlib|)
;; (autoload 'union '|gcl_listlib|)
;; (autoload 'vector '|gcl_arraylib|)
;; (autoload 'vector-pop '|gcl_arraylib|)
;; (autoload 'vector-push '|gcl_arraylib|)
;; (autoload 'vector-extend '|gcl_arraylib|)
;; (autoload 'write-to-string '|gcl_iolib|)
;; (autoload 'y-or-n-p '|gcl_iolib|)
;; (autoload 'yes-or-no-p '|gcl_iolib|)

;; (autoload 'logical-pathname-translations '|gcl_iolib|)
;; (autoload 'si::set-logical-pathname-translations '|gcl_iolib|)
;; (autoload 'ensure-directories-exist '|gcl_iolib|)

(set-dispatch-macro-character #\# #\a 'si::sharp-a-reader)
(set-dispatch-macro-character #\# #\A 'si::sharp-a-reader)
;(autoload 'si::sharp-a-reader '"iolib")
(set-dispatch-macro-character #\# #\s 'si::sharp-s-reader)
(set-dispatch-macro-character #\# #\S 'si::sharp-s-reader)
;(autoload 'si::sharp-s-reader '|gcl_iolib|)


;;; DEFAUTOLOADMACRO definitions.

;; (autoload-macro 'assert '|gcl_assert|)
;; (autoload-macro 'ccase '|gcl_assert|)
;; (autoload-macro 'check-type '|gcl_assert|)
;; (autoload-macro 'ctypecase '|gcl_assert|)
;; (autoload-macro 'decf '|gcl_setf|)
;; (autoload-macro 'define-modify-macro '|gcl_setf|)
;; (autoload-macro 'define-setf-method '|gcl_setf|)
;; (autoload-macro 'defsetf '|gcl_setf|)
;; (autoload-macro 'defstruct '|gcl_defstruct|)
;; (autoload-macro 'si::define-structure '|gcl_defstruct|)
;; (autoload-macro 'deftype '|gcl_predlib|)
;; (autoload-macro 'do-all-symbols '|gcl_packlib|)
;; (autoload-macro 'do-external-symbols '|gcl_packlib|)
;; (autoload-macro 'do-symbols '|gcl_packlib|)
;; (autoload-macro 'ecase '|gcl_assert|)
;; (autoload-macro 'etypecase '|gcl_assert|)
;; (autoload-macro 'incf '|gcl_setf|)
;; (autoload-macro 'pop '|gcl_setf|)
;; (autoload-macro 'push '|gcl_setf|)
;; (autoload-macro 'pushnew '|gcl_setf|)
;; (autoload-macro 'remf '|gcl_setf|)
;; (autoload-macro 'rotatef '|gcl_setf|)
;; (autoload-macro 'setf '|gcl_setf|)
;; (autoload-macro 'shiftf '|gcl_setf|)
;; (autoload-macro 'step '|gcl_trace|)
;; (autoload-macro 'time '|gcl_mislib|)
;; (autoload-macro 'trace '|gcl_trace|)
;; (autoload-macro 'typecase '|gcl_assert|)
;; (autoload-macro 'untrace '|gcl_trace|)
;; (autoload-macro 'with-input-from-string '|gcl_iolib|)
;; (autoload-macro 'with-open-file '|gcl_iolib|)
;; (autoload-macro 'with-open-stream '|gcl_iolib|)
;; (autoload-macro 'with-output-to-string '|gcl_iolib|)
;; (autoload-macro 'with-standard-io-syntax '|gcl_iolib|)

)   ;;end autoloads of normally loaded files.j
(if (find-package "COMPILER") (push :compiler *features*))
#+compiler
(autoload 'compiler::emit-fn '|../cmpnew/gcl_collectfn|)
(autoload 'compiler::init-fn '|../cmpnew/gcl_collectfn|)
(autoload 'si::monstartup '"gprof")
(autoload  'si::set-up-profile '"profile")

;; (AUTOLOAD 'IDESCRIBE '|gcl_info|)
;; (AUTOLOAD 'INFO '|gcl_info|)
;; (AUTOLOAD 'LIST-MATCHES '|gcl_info|)
;; (AUTOLOAD 'get-match '|gcl_info|)
(AUTOLOAD 'print-node '|tinfo|)
(AUTOLOAD 'offer-choices '|tinfo|)
(AUTOLOAD 'tkconnect '|tkl|)




;; the sun has a broken ypbind business, if one wants to save.
;; So to stop users from invoking this
#+sun
(defun user-homedir-pathname ()
 (let* ((tem (si::getenv "HOME"))
	(l (- (length tem) 1)))
   (cond ((null tem) nil)
	 (t 
	  (or (and (>= l 0)
		   (eql (aref tem l) #\/))
	      (setq tem (concatenate 'string tem "/")))
	  (pathname tem)))))


(AUTOLOAD 'init-readline '|gcl_readline|)
(AUTOLOAD 'user::xgcl-demo '|gcl_dwtest|)
(defun user::xgcl nil
 (use-package :xlib)
 (format t "Welcome to xgcl! Try (xgcl-demo) for a demonstration."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_auto_new.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_packlib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;    packlib.lsp
;;;;
;;;;                    package routines


;; (in-package 'lisp)


;; (export '(find-all-symbols do-symbols do-external-symbols do-all-symbols with-package-iterator))
;; (export '(apropos apropos-list))

(in-package :system)

;;
;; This slightly slower version uses less invocation history stack space
;;
;; (defmacro with-package-iterator ((name packlist key &rest keys) &rest body
;; 				 &aux (*gensym-counter* 0)
;; 				 (pl (sgen "WPI-PL")) (ql (sgen "WPI-QL"))
;; 				 (ilim (sgen "WPI-ILIM")) (elim (sgen "WPI-ELIM"))
;; 				 (p (sgen "WPI-P")) (q (sgen "WPI-Q")) (l (sgen "WPI-L"))
;; 				 (a (sgen "WPI-A")) (x (sgen "WPI-X")) (y (sgen "WPI-Y")))
;;   (declare (optimize (safety 2)))
;;   (let (int ext inh)
;;     (dolist (key (cons key keys))
;;       (ecase key
;; 	     (:internal  (setq int t))
;; 	     (:external  (setq ext t))
;; 	     (:inherited (setq inh t))))
;;     `(let* ((,pl ,packlist) ,p ,q ,ql (,x 0) (,y 0) (,ilim 0) (,elim 0) ,l ,a)
;;        (declare ((integer 0 1048573) ,x ,y ,ilim ,elim) (ignorable ,x ,y ,ilim ,elim))
;;        (labels 
;; 	((match (s l) (member-if (lambda (x) (declare (symbol x)) (string= s x)) l))
;; 	 (iematch (s p h) (or (match s (package-internal p (mod h (package-internal_size p))))
;; 			      (match s (package-external p (mod h (package-external_size p))))))
;; 	 (next-var nil 
;; 		   (tagbody 
;; 		    :top
;; 		    (cond ,@(when (or int ext) `(((when (eq ,q ,p) ,l) (return-from next-var (prog1 ,l (pop ,l))))))
;; 			  ,@(when inh `(((unless (eq ,q ,p) ,l) 
;; 					 (let* ((v (prog1 ,l (pop ,l))) (s (symbol-name (car v))) (h (pack-hash s)))
;; 					   (when (iematch s ,p h) (go :top))
;; 					   (return-from next-var (progn (setq ,a :inherited) v))))))
;; 			  ,@(when int `(((and (eq ,q ,p) (< ,x ,ilim)) (setq ,l (package-internal ,q ,x) ,a :internal ,x (1+ ,x)) (go :top))))
;; 			  ,@(when (or ext inh) `(((< ,y ,elim) (setq ,l (package-external ,q ,y) ,a :external ,y (1+ ,y)) (go :top))))
;; 			  (,ql 
;; 			   (setq ,x 0 ,y 0 ,q (if (listp ,ql) (pop ,ql) (prog1 ,ql (setq ,ql nil))))
;; 			   (multiple-value-setq (,elim ,ilim) (package-size ,q))
;; 			   (go :top))
;; 			  (,pl 
;; 			   (setq ,p (coerce-to-package (if (listp ,pl) (pop ,pl) (prog1 ,pl (setq ,pl nil))))
;; 				 ,ql ,(if inh `(cons ,p (package-use-list ,p)) p))
;; 			   (go :top)))))
;; 	 (,name nil (let ((f (next-var))) (values f (car f) ,a ,p))))
;; 	,@body))))

(defmacro with-package-iterator ((name packlist key &rest keys) &rest body
				 &aux (*gensym-counter* 0)
				 (pl (sgen "WPI-PL")) (ql (sgen "WPI-QL"))
				 (ilim (sgen "WPI-ILIM")) (elim (sgen "WPI-ELIM"))
				 (p (sgen "WPI-P")) (q (sgen "WPI-Q")) (l (sgen "WPI-L"))
				 (a (sgen "WPI-A")) (x (sgen "WPI-X")) (y (sgen "WPI-Y")))
  (declare (optimize (safety 1)))
  (let (int ext inh)
    (dolist (key (cons key keys))
      (ecase key
	     (:internal  (setq int t))
	     (:external  (setq ext t))
	     (:inherited (setq inh t))))
    `(let* ((,pl ,packlist) ,p ,q ,ql (,x 0) (,y 0) (,ilim 0) (,elim 0) ,l ,a)
       (declare ((integer 0 1048573) ,x ,y ,ilim ,elim) (ignorable ,x ,y ,ilim ,elim))
       (labels 
	((match (s l) (member-if (lambda (x) (declare (symbol x)) (string= s x)) l))
	 (inh-match (&aux (v (prog1 ,l (pop ,l))) (s (symbol-name (car v))) (h (pack-hash s)))
		    (cond ((match s (package-internal ,p (mod h (package-internal_size ,p)))) (next-var))
			  ((match s (package-external ,p (mod h (package-external_size ,p)))) (next-var))
			  ((setq ,a :inherited) v)))
	 (next-var nil 
		   (cond ,@(when (or int ext) `(((when (eq ,q ,p) ,l) (prog1 ,l (pop ,l)))))
			 ,@(when inh `(((unless (eq ,q ,p) ,l) (inh-match))))
			 ,@(when int `(((and (eq ,q ,p) (< ,x ,ilim)) (setq ,l (package-internal ,q ,x) ,a :internal ,x (1+ ,x)) (next-var))))
			 ,@(when (or ext inh) `(((< ,y ,elim) (setq ,l (package-external ,q ,y) ,a :external ,y (1+ ,y)) (next-var))))
			 (,ql 
			  (setq ,x 0 ,y 0 ,q (if (listp ,ql) (pop ,ql) (prog1 ,ql (setq ,ql nil))))
			  (multiple-value-setq (,elim ,ilim) (package-size ,q))
			  (next-var))
			 (,pl 
			  (setq ,p (coerce-to-package (if (listp ,pl) (pop ,pl) (prog1 ,pl (setq ,pl nil))))
				,ql ,(if inh `(cons ,p (package-use-list ,p)) p))
			  (next-var))))
	 (,name nil (let ((f (next-var))) (values f (car f) ,a ,p))))
	,@body))))

;; (defmacro with-package-iterator ((name packlist key &rest keys) &rest body
;; 				 &aux (*gensym-counter* 0)
;; 				 (pl (sgen "WPI-PL")) (ql (sgen "WPI-QL"))
;; 				 (ilim (sgen "WPI-ILIM")) (elim (sgen "WPI-ELIM"))
;; 				 (p (sgen "WPI-P")) (q (sgen "WPI-Q")) (l (sgen "WPI-L"))
;; 				 (a (sgen "WPI-A")) (x (sgen "WPI-X")) (y (sgen "WPI-Y")))
;;   (declare (optimize (safety 2)))
;;   (let (int ext inh)
;;     (dolist (key (cons key keys))
;;       (ecase key
;; 	     (:internal  (setq int t))
;; 	     (:external  (setq ext t))
;; 	     (:inherited (setq inh t))))
;;     `(let* ((,pl ,packlist) ,p ,q ,ql (,x 0) (,y 0) (,ilim 0) (,elim 0) ,l ,a)
;;        (declare ((integer 0 1048573) ,x ,y ,ilim ,elim) (ignorable ,x ,y ,ilim ,elim))
;;        (labels 
;; 	((match (s l) (member-if (lambda (x) (declare (symbol x)) (string= s x)) l))
;; 	 (inh-match (&aux (v (prog1 ,l (pop ,l))) (s (symbol-name (car v))) (h (pack-hash s)))
;; 		    (cond ((match s (package-internal ,p (mod h (package-internal_size ,p)))) (next-var))
;; 			  ((match s (package-external ,p (mod h (package-external_size ,p)))) (next-var))
;; 			  ((setq ,a :inherited) v)))
;; 	 (next-var nil 
;; 		   (tagbody
;; 		    :top
;; 		    (cond ,@(when (or int ext) `(((when (eq ,q ,p) ,l) (return-from next-var (prog1 ,l (pop ,l))))))
;; 			  ,@(when inh `(((unless (eq ,q ,p) ,l) (return-from next-var (inh-match)))))
;; 			  ,@(when int `(((and (eq ,q ,p) (< ,x ,ilim)) (setq ,l (package-internal ,q ,x) ,a :internal ,x (1+ ,x)) (go :top))))
;; 			  ,@(when (or ext inh) `(((< ,y ,elim) (setq ,l (package-external ,q ,y) ,a :external ,y (1+ ,y)) (go :top))))
;; 			  (,ql 
;; 			   (setq ,x 0 ,y 0 ,q (if (listp ,ql) (pop ,ql) (prog1 ,ql (setq ,ql nil))))
;; 			   (multiple-value-setq (,elim ,ilim) (package-size ,q))
;; 			   (go :top))
;; 			  (,pl 
;; 			   (setq ,p (coerce-to-package (if (listp ,pl) (pop ,pl) (prog1 ,pl (setq ,pl nil))))
;; 				 ,ql ,(if inh `(cons ,p (package-use-list ,p)) p))
;; 			   (go :top)))))
;; 	 (,name nil (let ((f (next-var))) (values f (car f) ,a ,p))))
;; 	,@body))))



(eval-when 
 (eval compile)
 (defmacro do-symbols1 ((var package result-form &rest keys) body
			&aux (*gensym-counter* 0)(m (sgen "DS-M"))(f (sgen "DS-F")))
  `(multiple-value-bind
    (doc declarations check-types body)
    (parse-body-header ,body)
    (declare (ignore doc))
    `(with-package-iterator 
      (,',f ,,package ,,@keys)
      (do (,',m ,,var) ((not (multiple-value-setq (,',m ,,var) (,',f))) ,,result-form)
	  (declare (ignorable ,',m) (symbol ,,var))
	  ,@declarations
	  ,@check-types
	  ,@body)))))

(defmacro do-symbols ((var &optional (package '*package*) result-form) &rest body)
  (do-symbols1 (var package result-form :internal :external :inherited) body))

(defmacro do-external-symbols ((var &optional (package '*package*) result-form) &rest body)
  (do-symbols1 (var package result-form :external) body))

(defmacro do-all-symbols ((var &optional result-form) &rest body)
  (do-symbols1 (var '(list-all-packages) result-form :internal :external :inherited) body))

(defun find-all-symbols (sd)
  (declare (optimize (safety 1)))
  (check-type sd string-designator)
  (setq sd (string  sd))
  (mapcan (lambda (p)
	    (multiple-value-bind 
	     (s i)
	     (find-symbol sd p)
	     (when (or (eq i :internal) (eq i :external))
		 (list s))))
          (list-all-packages)))

;; (defun substringp (sub str)
;;   (do ((i (- (length str) (length sub)))
;;        (l (length sub))
;;        (j 0 (1+ j)))
;;       ((> j i) nil)
;;     (when (string-equal sub str :start2 j :end2 (+ j l))
;;           (return t))))


(defun print-symbol-apropos (symbol)
  (prin1 symbol)
  (when (fboundp symbol)
        (if (special-operator-p symbol)
            (princ "  Special form")
            (if (macro-function symbol)
                (princ "  Macro")
                (princ "  Function"))))
  (when (boundp symbol)
        (if (constantp symbol)
            (princ "  Constant: ")
            (princ "  has value: "))
        (prin1 (symbol-value symbol)))
  (terpri))

(defun apropos-list (string &optional package &aux list (package (or package (list-all-packages))))
  (declare (optimize (safety 1)))
  (setq string (string string))
  (do-symbols (symbol package list) ;FIXME?
	       (when (search string (string symbol))
		  (push symbol list)))
  (stable-sort list 'string< :key 'symbol-name))

(defun apropos (string &optional package)
  (declare (optimize (safety 1)))
  (dolist (symbol (apropos-list string package))
    (print-symbol-apropos symbol))
  (values))

(defun package-name (p) 
  (c-package-name (si::coerce-to-package p)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_packlib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_serror.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package :si)

(macrolet 
 ((make-conditionp (condition &aux (n (intern (concatenate 'string (string condition) "P"))))
		   `(defun ,n (x &aux (z (si-find-class ',condition nil)))
		      (when z
			(funcall (setf (symbol-function ',n) (lambda (x) (typep x z))) x))))
  (make-condition-classp (class &aux (n (intern (concatenate 'string (string class) "-CLASS-P"))))
			 `(defun ,n (x &aux (s (si-find-class 'standard-class nil)) (z (si-find-class ',class nil)))
			    (when (and s z)
			      (funcall (setf (symbol-function ',n)
					     (lambda (x &aux (x (if (symbolp x) (si-find-class x nil) x)))
					       (when (typep x s)
						 (member z (si-class-precedence-list x))))) x)))))
 (make-conditionp condition)
 (make-conditionp warning)
 (make-condition-classp condition)
 (make-condition-classp simple-condition))
 

(defun coerce-to-condition (datum arguments default-type function-name)
  (cond ((conditionp datum)
	 (if arguments
	     (cerror "ignore the additional arguments."
		     'simple-type-error
		     :datum arguments
		     :expected-type 'null
		     :format-control "you may not supply additional arguments ~
				     when giving ~s to ~s."
		     :format-arguments (list datum function-name)))
	 datum)
        ((condition-class-p datum)
	 (apply #'make-condition datum arguments))
        ((when (condition-class-p default-type) (or (stringp datum) (functionp datum)))
	 (make-condition default-type :format-control datum :format-arguments arguments))
	((coerce-to-string datum arguments))))

(defvar *handler-clusters* nil)
(defvar *break-on-signals* nil)

(defun signal (datum &rest arguments)
  (declare (optimize (safety 1)))
  (let ((*handler-clusters* *handler-clusters*)
	(condition (coerce-to-condition datum arguments 'simple-condition 'signal)))
    (if (typep condition *break-on-signals*)
	(break "~a~%break entered because of *break-on-signals*." condition))
    (do nil ((not *handler-clusters*))
	(dolist (handler (pop *handler-clusters*))
	  (when (typep condition (car handler))
	    (funcall (cdr handler) condition))))
    nil))

(defvar *debugger-hook* nil)
(defvar *debug-level* 0)
(defvar *debug-restarts* nil)
(defvar *debug-abort* nil)
(defvar *debug-continue* nil)
(defvar *abort-restarts* nil)

(defun break-level-invoke-restart (n)
  (cond ((when (plusp n) (< n (+ (length *debug-restarts*) 1)))
	 (invoke-restart-interactively (nth (1- n) *debug-restarts*)))
	((format t "~&no such restart."))))

(defun fun-name (fun) (sixth (c-function-plist fun)))

(defun find-ihs (s i &optional (j i))
  (cond ((eq (ihs-fname i) s) i)
	((and (> i 0) (find-ihs s (1- i) j)))
	(j)))

(defmacro without-interrupts (&rest forms)
  `(let (*quit-tag* *quit-tags* *restarts*)
     ,@forms))

(defun process-args (args &optional fc fa others);FIXME do this without consing, could be oom
  (cond ((not args) (nconc (nreverse others) (when (and fc fa) (list (apply 'format nil fc fa)))))
	((eq (car args) :format-control)
	 (process-args (cddr args) (cadr args) fa others))
	((eq (car args) :format-arguments)
	 (process-args (cddr args) fc (cadr args) others))
	((process-args (cdr args) fc fa (cons (car args) others)))))

(defun coerce-to-string (datum args) 
  (cond ((stringp datum)
	 (if args 
	     (let ((*print-pretty* nil)
		   (*print-level* *debug-print-level*)
		   (*print-length* *debug-print-level*)
		   (*print-case* :upcase))
	       (apply 'format nil datum args))
	   datum))
	((symbolp datum)
	 (let ((args (process-args args)))
	   (substitute 
	    #\^ #\~ 
	    (coerce-to-string
	     (if args
		 (apply 'string-concatenate (cons datum (make-list (length args) :initial-element " ~s")))
	       (string datum))
	     args))))
	("unknown error")))

(defun warn (datum &rest arguments)
  (declare (optimize (safety 2)))
  (let ((c (process-error datum arguments 'simple-warning)))
    (check-type c (or string (satisfies warningp)) "a warning condition")
    (when *break-on-warnings*
      (break "~A~%break entered because of *break-on-warnings*." c))
    (restart-case
     (signal c)
     (muffle-warning nil :report "Skip warning."  (return-from warn nil)))
    (format *error-output* "~&Warning: ~a~%" c)
    nil))

(dolist (l '(break cerror error universal-error-handler ihs-top get-sig-fn-name next-stack-frame check-type-symbol))
  (setf (get l 'dbl-invisible) t))

(defvar *sig-fn-name* nil)

(defun get-sig-fn-name (&aux (p (ihs-top))(p (next-stack-frame p)))
  (when p (ihs-fname p)))

(defun process-error (datum args &optional (default-type 'simple-error))
  (let ((internal (cond ((simple-condition-class-p datum)
			 (find-symbol (concatenate 'string "INTERNAL-" (string datum)) :conditions))
			((condition-class-p datum)
			 (find-symbol (concatenate 'string "INTERNAL-SIMPLE-" (string datum)) :conditions)))))
    (coerce-to-condition (or internal datum) (if internal (list* :function-name *sig-fn-name* args) args) default-type 'process-error)))

(defun universal-error-handler (n cp fn cs es &rest args &aux (*sig-fn-name* fn))
  (declare (ignore es))
  (if cp (apply #'cerror cs n args) (apply #'error n args)))

(defun cerror (continue-string datum &rest args &aux (*sig-fn-name* (or *sig-fn-name* (get-sig-fn-name))))
  (values 
   (with-simple-restart 
    (continue continue-string args)
    (apply #'error datum args))))
(putprop 'cerror t 'compiler::cmp-notinline)


(defun error (datum &rest args &aux (*sig-fn-name* (or *sig-fn-name* (get-sig-fn-name))))
  (let ((c (process-error datum args))(q (or *quit-tag* +top-level-quit-tag+)))
    (signal c)
    (invoke-debugger c)
    (throw q q)))
(putprop 'error t 'compiler::cmp-notinline)
  

(defun invoke-debugger (condition)

  (when *debugger-hook*
	(let ((hook *debugger-hook*) *debugger-hook*)
	  (funcall hook condition hook)))

  (maybe-clear-input)
  
  (let ((correctable (find-restart 'continue))
	*print-pretty*
	(*print-level* *debug-print-level*)
	(*print-length* *debug-print-level*)
	(*print-case* :upcase))
    (terpri *error-output*)
    (format *error-output* (if (and correctable *break-enable*) "~&Correctable error: " "~&Error: "))
    (let ((*indent-formatted-output* t))
      (when (stringp condition) (format *error-output* condition)))
    (terpri *error-output*)
    (if (> (length *link-array*) 0)
	(format *error-output* "Fast links are on: do (si::use-fast-links nil) for debugging~%"))
    (format *error-output* "Signalled by ~:@(~S~).~%" (or *sig-fn-name* "an anonymous function"))
    (when (and correctable *break-enable*)
      (format *error-output* "~&If continued: ")
      (funcall (restart-report-function correctable) *error-output*))
    (force-output *error-output*)
    (when *break-enable* (break-level condition))))


(defun dbl-eval (- &aux (break-command t))
  (let ((val-list (multiple-value-list
		   (cond 
		    ((keywordp -) (break-call - nil 'break-command))
		    ((and (consp -) (keywordp (car -))) (break-call (car -) (cdr -) 'break-command))
		    ((integerp -) (break-level-invoke-restart -))     
		    (t (setq break-command nil) (evalhook - nil nil *break-env*))))))
    (cons break-command val-list)))

(defun dbl-rpl-loop (p-e-p)

  (setq +++ ++ ++ + + -)

  (if *no-prompt*
      (setq *no-prompt* nil)
    (format *debug-io* "~&~a~a>~{~*>~}"
	    (if p-e-p "" "dbl:")
	    (if (eq *package* (find-package 'user)) "" (package-name *package*))
	    *break-level*))
  (force-output *error-output*)

  (setq - (dbl-read *debug-io* nil *top-eof*))
  (when (eq - *top-eof*) (bye -1))
  (let* ((ev (dbl-eval -))
	 (break-command (car ev))
	 (values (cdr ev)))
    (unless (and break-command (eq (car values) :resume))
      (setq /// // // / / values *** ** ** * * (car /))
      (fresh-line *debug-io*)
      (dolist (val /)
	(prin1 val *debug-io*)
	(terpri *debug-io*))
      (dbl-rpl-loop p-e-p))))

(defun do-break-level (at env p-e-p debug-level); break-level

  (unless
      (with-simple-restart
       (abort "Return to debug level ~D." debug-level)

       (catch-fatal 1)
       (setq *interrupt-enable* t)
       (cond (p-e-p
	      (format *debug-io* "~&~A~2%" at)
	      (set-current)
	      (setq *no-prompt* nil)
	      (show-restarts))
	     ((set-back at env)))

       (not (catch 'step-continue (dbl-rpl-loop p-e-p))))

    (terpri *debug-io*)
    (break-current)
    (do-break-level at env p-e-p debug-level)))


(defun break-level (at &optional env)
  (let* ((p-e-p (unless (listp at) t))
         (+ +) (++ ++) (+++ +++)
         (- -)
         (* *) (** **) (*** ***)
         (/ /) (// //) (/// ///)
	 (*quit-tags* (cons (cons *break-level* *quit-tag*) *quit-tags*))
	 *quit-tag*
	 (*break-level* (if p-e-p (cons t *break-level*) *break-level*))
	 (*ihs-base* (1+ *ihs-top*))
	 (*ihs-top* (ihs-top))
	 (*frs-base* (or (sch-frs-base *frs-top* *ihs-base*) (1+ (frs-top))))
	 (*frs-top*  (frs-top))
	 (*current-ihs* *ihs-top*)
	 (*debug-level* (1+ *debug-level*))
	 (*debug-restarts* (compute-restarts))
	 (*debug-abort* (find-restart 'abort))
	 (*debug-continue* (find-restart 'continue))
	 (*abort-restarts* (remove-if-not (lambda (x) (eq 'abort (restart-name x))) *debug-restarts*))
	 (*readtable* (or *break-readtable* *readtable*))
	 *break-env* *read-suppress*)
    
      (do-break-level at env p-e-p *debug-level*)))

(putprop 'break-level t 'compiler::cmp-notinline)

(defun break (&optional format-string &rest args &aux message (*sig-fn-name* (or *sig-fn-name* (get-sig-fn-name))))

  (let ((*print-pretty* nil)
	(*print-level* 4)
	(*print-length* 4)
	(*print-case* :upcase))
    (terpri *error-output*)
    (cond (format-string
	   (format *error-output* "~&Break: ")
	   (let ((*indent-formatted-output* t))
	     (apply 'format *error-output* format-string args))
	   (terpri *error-output*)
	   (setq message (apply 'format nil format-string args)))
	  (t (format *error-output* "~&Break.~%")
	     (setq message ""))))
  (with-simple-restart 
   (continue "Return from break.")
   (break-level message))
  nil)
(putprop 'break t 'compiler::cmp-notinline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_serror.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_mislib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; This file is IMPLEMENTATION-DEPENDENT.


;(in-package 'lisp)


;(export 'time)
;(export '(reset-sys-paths
;	  decode-universal-time
;	  encode-universal-time compile-file-pathname complement constantly))


(in-package :system)

(export '(funcallable-symbol-function));FIXME fsf

(defmacro time (form)
  (declare (optimize (safety 2)))
  (let ((real-start (gensym)) (real-end (gensym)) (gbc-time-start (gensym))
	(gbc-time (gensym)) (x (gensym)) (run-start (gensym)) (run-end (gensym))
	(child-run-start (gensym)) (child-run-end (gensym)))
  `(let (,real-start ,real-end (,gbc-time-start (si::gbc-time)) ,gbc-time ,x)
     (setq ,real-start (get-internal-real-time))
     (multiple-value-bind (,run-start ,child-run-start) (get-internal-run-times)
       (si::gbc-time 0)
       (setq ,x (multiple-value-list ,form))
       (setq ,gbc-time (si::gbc-time))
       (si::gbc-time (+ ,gbc-time-start ,gbc-time))
       (multiple-value-bind (,run-end ,child-run-end) (get-internal-run-times)
	 (setq ,real-end (get-internal-real-time))
	 (fresh-line *trace-output*)
	 (format *trace-output*
		 "real time       : ~10,3F secs~%~
                  run-gbc time    : ~10,3F secs~%~
                  child run time  : ~10,3F secs~%~
                  gbc time        : ~10,3F secs~%"
		 (/ (- ,real-end ,real-start) internal-time-units-per-second)
		 (/ (- (- ,run-end ,run-start) ,gbc-time) internal-time-units-per-second)
		 (/ (- ,child-run-end ,child-run-start) internal-time-units-per-second)
		 (/ ,gbc-time internal-time-units-per-second))))
       (values-list ,x))))

(defconstant seconds-per-day #.(* 24 3600))

(defun leap-year-p (y)
  (and (zerop (mod y 4))
       (or (not (zerop (mod y 100))) (zerop (mod y 400)))))

(defun number-of-days-from-1900 (y)
  (let ((y1 (1- y)))
    (+ (* (- y 1900) 365)
       (floor y1 4) (- (floor y1 100)) (floor y1 400)
       -460)))

(eval-when
 (compile eval)
 (defmacro mmd (n &optional lp 
		  &aux (l '(31 28 31 30 31 30 31 31 30 31 30 31))
		  (l (if lp (cons (pop l) (cons (1+ (pop l)) l)) l))(r 0)(s (mapcar (lambda (x) (incf r x)) l)))
  `(defconstant ,n (make-array ,(length s) :element-type '(integer ,(car s) ,(car (last s))) :initial-contents ',s))))
       
(mmd +md+)
(mmd +lmd+ t)

(defun decode-universal-time (ut &optional (tz (current-timezone) tzp) 
				 &aux (dstp (unless tzp (current-dstp))) (ut (- ut (* tz 3600))))
  (declare (optimize (safety 2)))
  (check-type ut (integer 0))
  (check-type tz rational)
  (multiple-value-bind
   (d ut) (floor ut seconds-per-day)
   (let* ((dow (mod d 7))(y (+ 1900 (floor d 366))))
     (labels ((l (y dd &aux (lyp (leap-year-p y))(td (if lyp 366 365))(x (- d dd)))
		 (if (< x td) (values (1+ x) y lyp) (l (1+ y) (+ dd td)))))
	     (multiple-value-bind
	      (d y lyp) (l y (number-of-days-from-1900 y))
	      (let* ((l (if lyp +lmd+ +md+))
		     (m (position d l :test '<=))
		     (d (if (> m 0) (- d (aref l (1- m))) d)))
		(multiple-value-bind
		 (h ut) (floor ut 3600)
		 (multiple-value-bind
		  (min sec) (floor ut 60)
		  (values sec min h d (1+ m) y dow dstp tz)))))))))

(defun encode-universal-time (sec min h d m y &optional (tz (current-timezone)))
  (declare (optimize (safety 2)))
  (check-type sec (integer 0 59))
  (check-type min (integer 0 59))
  (check-type h (integer 0 23))
  (check-type d (integer 1 31))
  (check-type m (integer 1 12))
  (check-type y (integer 1900))
  (check-type tz rational)
  (when (<= 0 y 99)
    (multiple-value-bind
     (sec min h d m y1 dow dstp tz) (get-decoded-time)
     (declare (ignore sec min h d m dow dstp tz))
     (incf y (- y1 (mod y1 100)))
     (cond ((< (- y y1) -50) (incf y 100))
	   ((>= (- y y1) 50) (decf y 100)))))
  (+ (* (+ (1- d) (number-of-days-from-1900 y) (if (> m 1) (aref (if (leap-year-p y) +lmd+ +md+) (- m 2)) 0))
        seconds-per-day)
     (* (+ h tz) 3600) (* min 60) sec))

(defun get-decoded-time ()
  (decode-universal-time (get-universal-time)))

;Courtesy Paul Dietz
(defun compile-file-pathname (pathname)
  (declare (optimize (safety 2)))
  (make-pathname :defaults pathname :type "o"))
(defun constantly (x)
  (declare (optimize (safety 2)))
  (lambda (&rest args)
    (declare (ignore args) (dynamic-extent args))
    x))
(defun complement (fn)
  (declare (optimize (safety 2)))
  (lambda (&rest args) (not (apply fn args))))

 (defun lisp-implementation-version nil
   (declare (optimize (safety 2)))
   (format nil "GCL ~a.~a.~a"
	   si::*gcl-major-version*
	   si::*gcl-minor-version*
	   si::*gcl-extra-version*))

(defun objlt (x y)
  (declare (object x y))
  (let ((x (si::address x)) (y (si::address y)))
    (declare (fixnum x y))
    (if (< y 0)
	(if (< x 0) (< x y) t)
      (if (< x 0) nil (< x y)))))

(defun heaprep nil
  
  (let ((f (list
	    "word size:            ~a bits~%"
	    "page size:            ~a bytes~%"
	    "heap start:           0x~x~%"
	    "heap max :            0x~x~%"
	    "shared library start: 0x~x~%"
	    "cstack start:         0x~x~%"
	    "cstack mark offset:   ~a bytes~%"
	    "cstack direction:     ~[downward~;upward~;~]~%"
	    "cstack alignment:     ~a bytes~%"
	    "cstack max:           ~a bytes~%"
	    "physical pages:       ~a~%"
	    "immfix start:         0x~x~%"
	    "immfix size:          ~a fixnums~%"))
	(v (multiple-value-list (si::heap-report))))
    
    (do ((v v (cdr v)) (f f (cdr f))) ((not (car v)))
	(format t (car f) 
		(let ((x (car v))) 
		  (cond ((>= x 0) x) 
			((+ x (* 2 (1+ most-positive-fixnum))))))))))

(defun room (&optional x)

  (let ((l (room-report));(multiple-value-list (si:room-report)))
        maxpage holepage leftpage ncbpage maxcbpage ncb cbgbccount npage
        rbused rbfree nrbpage rbgbccount maxrbpage maxnpage
        info-list link-alist)
    (setq maxpage (nth 0 l) leftpage (nth 1 l)
          ncbpage (nth 2 l) maxcbpage (nth 3 l) ncb (nth 4 l)
          cbgbccount (nth 5 l)
          holepage (nth 6 l)
          rbused (nth 7 l) rbfree (nth 8 l) nrbpage (nth 9 l)
	  maxrbpage (nth 10 l)
          rbgbccount (nth 11 l)
          l (nthcdr 12 l))
    (do ((l l (nthcdr 7 l))
         (j 0 (+ j (if (nth 3 l) (nth 3 l) 0)))
         (i 0 (+ i (if (nth 3 l) (nth 3 l) 0))))
        ((null l) (setq npage i maxnpage j))
      (let ((typename (intern (nth 0 l)))
            (nused (nth 1 l))
            (nfree (nth 2 l))
            (npage (nth 3 l))
            (maxpage (nth 4 l))
            (gbccount (nth 5 l))
            (ws (nth 6 l)))
        (if nused
            (push (list typename ws npage maxpage
                        (if (zerop (+ nused nfree))
                            0
                            (/ nused 0.01 (+ nused nfree)))
                        (if (zerop gbccount) nil gbccount))
                  info-list)
            (let* ((nfree (intern nfree))
		   (a (assoc nfree link-alist)))
                 (if a
                     (nconc a (list typename))
                     (push (list nfree typename)
                           link-alist))))))
    (terpri)
    (format t "~@[~2A~]~10@A/~A~21T~6@A%~@[~8@A~]~37T~{~A~^ ~}~%~%" "WS" "UP" "MP" "FI" "GC" '("TYPES"))
    (dolist (info (reverse info-list))
      (apply #'format t "~@[~2D~]~10D/~D~21T~6,1F%~@[~8D~]~37T~{~A~^ ~}"
             (append (cdr info)
                     (if  (assoc (car info) link-alist)
                          (list (assoc (car info) link-alist))
                          (list (list (car info))))))
      (terpri)
      )
    (terpri)
    (format t "~12D/~D~28T~@[~8D~]~37Tcontiguous (~D blocks)~%"
            ncbpage maxcbpage (if (zerop cbgbccount) nil cbgbccount) ncb)
    (format t "~13T~D~37Thole~%" holepage)
    (format t "~12D/~D~21T~6,1F%~@[~8D~]~37Trelocatable~%~%"
            nrbpage maxrbpage (/ rbused 0.01 (+ rbused rbfree))
            (if (zerop rbgbccount) nil rbgbccount))
    (format t "~12D pages for cells~%~%" npage)
    (format t "~12D total pages in core~%" (+ npage ncbpage nrbpage))
    (format t "~12D current core maximum pages~%" (+ maxnpage maxcbpage maxrbpage))
    (format t "~12D pages reserved for gc~%" maxrbpage)
    (format t "~12D pages available for adding to core~%" leftpage)
    (format t "~12D pages reserved for core exhaustion~%~%" (- maxpage (+ maxnpage maxcbpage (ash maxrbpage 1) leftpage)))
    (format t "~12D maximum pages~%" maxpage)
    (values)
    )
  (when x
    (format t "~%~%")
    (format t "Key:~%~%WS: words per struct~%UP: allocated pages~%MP: maximum pages~%FI: fraction of cells in use on allocated pages~%GC: number of gc triggers allocating this type~%~%")
    (heaprep))
  (values))


(defvar *call-stack* nil)
(defvar *prof-list* nil)
(defvar *profiling* nil)
(defun in-call (sym)
  (when *profiling*
    (push (cons sym (gettimeofday)) *call-stack*)))
(defun out-call (tm)
  (when *call-stack*
    (let* ((r (pop *call-stack*))
	   (tm (- tm (cdr r)))
	   (e (car (member (caar *call-stack*) (pushnew (list (caar *call-stack*)) *prof-list* :key 'car) :key 'car)))
	   (f (car (member (car r) (pushnew (list* (car r) 0 0) (cdr e) :key 'car) :key 'car))))
      (setf (cadr f) (+ tm (cadr f)) (cddr f) (1+ (cddr f))))))
(defun prof (v)
  (print-prof)
  (setq *call-stack* nil *prof-list* nil *profiling* v))
(defun print-prof nil
  (dolist (l *prof-list*)
    (setf (cdr l) (sort (cdr l) (lambda (x y) (> (cadr x) (cadr y))))))
  (setq *prof-list* (sort *prof-list* (lambda (x y) (> (reduce (lambda (y x) (+ y (cadr x))) (cdr x) :initial-value 0)
						       (reduce (lambda (y x) (+ y (cadr x))) (cdr y) :initial-value 0)))))
  (print *prof-list*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_mislib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_cp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :compiler)

(eval-when
 (compile) 

 (defun foo-reader (stream subchar)
   (declare (ignore subchar) (optimize (safety 2)))
   (let ((x (read-delimited-list #\} stream)))
     (let (zz z r) 
       (mapc (lambda (x) 
	       (cond ((member x '(|enum| |union| |struct| |unsigned|)) (setq zz x))
		     ((not z) (setq z (if zz (list zz x) x)))
		     ((integerp x) (setq r (cons (list z (cadar r) x) (cdr r))))
		     ((eq x '|;|) (setq z nil zz nil))
		     ((push (list z x) r)))) x) 
       (nreverse r))))
 
 (defun |;-reader| (stream subchar)
   (declare (ignore stream subchar) (optimize (safety 2)))
   '|;|)
 
 
 
 (defun readtable-h nil
   (setf (readtable-case *readtable*) :preserve)
   (set-macro-character #\{ 'foo-reader)
   (set-macro-character #\; '|;-reader|)
   (set-syntax-from-char #\# #\;)
   (set-syntax-from-char #\} #\))
   (dolist (l (coerce ":|,.()" 'list))
     (set-syntax-from-char l #\Space)))
 
 (defun get-com (f &aux x com td (*readtable* (copy-readtable)))
   
   (readtable-h)
   (with-input-from-string
    (s (si::file-to-string f))
    (do ((y nil x)(z nil y)) 
	((eq 'eof (setq x (read s nil 'eof))) 
	 (unless (and com td) (error "h read error"))
	 (list com td))
	(when (and (member z '(|struct| |union|)) (consp x)) 
	  (push (list z y x) com))
	(when (eq x '|typedef|) 
	  (push (read-delimited-list #\; s) td)))))
 
 (defun td (k l)
   (let* ((kn (when (symbolp k) (symbol-name k)))
	  (kk (when kn (find-symbol (string-upcase kn) 'keyword)))
	  (kk (when (get kk 'compiler::lisp-type) kk))
	  (x (car (member k l :key (lambda (x) (car (last x)))))))
     (cond (kk)
	   ((not x) k)
	   ((eq (car x) '|unsigned|) (cons (td (cadr x) l) (car x)))
	   ((not (cddr x)) (td (car x) l))
	   (x))))
 
 (defun slist nil
   
   (let* ((com (get-com "../h/cmpinclude.h"))
	  (td (cadr com))
	  (com (car com))
	  (u (car (member-if (lambda (x) (and (eq (car x) '|union|) (eq (cadr x) '|lispunion|))) com)))
	  (u (remove-if-not 'consp (caddr u) :key 'car)))
     (mapcar (lambda (x) 
	       (let ((y (car (member-if (lambda (z) 
					  (when (consp (car x))
					    (and  (eq (caar x) (car z)) (eq (cadar x) (cadr z))))) com)))) 
		 (list (car x) (cadr x)
		       (mapcar (lambda (z) (cons (td (car z) td) (cdr z))) (caddr y))))) u)))
 
 (defun ft (x)
   (let* ((x (symbol-name x))
	  (u (search "_" x))
	  (x (if u (subseq x 0 u) x))
	  (x (find-symbol (string-upcase x) 'compiler)))
     (unless (compiler::type>= (compiler::cmp-norm-tp x) '*) x)))
 
 (defun csd (b sn x)
   (let* ((z (car x))
	  (ct (consp z))
	  (y (when ct (cdr z)))
	  (z (if ct (car z) z)))
     (when (keywordp z)
       (unless (eq (cadr x) '|pad|)
	 (let* ((e (string (cadr x)))
		(s (when (eql #\* (aref e 0)) `(c::i)))
		(e (if s (subseq e 1) e))
		(es (search "_" e))
		(ee (if es (subseq e (1+ es)) e))
		(n (intern (string-upcase (compiler::strcat (symbol-name b) "-" ee)) 'c))
		(ns (intern (string-upcase (compiler::strcat "set-" n)) 'c))
		(zz (if (member z '(:integer :real :plist :pack :string :structure :keyword :direl :symbol)) :object z))
		(ss (concatenate 'string "->"  (string sn) "." (string e) (if s "[" "")))
		(f `(si::lit ,zz (:object c::x) ,ss ,@(when s `((:fixnum c::i) "]"))));FIXME
		(fp f)
		(i (third x))
		(tt1 (when i (if y `(integer 0 (,(ash 1 i)))
			       `(integer ,(- (ash 1 (1- i))) (,(ash 1 (1- i)))))))
		(tt1 (or tt1 (get z 'compiler::lisp-type)))
		(f (if tt1 `(the ,tt1 ,f) f))
		(fs (sublis
		     (list (cons fp (append `(si::lit ,zz "(") (cddr fp) `("=" (,zz c::y) ")")))) f))
		(v (member e '("FIXVAL" "LFVAL" "SFVAL" "CODE") :test 'equal))
		(fs (unless v fs));could be an immediate fixnum, unsettable
		(f (if v `c::x f)));use default coersion
	   `(progn (defun ,n (c::x ,@s) 
		     (declare (optimize (safety 1)))
		     ,@(when s `((declare (seqind c::i))))
		     (check-type c::x ,b)
		     ,f)
		   ,@(when fs
		       `((defun ,ns (c::y c::x ,@s) 
			   (declare (optimize (safety 1)) (ignorable c::y))
			   ,@(when s `((declare (seqind c::i))))
			   (check-type c::x ,b)
			   ,@(when tt1 `((check-type c::y ,tt1)))
			   (compiler::side-effects)
			   ,fs)))))))))
 
 (defun cs (def d)
   (let ((b (ft (cadar def))))
     (when b
       (let* ((sn (cadr def))
	      (def (caddr def))
;	      (def (set-difference def d :test 'equal));No early set-difference FIXME
	      (def (remove-if (lambda (x) (member x d :test 'equal)) def)))
	 `(progn
	    ,@(mapcar (lambda (x) (csd b sn x)) def))))))
 
 (defmacro bar nil
   `(progn
      ,@(let* ((s (slist))
	       (d (caddar (member '(|struct| |dummy|) s :test 'equal :key 'car))))
	  (remove-if-not 'identity (mapcar (lambda (x) (cs x d)) s))))))
 
(bar)

(in-package :compiler)

(defun c::hashtable-self (x i)
  (declare (optimize (safety 1)))
  (declare (seqind i))
  (check-type x hash-table)
  (lit :fixnum "(fixnum)(" (:object x)  "->ht.ht_self+" (:fixnum i) ")"))

(defun c::gethash-int (x y)
  (declare (optimize (safety 1)))
  (check-type y hash-table)
  (lit :fixnum "(fixnum)gethash(" (:object x) "," (:object y) ")"))

(defun c::sxhash-int (x)
  (declare (optimize (safety 1)))
  (lit :fixnum "ihash_equal1(" (:object x) ",0)"))

(defun c::close-int (x)
  (declare (optimize (safety 1)))
  (check-type x stream)
  (lit :fixnum "(close_stream(" (:object x) "),1)"))

(defun c::read-object-non-recursive (x)
  (declare (optimize (safety 1)))
  (check-type x stream)
  (lit :object "read_object_non_recursive(" (:object x) ")"))

(defun c::read-object-recursive (x)
  (declare (optimize (safety 1)))
  (check-type x stream)
  (lit :object "read_object_non_recursive(" (:object x) ")"))

(defun c::htent-value (x)
  (declare (optimize (safety 1)))
  (check-type x fixnum)
  (lit :object "((struct htent *)" (:fixnum x) ")->hte_value"))

(defun c::htent-key (x)
  (declare (optimize (safety 1)))
  (check-type x fixnum)
  (lit :object "((struct htent *)" (:fixnum x) ")->hte_key"))

(defun c::set-htent-value (y x)
  (declare (optimize (safety 1)))
  (check-type x fixnum)
  (lit :object "((struct htent *)" (:fixnum x) ")->hte_value=" (:object y)))

(defun c::set-htent-key (y x)
  (declare (optimize (safety 1)))
  (check-type x fixnum)
  (lit :object "((struct htent *)" (:fixnum x) ")->hte_key=" (:object y)))

(defun c::make-string-output-stream nil
  (declare (optimize (safety 1)))
  (lit :object "make_string_output_stream(64)"))

(defun funcallable-symbol-p (s)
  (and (symbolp s)
       (/= (si::address (c-symbol-gfdef s)) 0)
       (= (c-symbol-mflag s) 0)
       (= (c-symbol-sfdef s) (si::address nil))))
(setf (get 'funcallable-symbol-p 'cmp-inline) t)

(defun fsf (s)
  (declare (optimize (safety 1)))
;  (check-type s funcallable-symbol); FIXME
  (assert (funcallable-symbol-p s))
  (the function (c-symbol-gfdef s)));FIXME
(setf (get 'fsf 'cmp-inline) t)

(defun tt3 (x) (lit :fixnum "fto(" (:object x) ")"))
(si::putprop 'tt3 t 'cmp-inline)
(defun tt30 (x) (lit :boolean "!fto0(" (:object x) ")"))
(si::putprop 'tt30 t 'cmp-inline)

(defun fn-env (x) ;FIXME expose pointers above, rename function type to type-spec
  (declare (optimize (safety 1)))
  (typecase x
   (compiled-function (c::function-env x 0))))

(eval-when
 (compile)
 (defmacro baz (&aux res)
   `(progn
      ,@(mapcan (lambda (l &aux (s (intern (cadar l) 'c)) (w (cddr l)) (k1 (cadr l)) (k2 (car (last l))))
		  `((defun ,s (,@(when w `(x)) z)
		      (declare (optimize (safety 1)))
		      ,@(when w `((check-type x ,(export-type (get k1 'lisp-type)))))
		      (check-type z ,(export-type (get k2 'lisp-type)))
		      (lit ,(caar l) ,@(when w `((,k1 x))) ,(cadar l) (,k2 z)))
		    (export ',s :c)))
		'(((:fixnum   "&")  :fixnum :fixnum)
		  ((:fixnum  "\|")  :fixnum :fixnum)
		  ((:fixnum   "^")  :fixnum :fixnum)
		  ((:fixnum  "<<")  :fixnum :fixnum)
		  ((:fixnum  ">>")  :fixnum :fixnum)
		  ((:fixnum   "~")  :fixnum)
		  ((:boolean "==")  :cnum :cnum)
		  ((:boolean "!=")  :cnum :cnum)
		  ((:boolean ">=")  :creal :creal);FIXME creal, immfix, obj
		  ((:boolean "<=")  :creal :creal)
		  ((:boolean  ">")  :creal :creal)
		  ((:boolean  "<")  :creal :creal))))))

(baz)

(setf (symbol-function 'si::package-internal) (symbol-function 'c::package-internal)
      (symbol-function 'si::package-internal_size) (symbol-function 'c::package-internal_size)
      (symbol-function 'si::package-external) (symbol-function 'c::package-external)
      (symbol-function 'si::package-external_size) (symbol-function 'c::package-external_size));FIXME

(setf (symbol-function 'array-rank) (symbol-function 'c::array-rank)
      (symbol-function 'array-total-size) (symbol-function 'c::array-dim)
      (symbol-function 'si::array-hasfillp) (symbol-function 'c::array-hasfillp)
      (symbol-function 'si::array-offset) (symbol-function 'c::array-offset)
      (symbol-function 'si::array-dims) (symbol-function 'c::array-dims)
      (symbol-function 'si::array-elttype) (symbol-function 'c::array-elttype)
      (symbol-function 'si::array-eltsize) (symbol-function 'c::array-eltsize)
      (symbol-function 'si::array-mode) (symbol-function 'c::array-mode)
      (symbol-function 'si::vector-dim) (symbol-function 'c::vector-dim))

(defun c::set-d-tt (i o);FIXME automate
  (declare (optimize (safety 1)))
  (check-type i (mod 16))
  (side-effects)
  (lit :fixnum (:object o) "->d.tt=" (:fixnum i)))

(in-package :si)

(eval-when 
 (compile)

 (defmacro make-ref (&optional r &aux (fn (if r `rref `ref)) (ik (if +sfix+ :fixnum :int))(dk (unless +sfix+ :fixnum)))
   (labels ((l1 (s k u &optional v)
		`(lit ,k ,(strcat "((u" (format nil "~s" (* 8 s)) "*)") 
		      (,(if r :object :fixnum) a) ,(if r "->a.a_self" "") ")[" (:fixnum i) "]." 
		      ,(ecase u (0 "i")((1 4) "u")(2 "f")(3 "c")(5 "o")) ,@(when v `("=" (,k v)))))
	    (l2 (u s k) `(if vp ,(l1 s k u t) ,(l1 s k u)))
	    (l3 (s k fk &optional tp)
		`(ecase 
		  u
		  ,@(when k `((0 ,(l2 0 s k)) 
			      ,@(unless (eq k :fixnum) `((1 (the ,tp ,(l2 1 s k)))))))
		  ,@(when fk `((2 ,(l2 2 s fk))))
		  ,@(when (eq k :char) `((4 (let ((v (if vp (char-code v) #\Space))) (code-char ,(l2 4 s k))))))
		  ,@(when (eq k :fixnum) `((5 ,(l2 5 s :object)))))))
	   `(progn
	      (defun ,fn (a i s u &optional (v nil vp))
		(declare (optimize (safety 1)))
		(check-type a array)
		(check-type i seqind)
		(check-type s seqind)
		(check-type u seqind)
		(compiler::side-effects);FIXME
		(ecase 
		 s
		 (1 ,(l3 1 :char  nil 'unsigned-char))
		 (2 ,(l3 2 :short nil 'unsigned-short))
		 (4 ,(l3 4 ik  :float))
		 (8 ,(l3 8 dk :double))))
	      (putprop ',fn t 'compiler::cmp-inline)))))

(make-ref)
(make-ref t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_cp.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_fpe_test.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#.`(defun test-fpe (f a r &optional chk &aux cc (o (mapcan (lambda (x) (list x t)) (break-on-floating-point-exceptions))))
     (flet ((set-break (x) (when (keywordp r)
			     (apply 'break-on-floating-point-exceptions (append (unless x o) (list r x))))))
       (let* ((rr (handler-case (unwind-protect (progn (set-break t) (apply f a)) (set-break nil))
				,@(mapcar (lambda (x &aux (x (car x))) `(,x (c) (setq cc c) ,(intern (symbol-name x) :keyword)))
					  (append si::+fe-list+ '((arithmetic-error)(error)))))))
	 (print (list* f a r rr (when cc (list cc (arithmetic-error-operation cc) (arithmetic-error-operands cc)))))
	 (assert (eql r rr))
	 (when (and chk cc)
	   (unless (eq 'fnop (cadr (member :op (arithmetic-error-operation cc))))
	     (assert (eq (symbol-function f) (cadr (member :fun (arithmetic-error-operation cc)))))
	     (assert (or (every 'equalp (mapcar (lambda (x) (if (numberp x) x (coerce x 'list))) a)
				(arithmetic-error-operands cc))
			 (every 'equalp (nreverse (mapcar (lambda (x) (if (numberp x) x (coerce x 'list))) a))
				(arithmetic-error-operands cc)))))))))

#+(or x86_64 i386)
(progn
  (eval-when
      (compile eval)
    (defmacro deft (n rt args &rest code)
      `(progn
	 (clines ,(nstring-downcase 
		   (apply 'concatenate 'string
			  (symbol-name rt) " " (symbol-name n) "("
			  (apply 'concatenate 'string 
				 (mapcon (lambda (x) (list* (symbol-name (caar x)) " " (symbol-name (cadar x)) 
							    (when (cdr x) (list ", ")))) args)) ") " code)))
	 (defentry ,n ,(mapcar 'car args) (,rt ,(string-downcase (symbol-name n)))))))
  
  (deft fdivp object ((object x) (object y))
    "{volatile double a=lf(x),b=lf(y),c;"
    "__asm__ __volatile__ (\"fldl %1;fldl %0;fdivp %%st,%%st(1);fstpl %2;fwait\" "
    ": \"=m\" (a), \"=m\" (b) : \"m\" (c));"
    "return make_longfloat(c);}")
  
  (deft divpd object ((object x) (object y) (object z))
    "{__asm__ __volatile__ (\"movapd %0,%%xmm0;movapd %1,%%xmm1;divpd %%xmm0,%%xmm1;movapd %%xmm1,%2\" "
    ": \"=m\" (*(char *)x->a.a_self), \"=m\" (*(char *)y->a.a_self) : \"m\" (*(char *)z->a.a_self));"
    "return z;}")
  
  (deft divpdm object ((object x) (object y) (object z))
    "{__asm__ __volatile__ (\"movapd %1,%%xmm1;divpd %0,%%xmm1;movapd %%xmm1,%2\" "
    ": \"=m\" (*(char *)x->a.a_self), \"=m\" (*(char *)y->a.a_self) : \"m\" (*(char *)z->a.a_self));"
    "return z;}")
  
  (deft divps object ((object x) (object y) (object z))
    "{__asm__ __volatile__ (\"movaps %0,%%xmm0;movaps %1,%%xmm1;divps %%xmm0,%%xmm1;movaps %%xmm1,%2\" "
    ": \"=m\" (*(char *)x->a.a_self), \"=m\" (*(char *)y->a.a_self) : \"m\" (*(char *)z->a.a_self));"
    "return z;}")
  
  (deft divpsm object ((object x) (object y) (object z))
    "{__asm__ __volatile__ (\"movaps %1,%%xmm1;divps %0,%%xmm1;movaps %%xmm1,%2\" "
    ": \"=m\" (*(char *)x->a.a_self), \"=m\" (*(char *)y->a.a_self) : \"m\" (*(char *)z->a.a_self));"
    "return z;}")
  
  (deft divsd object ((object x) (object y))
    "{volatile double a=lf(x),b=lf(y),c;"
    "__asm__ __volatile__ (\"movsd %0,%%xmm0;movsd %1,%%xmm1;divsd %%xmm1,%%xmm0;movsd %%xmm0,%2\" "
    ": \"=m\" (a), \"=m\" (b) : \"m\" (c));"
    "return make_longfloat(c);}")
  
  (deft divsdm object ((object x) (object y))
    "{volatile double a=lf(x),b=lf(y),c;"
    "__asm__ __volatile__ (\"movsd %0,%%xmm0;divsd %1,%%xmm0;movsd %%xmm0,%2\" "
    ": \"=m\" (a), \"=m\" (b) : \"m\" (c));"
    "return make_longfloat(c);}")
  
  (deft divss object ((object x) (object y))
    "{volatile float a=sf(x),b=sf(y),c;"
    "__asm__ __volatile__ (\"movss %0,%%xmm0;movss %1,%%xmm1;divss %%xmm1,%%xmm0;movss %%xmm0,%2\" "
    ": \"=m\" (a), \"=m\" (b) : \"m\" (c));"
    "return make_shortfloat(c);}")
  
  (deft divssm object ((object x) (object y))
    "{volatile float a=sf(x),b=sf(y),c;"
    "__asm__ __volatile__ (\"movss %0,%%xmm0;divss %1,%%xmm0;movss %%xmm0,%2\" "
    ": \"=m\" (a), \"=m\" (b) : \"m\" (c));"
    "return make_shortfloat(c);}")
  
  (deft sqrtpd object ((object x) (object y) (object z))
    "{__asm__ __volatile__ (\"movapd %0,%%xmm0;movapd %1,%%xmm1;sqrtpd %%xmm0,%%xmm1;movapd %%xmm1,%2\" "
    ": \"=m\" (*(char *)x->a.a_self), \"=m\" (*(char *)y->a.a_self) : \"m\" (*(char *)z->a.a_self));"
    "return z;}")
  
  (eval-when
      (compile load eval)
    (deft c_array_self fixnum ((object x)) "{return (fixnum)x->a.a_self;}")
    (defun c-array-eltsize (x) (ecase (array-element-type x) (short-float 4) (long-float 8)))
    (defun make-aligned-array (alignment size &rest r
					 &aux (ic (member :initial-contents r)) y
					 (c (cadr ic))
					 (r (append (ldiff r ic) (cddr ic)))
					 (a (apply 'make-array (+ alignment size) (list* :static t r))))
      (setq y (map-into
	       (apply 'make-array size
		      :displaced-to a
		      :displaced-index-offset (truncate (- alignment (mod (c_array_self a) alignment)) (c-array-eltsize a))
		      r)
	       'identity c))
      (assert (zerop (mod (c_array_self y) 16)))
      y))
  
  (setq fa (make-aligned-array 16 4 :element-type 'short-float :initial-contents '(1.2s0 2.3s0 3.4s0 4.1s0))
	fb (make-aligned-array 16 4 :element-type 'short-float)
	fc (make-aligned-array 16 4 :element-type 'short-float :initial-contents '(1.3s0 2.4s0 3.5s0 4.6s0))
	fx (make-aligned-array 16 4 :element-type 'short-float :initial-contents (make-list 4 :initial-element most-positive-short-float))
	fm (make-aligned-array 16 4 :element-type 'short-float :initial-contents (make-list 4 :initial-element least-positive-normalized-short-float))
	fn (make-aligned-array 16 4 :element-type 'short-float :initial-contents (make-list 4 :initial-element -1.0s0))
	fr (make-aligned-array 16 4 :element-type 'short-float))
  
  (setq da (make-aligned-array 16 2 :element-type 'long-float :initial-contents '(1.2 2.3))
	db (make-aligned-array 16 2 :element-type 'long-float)
	dc (make-aligned-array 16 2 :element-type 'long-float :initial-contents '(1.3 2.4))
	dx (make-aligned-array 16 2 :element-type 'long-float :initial-contents (make-list 2 :initial-element most-positive-long-float))
	dm (make-aligned-array 16 2 :element-type 'long-float :initial-contents (make-list 2 :initial-element least-positive-normalized-long-float))
	dn (make-aligned-array 16 2 :element-type 'long-float :initial-contents (make-list 2 :initial-element -1.0))
	dr (make-aligned-array 16 2 :element-type 'long-float))
  
  (test-fpe 'fdivp (list 1.0 2.0) 0.5 t)
  (test-fpe 'fdivp (list 1.0 0.0) :division-by-zero t)
  (test-fpe 'fdivp (list 0.0 0.0) :floating-point-invalid-operation t)
  (test-fpe 'fdivp (list most-positive-long-float least-positive-normalized-long-float) :floating-point-overflow);fstpl
  (test-fpe 'fdivp (list least-positive-normalized-long-float most-positive-long-float) :floating-point-underflow);fstpl
  (test-fpe 'fdivp (list 1.2 1.3) :floating-point-inexact);post args
  
  (test-fpe 'divpd (list da da dr) dr t)
  (test-fpe 'divpd (list db da dr) :division-by-zero t)
  (test-fpe 'divpd (list db db dr) :floating-point-invalid-operation t)
  (test-fpe 'divpd (list dm dx dr) :floating-point-overflow t)
  (test-fpe 'divpd (list dx dm dr) :floating-point-underflow t)
  (test-fpe 'divpd (list da dc dr) :floating-point-inexact t)
  
  (test-fpe 'divpdm (list da da dr) dr t)
  (test-fpe 'divpdm (list db da dr) :division-by-zero t)
  (test-fpe 'divpdm (list db db dr) :floating-point-invalid-operation t)
  (test-fpe 'divpdm (list dm dx dr) :floating-point-overflow t)
  (test-fpe 'divpdm (list dx dm dr) :floating-point-underflow t)
  (test-fpe 'divpdm (list da dc dr) :floating-point-inexact t)
  
  
  (test-fpe 'divps (list fa fa fr) fr t)
  (test-fpe 'divps (list fb fa fr) :division-by-zero t)
  (test-fpe 'divps (list fb fb fr) :floating-point-invalid-operation t)
  (test-fpe 'divps (list fm fx fr) :floating-point-overflow t)
  (test-fpe 'divps (list fx fm fr) :floating-point-underflow t)
  (test-fpe 'divps (list fa fc fr) :floating-point-inexact t)
  
  (test-fpe 'divpsm (list fa fa fr) fr t)
  (test-fpe 'divpsm (list fb fa fr) :division-by-zero t)
  (test-fpe 'divpsm (list fb fb fr) :floating-point-invalid-operation t)
  (test-fpe 'divpsm (list fm fx fr) :floating-point-overflow t)
  (test-fpe 'divpsm (list fx fm fr) :floating-point-underflow t)
  (test-fpe 'divpsm (list fa fc fr) :floating-point-inexact t)
  
  
  
  (test-fpe 'divsd (list 1.0 2.0) 0.5 t)
  (test-fpe 'divsd (list 1.0 0.0) :division-by-zero t)
  (test-fpe 'divsd (list 0.0 0.0) :floating-point-invalid-operation t)
  (test-fpe 'divsd (list most-positive-long-float least-positive-normalized-long-float) :floating-point-overflow t)
  (test-fpe 'divsd (list least-positive-normalized-long-float most-positive-long-float) :floating-point-underflow t)
  (test-fpe 'divsd (list 1.2 2.3) :floating-point-inexact t)
  
  (test-fpe 'divsdm (list 1.0 2.0) 0.5 t)
  (test-fpe 'divsdm (list 1.0 0.0) :division-by-zero t)
  (test-fpe 'divsdm (list 0.0 0.0) :floating-point-invalid-operation t)
  (test-fpe 'divsdm (list most-positive-long-float least-positive-normalized-long-float) :floating-point-overflow t)
  (test-fpe 'divsdm (list least-positive-normalized-long-float most-positive-long-float) :floating-point-underflow t)
  (test-fpe 'divsdm (list 1.2 2.3) :floating-point-inexact t)
  
  (test-fpe 'divss (list 1.0s0 2.0s0) 0.5s0 t)
  (test-fpe 'divss (list 1.0s0 0.0s0) :division-by-zero t)
  (test-fpe 'divss (list 0.0s0 0.0s0) :floating-point-invalid-operation t)
  (test-fpe 'divss (list most-positive-short-float least-positive-normalized-short-float) :floating-point-overflow t)
  (test-fpe 'divss (list least-positive-normalized-short-float most-positive-short-float) :floating-point-underflow t)
  (test-fpe 'divss (list 1.2s0 2.3s0) :floating-point-inexact t)
  
  (test-fpe 'divssm (list 1.0s0 2.0s0) 0.5s0 t)
  (test-fpe 'divssm (list 1.0s0 0.0s0) :division-by-zero t)
  (test-fpe 'divssm (list 0.0s0 0.0s0) :floating-point-invalid-operation t)
  (test-fpe 'divssm (list most-positive-short-float least-positive-normalized-short-float) :floating-point-overflow t)
  (test-fpe 'divssm (list least-positive-normalized-short-float most-positive-short-float) :floating-point-underflow t)
  (test-fpe 'divssm (list 1.2s0 2.3s0) :floating-point-inexact t)
  
  (test-fpe 'sqrtpd (list da db dr) dr t)
  (test-fpe 'sqrtpd (list dn db dr) :floating-point-invalid-operation t)
  (test-fpe 'sqrtpd (list da db dr) :floating-point-inexact t))


(defun l/ (x y) (declare (long-float x y)) (/ x y))
(defun s/ (x y) (declare (short-float x y)) (/ x y))
(defun lsqrt (x) (declare (long-float x)) (the long-float (sqrt x)))


(test-fpe 'l/ (list 1.0 2.0) 0.5 t)
(test-fpe 'l/ (list 1.0 0.0) :division-by-zero t)
(test-fpe 'l/ (list 0.0 0.0) :floating-point-invalid-operation t)
(test-fpe 'l/ (list most-positive-long-float least-positive-normalized-long-float) :floating-point-overflow t)
(test-fpe 'l/ (list least-positive-normalized-long-float most-positive-long-float) :floating-point-underflow t)
(test-fpe 'l/ (list 1.2 1.3) :floating-point-inexact t)

(test-fpe 's/ (list 1.0s0 2.0s0) 0.5s0 t)
(test-fpe 's/ (list 1.0s0 0.0s0) :division-by-zero t)
(test-fpe 's/ (list 0.0s0 0.0s0) :floating-point-invalid-operation t)
(test-fpe 's/ (list most-positive-short-float least-positive-normalized-short-float) :floating-point-overflow t)
(test-fpe 's/ (list least-positive-normalized-short-float most-positive-short-float) :floating-point-underflow t)
(test-fpe 's/ (list 1.2s0 1.3s0) :floating-point-inexact t)

(test-fpe 'lsqrt (list 4.0) 2.0 t)
(test-fpe 'lsqrt (list -1.0) :floating-point-invalid-operation t)
(test-fpe 'lsqrt (list 1.2) :floating-point-inexact t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_fpe_test.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_defmacro.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;    defmacro.lsp
;;;;
;;;;         defines SI:DEFMACRO*, the defmacro preprocessor


;; (in-package :lisp)
;; (export '(lambda defvar import &whole &environment &body))


(in-package :system)


;;; valid lambda-list to DEFMACRO is:
;;;
;;;	( [ &whole sym ]
;;;	  [ &environment sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { defmacro-lambda-list | sym }.
;;; A symbol may be accepted as a DEFMACRO lambda-list, in which case
;;; (DEFMACRO <name> <symbol> ... ) is equivalent to
;;; (DEFMACRO <name> (&REST <symbol>) ...).
;;; Defamcro-lambda-list is defined as:
;;;
;;;	( { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )

;; defvar is not yet available.
(mapc '*make-special '(*dl* *key-check* *arg-check*))


(defun get-&environment(vl &aux env)
  (let ((env-m
	 (and (listp vl)
	      (do ((tail vl (cdr tail)))
		  ((not (consp tail)) nil)
		(when (eq '&environment (car tail))
		  (return tail))))))
    (cond (env-m
 	   (setq env (cadr env-m))
 	   (setq vl (append (ldiff vl env-m) (cddr env-m)))))
    (values vl env)))



(defun gensym (&optional (x nil xp))
  (cond ((not xp) (gensym0))
	((stringp x) (gensym1s x))
	((gensym1ig x))))
			 
(export '(blocked-body-name parse-body-header))

(defun parse-body-header (x &optional doc decl ctps &aux (a (car x)))
  (cond 
   ((unless (or doc ctps) (and (stringp a) (cdr x))) (parse-body-header (cdr x) a decl ctps))
   ((unless ctps (when (consp a) (eq (car a) 'declare)))  (parse-body-header (cdr x) doc (cons a decl) ctps))
   ((when (consp a) (member (car a) '(check-type assert))) (parse-body-header (cdr x) doc decl (cons a ctps)))
   (t (values doc (nreverse decl) (nreverse ctps) x))))

(defun make-blocked-lambda (ll decls ctps body block)
  (let ((body (if (eq block (blocked-body-name body)) body `((block ,block ,@body)))))
    `(lambda ,ll ,@decls ,@ctps ,@body)))

(defun blocked-body-name (body)
  (when (and (not (cdr body))
	     (consp (car body))
	     (eq (caar body) 'block))
    (cadar body)))


(defun defmacro-lambda (name vl body &aux whole)

  (cond ((listp vl))
        ((symbolp vl) (setq vl (list '&rest vl)))
        ((error "The defmacro-lambda-list ~s is not a list." vl)))
  
  (cond ((and (listp vl) (eq (car vl) '&whole))
	 (setq whole (cadr vl)) (setq vl (cddr vl)))
	((setq whole (gensym))))  
  
  (multiple-value-bind
   (doc decls ctps body)
   (parse-body-header body)

   (declare (ignore doc))
   
   (multiple-value-bind
    (vl env)
    (get-&environment vl)
    
    (let* ((envp env)
	   (env (or env (gensym)))
	   (*dl* `(&aux ,env ,whole))
	   *key-check* *arg-check*
	   (ppn (dm-vl vl whole t)))

      (declare (ignore ppn))
      
      (dolist (kc *key-check*)
	(push `(unless (getf ,(car kc) :allow-other-keys);FIXME order?
		 (do ((vl ,(car kc) (cddr vl)))
		     ((endp vl))
		     (unless (member (car vl) ',(cons :allow-other-keys (cdr kc)))
		       (dm-key-not-allowed (car vl)))))
	      body))
      
      (dolist (ac *arg-check*)
	(push `(when ,(dm-nth-cdr (cdr ac) (car ac)) (dm-too-many-arguments)) body))

      (unless envp (push `(declare (ignore ,env)) decls))

      (make-blocked-lambda (nreverse *dl*) decls ctps body name)))))

;; (defun si:defmacro* (name vl body
;;                           &aux *dl* (*key-check* nil)
;; 			  (*arg-check* nil)
;; 			  doc decls whole ppn (env nil) envp)
;;   (cond ((listp vl))
;;         ((symbolp vl) (setq vl (list '&rest vl)))
;;         (t (error "The defmacro-lambda-list ~s is not a list." vl)))
;;   (multiple-value-setq (doc decls body) (find-doc body nil))
;;   (cond ((and (listp vl) (eq (car vl) '&whole))
;;          (setq whole (cadr vl)) (setq vl (cddr vl)))
;;         (t (setq whole (gensym))))
;;   (multiple-value-setq (vl env)
;; 		       (get-&environment vl))
;;   (setq envp env)
;;   (or env (setq env (gensym)))
;;   (setq *dl* `(&aux ,env ,whole))
;;   (setq ppn (dm-vl vl whole t))
;;   (dolist (kc *key-check*)
;;     (push `(unless (getf ,(car kc) :allow-other-keys);FIXME order?
;; 	     (do ((vl ,(car kc) (cddr vl)))
;; 		 ((endp vl))
;; 		 (unless (member (car vl) ',(cons :allow-other-keys (cdr kc)))
;; 		   (dm-key-not-allowed (car vl)))))
;; 	  body))
;;   (dolist (ac *arg-check*)
;;     (push `(when ,(dm-nth-cdr (cdr ac) (car ac))
;; 	     (dm-too-many-arguments)) body))
;;   (unless envp (push `(declare (ignore ,env)) decls))
;; ;  (list doc ppn `(lambda-block ,name ,(reverse *dl*) ,@(append decls body)))
;;   (list doc ppn (eval `(function (lambda ,(reverse *dl*) ,@decls (block ,name ,@body)))))
;; ;  (list doc ppn (let ((nn (gensym))) (eval `(defun ,nn ,(reverse *dl*) ,@decls (block ,name ,@body))) (symbol-function nn)))
;;   )

(defun dm-vl (vl whole top)
  (when (consp whole)
    (let ((n (gensym)))
      (setq *dl* (subst n whole *dl*))
      (dm-vl whole n nil)
      (setq whole n)))


 (do ((optionalp nil) (restp nil) (keyp nil)
       (allow-other-keys-p nil) (auxp nil)
       (rest nil) (allow-other-keys nil) (keys nil) (no-check nil)
       (n (if top 1 0)) (ppn nil)
       )
      ((not (consp vl))
       (when vl
         (when restp (dm-bad-key '&rest))
         (push (list vl (dm-nth-cdr n whole)) *dl*)
         (setq no-check t))
       (when (and rest (not allow-other-keys))
         (push (cons rest keys) *key-check*))
       (unless no-check (push (cons whole n) *arg-check*))
       ppn
       )
    (let ((v (car vl)))
      (cond
       ((eq v '&optional)
        (when optionalp (dm-bad-key '&optional))
        (setq optionalp t)
        (pop vl))
       ((or (eq v '&rest) (eq v '&body))
        (when restp (dm-bad-key v))
        (dm-v (cadr vl) (dm-nth-cdr n whole))
        (setq restp t optionalp t no-check t)
        (setq vl (cddr vl))
        (when (eq v '&body) (setq ppn (if top (1- n) n))))
       ((eq v '&key)
        (when keyp (dm-bad-key '&key))
        (setq rest (gensym))
        (push (list rest (dm-nth-cdr n whole)) *dl*)
        (setq keyp t restp t optionalp t no-check t)
        (pop vl))
       ((eq v '&allow-other-keys)
        (when (or (not keyp) allow-other-keys-p)
              (dm-bad-key '&allow-other-keys))
        (setq allow-other-keys-p t)
        (setq allow-other-keys t)
        (pop vl))
       ((eq v '&aux)
        (when auxp (dm-bad-key '&aux))
        (setq auxp t allow-other-keys-p t keyp t restp t optionalp t)
        (pop vl))
       (auxp
        (let (x (init nil))
             (cond ((symbolp v) (setq x v))
                   (t (setq x (car v))
                      (unless (endp (cdr v)) (setq init (cadr v)))))
             (dm-v x init))
        (pop vl))
       (keyp
        (let ((temp (gensym)) x k (init nil) (sv nil))
             (cond ((symbolp v) (setq x v k (intern (string v) 'keyword)))
                   (t (if (symbolp (car v))
                          (setq x (car v)
                                k (intern (string (car v)) 'keyword))
                          (setq x (cadar v) k (caar v)))
                      (unless (endp (cdr v))
                              (setq init (cadr v))
                              (unless (endp (cddr v))
                                      (setq sv (caddr v))))))
             (dm-v temp `(getf ,rest ,k 'failed))
             (dm-v x `(if (eq ,temp 'failed) ,init ,temp))
             (when sv (dm-v sv `(not (eq ,temp 'failed))))
             (push k keys))
        (pop vl))
       (optionalp
        (let (x (init nil) (sv nil))
             (cond ((symbolp v) (setq x v))
                   (t (setq x (car v))
                      (unless (endp (cdr v))
                              (setq init (cadr v))
                              (unless (endp (cddr v))
                                      (setq sv (caddr v))))))
             (dm-v x `(if ,(dm-nth-cdr n whole) ,(dm-nth n whole) ,init))
             (when sv (dm-v sv `(not (null ,(dm-nth-cdr n whole))))))
        (incf n)
        (pop vl)
        )
       (t (dm-v (or v (gensym)) `(if ,(dm-nth-cdr n whole)
                       ,(dm-nth n whole)
                       (dm-too-few-arguments)))
          (incf n)
          (pop vl))
       ))))

(defun dm-v (v init)
       (cond  ((symbolp v) (push (if init (list v init) v) *dl*))
	      ((consp v) (let* ((w (eq (car v) '&whole))
				(temp (if w (cadr v) (gensym)))
				(v (if w (cddr v) v)))
			   (push (if init (list temp init) temp) *dl*)
			   (dm-vl v temp nil)))
	      (t (error "Bad defmacro argument: ~s" v))))

(defun dm-nth (n v)
  (multiple-value-bind (q r) (floor n 4)
     (dotimes (i q) (setq v (list 'cddddr v)))
     (case r
        (0 (list 'car v))
        (1 (list 'cadr v))
        (2 (list 'caddr v))
        (3 (list 'cadddr v))
        )))

(defun dm-nth-cdr (n v)
  (multiple-value-bind (q r) (floor n 4)
     (dotimes (i q) (setq v (list 'cddddr v)))
     (case r
        (0 v)
        (1 (list 'cdr v))
        (2 (list 'cddr v))
        (3 (list 'cdddr v))
        )))

(defun dm-bad-key (key)
       (error 'program-error :format-control "Defmacro-lambda-list contains illegal use of ~s." :format-arguments (list key)))

(defun dm-too-few-arguments ()
       (error 'program-error :format-control "Too few arguments are supplied to defmacro-lambda-list."))

(defun dm-too-many-arguments ()
       (error 'program-error :format-control "Too many arguments are supplied to defmacro-lambda-list."))

(defun dm-key-not-allowed (key)
       (error 'program-error :format-control "The key ~s is not allowed." :format-arguments (list key)))

(defun find-declarations (body)
  (if (endp body)
      (values nil nil)
      (let ((d (macroexpand (car body))))
        (cond ((stringp d)
               (if (endp (cdr body))
                   (values nil (list d))
                   (multiple-value-bind (ds b)
                       (find-declarations (cdr body))
                     (values (cons d ds) b))))
              ((and (consp d) (eq (car d) 'declare))
               (multiple-value-bind (ds b)
                   (find-declarations (cdr body))
                 (values (cons d ds) b)))
              (t
               (values nil (cons d (cdr body))))))))

(defmacro symbol-to-function (sym)
  (let* ((n (gensym))
	 (gf (find-symbol "C-SYMBOL-GFDEF" (find-package :s))))
    `(when (symbolp ,sym)
       ,(if (fboundp gf) `(let ((,n (address (,gf ,sym))))
			    (unless (= +objnull+ ,n) (nani ,n)))
	  `(let* ((,n (when (fboundp ,sym) (symbol-function ,sym)))
		  (,n (if (and (consp ,n) (eq (car ,n) 'macro)) (cdr ,n) ,n)))
	     (unless (consp ,n) ,n))))))

(defmacro call (sym &optional f &rest keys) ;FIXME macro
  (let* ((fnf (gensym))(n (gensym)))
    `(let* ((,fnf (if (functionp ,sym) ,sym (symbol-to-function ,sym))));(coerce ,sym 'function)
       (or (when ,fnf (cfun-call ,fnf))
	   (when ,f
	     (let ((,n (make-call ,@keys)))
	       (when ,fnf (set-cfun-call ,n ,fnf))
	       ,n))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_defmacro.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_autoload.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.



;;;;    AUTOLOAD


;;; Go into LISP.
(in-package :si)

(export '(clines defentry defcfun)); defla

;(defconstant +keyword-package+ (find-package 'keyword))
;(defvar *features*)

;; (defun eval-feature (x)
;;   (cond ((atom x)
;;          (member x *features*))
;;         ((eq (car x) :and)
;;          (dolist (x (cdr x) t) (unless (eval-feature x) (return nil))))
;;         ((eq (car x) :or)
;;          (dolist (x (cdr x) nil) (when (eval-feature x) (return t))))
;;         ((eq (car x) :not)
;; 	 (not (eval-feature (cadr x))))
;; 	(t (error "~S is not a feature expression." x))))

;; ;;; Revised by Marc Rinfret.
;; (defun sharp-+-reader (stream subchar arg)
;;   (if (eval-feature (let ((*read-suppress* nil) 
;; 			  (*read-base* 10.)
;; 			  (*package* +keyword-package+))
;; 		      (read stream t nil t)))
;;       (values (read stream t nil t))
;;       (let ((*read-suppress* t)) (read stream t nil t) (values))))

;; (set-dispatch-macro-character #\# #\+ 'sharp-+-reader)
;; (set-dispatch-macro-character #\# #\+ 'sharp-+-reader
;;                               (si::standard-readtable))

;; (defun sharp---reader (stream subchar arg)
;;   (if (eval-feature (let ((*read-suppress* nil)
;; 			  (*read-base* 10.)
;; 			  (*package* +keyword-package+))
;;                          (read stream t nil t)))
;;       (let ((*read-suppress* t)) (read stream t nil t) (values))
;;       (values (read stream t nil t))))

;; (set-dispatch-macro-character #\# #\- 'sharp---reader)
;; (set-dispatch-macro-character #\# #\- 'sharp---reader
;;                               (si::standard-readtable))



(defun lisp-implementation-type () "GNU Common Lisp (GCL)")

(defun machine-type () #+sun "SUN"
  #+hp-ux "HP-UX"
  #+eclipse "ECLIPSE"
  #+vax "VAX"
  )
				 
;(defun machine-type () "DEC VAX11/780")

(defun machine-version () (machine-type))
;(defun machine-version () nil)

(defun machine-instance () (machine-type))
;(defun machine-instance () nil)

(defun software-type ()
  #+aosv "AOS/VS"
  #+bsd "BSD"
  #+system-v "SYSTEM-V"
  #+hp-ux "HP-UX")

;(defun software-type () "UNIX BSD")

(defun software-version () (software-type))
;(defun software-version () "4.2BSD")

;(defun short-site-name () "RIMS")
(defun short-site-name () nil)

;(defun long-site-name ()
;  "Research Institute for Mathematical Sciences, Kyoto University")
(defun long-site-name () nil)



;;; Compiler functions.

(defun proclaim (d)
       (when (eq (car d) 'special) (mapc #'si:*make-special (cdr d))))

(defun proclamation (d)
  (and (eq (car d) 'special)
       (dolist (var (cdr d) t)
               (unless (si:specialp var) (return nil)))))

(defun compile-file (&rest args)
       (error "COMPILE-FILE is not defined in this load module."))
(defun compile (&rest args)
       (error "COMPILE is not defined in this load module."))
(defun disassemble (&rest args)
       (error "DISASSEMBLE is not defined in this load module."))


;;; Editor.

;
(defun get-decoded-time ()
  (decode-universal-time (get-universal-time)))

#+never
(defun get-universal-time ()
  (multiple-value-bind (sec min h d m y dow dstp tz)
      (get-decoded-time)
    (encode-universal-time sec min h d m y tz)))

; System dependent Temporary directory.
(defun temp-dir ()
  "A system dependent path to a temporary storage directory as a string." 
  #+winnt (si::getenv "TEMP") #-winnt "/usr/tmp")

;  Set the default system editor to a fairly certain bet.
#-winnt(defvar *gcl-editor* "vi")
#+winnt(defvar *gcl-editor* "notepad")

(defun new-ed (editor-name)
  "Change the editor called by (ed) held in *gcl-editor*."
  (setf *gcl-editor* editor-name))

(defun ed (&optional name)
  "Edit a file using the editor named in *gcl-editor*; customise with new-ed()."
  (if (null name)
      (system *gcl-editor*)
    (cond ((stringp name) 
	   (system (format nil "~A ~A" *gcl-editor* name))) ; If string, assume file name.
	  ((pathnamep name)
	   (system (format nil "~A ~A" *gcl-editor* (namestring name)))) ; If pathname.
	  (t 
	   (let ((body (symbol-function name)))
	     (cond ((compiled-function-p body) (error "You can't edit compiled functions."))
		   ((and body
			 (consp body)
			 (eq (car body) 'lambda-block)) ; If lambda block, save file and edit.
		    (let ((ed-file (concatenate 'string
						(temp-dir)
						(format nil "~A" (cadr body))
						".lisp")))
		      (with-open-file
		       (st ed-file :direction :output)
		       (print `(defun ,name ,@ (cddr body)) st))
		      (system (format nil "~A ~A" *gcl-editor* ed-file))))
		   (t (system (format nil "~A ~A" *gcl-editor* name))))))))) ; Use symbol as filename

;;; Allocator.

(import 'si::allocate)
;(export '(allocate
	  ;allocated-pages maximum-allocatable-pages
          ;allocate-contiguous-pages
          ;allocated-contiguous-pages maximum-contiguous-pages
          ;allocate-relocatable-pages allocated-relocatable-pages 
;          spice structure))

;(defvar type-character-alist
;             '((cons . #\.)
;               (fixnum . #\N)
;               (bignum . #\B)
;               (ratio . #\R)
;               (short-float . #\F)
;               (long-float . #\L)
;               (complex . #\C)
;               (character . #\#)
;               (symbol . #\|)
;               (package . #\:)
;               (hash-table . #\h)
;               (array . #\a)
;               (vector . #\v)
;               (string . #\")
;               (bit-vector . #\b)
;               (structure . #\S)
;	       (sfun . #\g)
;               (stream . #\s)
;               (random-state . #\$)
;               (readtable . #\r)
;               (pathname . #\p)
;               (cfun . #\f)
;	       (vfun . #\V)
;               (cclosure . #\c)
;               (spice . #\!)))
;
;(defun get-type-character (type)
;  (let ((a (assoc type type-character-alist)))
;    (unless a
;            (error "~S is not an implementation type.~%~
;                   It should be one of:~%~
;                   ~{~10T~S~^~30T~S~^~50T~S~%~}~%"
;                   type
;                   (mapcar #'car type-character-alist)))
;    (cdr a)))

;(defun allocate (type quantity &optional really-allocate)
;  (si:alloc (get-type-character type) quantity really-allocate))

;(defun allocated-pages (type)
;  (si:npage (get-type-character type)))

;(defun maximum-allocatable-pages (type)
;  (si:maxpage (get-type-character type)))

;(defun allocate-contiguous-pages (quantity &optional really-allocate)
;  (si::alloc-contpage quantity really-allocate))

;(defun allocated-contiguous-pages ()
;  (si:ncbpage))

;(defun maximum-contiguous-pages ()
;  (si::maxcbpage))

;(defun allocate-relocatable-pages (quantity &optional really-allocate)
;  (si::alloc-relpage quantity))

;(defun allocated-relocatable-pages ()
;  (si::nrbpage))

;;; C Interface.

(defmacro Clines (&rest r) nil)
(defmacro defCfun (&rest r) nil)
(defmacro defentry (&rest r) nil)

(defmacro defla (&rest r) (cons 'defun r))

;;; Help.

;(export '(help help*))

(defun help (&optional (symbol nil s))
  (if s (si::print-doc symbol)
      (progn
        (princ "
Welcome to GNU Common Lisp (GCL for short).
Here are some functions you should learn first.

	(HELP symbol) prints the online documentation associated with the
	symbol.  For example, (HELP 'CONS) will print the useful information
	about the CONS function, the CONS data type, and so on.

	(HELP* string) prints the online documentation associated with those
	symbols whose print-names have the string as substring.  For example,
	(HELP* \"PROG\") will print the documentation of the symbols such as
	PROG, PROGN, and MULTIPLE-VALUE-PROG1.

	(SI::INFO <some string>) chooses from a list of all references in the
        on-line documentation to <some string>.

	(APROPOS <some string>) or (APROPOS <some string> '<a package>) list
        all symbols containing <some string>.

	(DESCRIBE '<symbol>) or (HELP '<symbol>) describe particular symbols.

	(BYE) or (BY) ends the current GCL session.

Good luck!				 The GCL Development Team")
        (values))))

(defun help* (string &optional (package (find-package "LISP")))
  (si::apropos-doc string package))

;;; Pretty-print-formats.
;;;
;;;	The number N as the property of a symbol SYMBOL indicates that,
;;;	in the form (SYMBOL f1 ... fN fN+1 ... fM), the subforms fN+1,...,fM
;;;	are the 'body' of the form and thus are treated in a special way by
;;;	the KCL pretty-printer.

;; (setf (get 'lambda 'si:pretty-print-format) 1)
;; (setf (get 'lambda-block 'si:pretty-print-format) 2)
;; (setf (get 'lambda-closure 'si:pretty-print-format) 4)
;; (setf (get 'lambda-block-closure 'si:pretty-print-format) 5)

;; (setf (get 'block 'si:pretty-print-format) 1)
;; (setf (get 'case 'si:pretty-print-format) 1)
;; (setf (get 'catch 'si:pretty-print-format) 1)
;; (setf (get 'ccase 'si:pretty-print-format) 1)
;; (setf (get 'clines 'si:pretty-print-format) 0)
;; (setf (get 'compiler-let 'si:pretty-print-format) 1)
;; (setf (get 'cond 'si:pretty-print-format) 0)
;; (setf (get 'ctypecase 'si:pretty-print-format) 1)
;; (setf (get 'defcfun 'si:pretty-print-format) 2)
;; (setf (get 'define-setf-method 'si:pretty-print-format) 2)
;; (setf (get 'defla 'si:pretty-print-format) 2)
;; (setf (get 'defmacro 'si:pretty-print-format) 2)
;; (setf (get 'defsetf 'si:pretty-print-format) 3)
;; (setf (get 'defstruct 'si:pretty-print-format) 1)
;; (setf (get 'deftype 'si:pretty-print-format) 2)
;; (setf (get 'defun 'si:pretty-print-format) 2)
;; (setf (get 'do 'si:pretty-print-format) 2)
;; (setf (get 'do* 'si:pretty-print-format) 2)
;; (setf (get 'do-symbols 'si:pretty-print-format) 1)
;; (setf (get 'do-all-symbols 'si:pretty-print-format) 1)
;; (setf (get 'do-external-symbols 'si:pretty-print-format) 1)
;; (setf (get 'dolist 'si:pretty-print-format) 1)
;; (setf (get 'dotimes 'si:pretty-print-format) 1)
;; (setf (get 'ecase 'si:pretty-print-format) 1)
;; (setf (get 'etypecase 'si:pretty-print-format) 1)
;; (setf (get 'eval-when 'si:pretty-print-format) 1)
;; (setf (get 'flet 'si:pretty-print-format) 1)
;; (setf (get 'labels 'si:pretty-print-format) 1)
;; (setf (get 'let 'si:pretty-print-format) 1)
;; (setf (get 'let* 'si:pretty-print-format) 1)
;; (setf (get 'locally 'si:pretty-print-format) 0)
;; (setf (get 'loop 'si:pretty-print-format) 0)
;; (setf (get 'macrolet 'si:pretty-print-format) 1)
;; (setf (get 'multiple-value-bind 'si:pretty-print-format) 2)
;; (setf (get 'multiple-value-prog1 'si:pretty-print-format) 1)
;; (setf (get 'prog 'si:pretty-print-format) 1)
;; (setf (get 'prog* 'si:pretty-print-format) 1)
;; (setf (get 'prog1 'si:pretty-print-format) 1)
;; (setf (get 'prog2 'si:pretty-print-format) 2)
;; (setf (get 'progn 'si:pretty-print-format) 0)
;; (setf (get 'progv 'si:pretty-print-format) 2)
;; (setf (get 'return 'si:pretty-print-format) 0)
;; (setf (get 'return-from 'si:pretty-print-format) 1)
;; (setf (get 'tagbody 'si:pretty-print-format) 0)
;; (setf (get 'the 'si:pretty-print-format) 1)
;; (setf (get 'throw 'si:pretty-print-format) 1)
;; (setf (get 'typecase 'si:pretty-print-format) 1)
;; (setf (get 'unless 'si:pretty-print-format) 1)
;; (setf (get 'unwind-protect 'si:pretty-print-format) 0)
;; (setf (get 'when 'si:pretty-print-format) 1)
;; (setf (get 'with-input-from-string 'si:pretty-print-format) 1)
;; (setf (get 'with-open-file 'si:pretty-print-format) 1)
;; (setf (get 'with-open-stream 'si:pretty-print-format) 1)
;; (setf (get 'with-standard-io-syntax 'si:pretty-print-format) 1)
;; (setf (get 'with-output-to-string 'si:pretty-print-format) 1)


(in-package :si)

(defvar *lib-directory* (namestring (truename "../")))

(import '(*lib-directory* *load-path* *system-directory*) 'si::user) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_autoload.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_sc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;to-do fast link defentry, sig propagation

;; (in-package 'lisp)
;; (export '(string char schar string= string/= string> string>= 
;; 		 string< string<= string-equal string-not-equal
;; 		 string-greaterp string-not-lessp string-lessp
;; 		 string-not-greaterp  char-code  code-char  char-upcase
;; 		 char-downcase  char=  char/=  char>  char>=  char<
;; 		 char<=  char-equal  char-not-equal  char-greaterp
;; 		 char-lessp  char-not-greaterp  char-not-lessp
;; 		 upper-case-p  lower-case-p  both-case-p
;; 		 string-upcase string-downcase nstring-upcase nstring-downcase
;; 		 string-trim string-left-trim string-right-trim))

(in-package :si)

(defun character-designator-p (s)
  (or (typep s 'fixnum)
      (= (c-stdesig-sdfillp s) 1)))

(deftype character-designator nil `(and string-designator (satisfies character-designator-p)))
(deftype string-designator    nil `(or string symbol character (integer 0 255)))

;; #.`(defun c-stdesig-self
;;      ,@(cdr (sublis '((array . string-designator) (c-array-self . c-stdesig-self)) (function-src 'c-array-self))))
;; #.`(defun c-stdesig-fillp 
;;      ,@(cdr (sublis '((vector . string-designator) (c-vector-fillp . c-stdesig-fillp)) (function-src 'c-vector-fillp))))

(eval-when
 (compile eval)

 (defmacro with-aref-shadow (&body body)
   `(labels ((lower-case-p (x) (<= #.(char-code #\a) x #.(char-code #\z)))
	     (upper-case-p (x) (<= #.(char-code #\A) x #.(char-code #\Z)))
	     (char-upcase (x) 
		     (if (lower-case-p x)
			 (+ x #.(- (char-code #\A) (char-code #\a))) x))
	     (char-downcase (x) 
			    (if (upper-case-p x)
				(+ x #.(- (char-code #\a) (char-code #\A))) x))
	     (aref (s i) (stdesig-self s i))
	     (aset (v s i) (set-stdesig-self s i v))
	     (length (s) (c-stdesig-sdfillp s))
	     (char= (x z) (= x z))
	     (char< (x z) (< x z))
	     (char> (x z) (> x z))
	     (char-equal (x z) (or (= x z) (= (char-upcase x) (char-upcase z))))
	     (char-greaterp (x z) (> (char-upcase x) (char-upcase z)))
	     (char-lessp    (x z) (< (char-upcase x) (char-upcase z))))
	    ,@body))

(defmacro defstr (name (s1 s2) = &body body)
   `(defun ,name (,s1 ,s2  &key (start1 0) end1 (start2 0) end2)
      (declare (optimize (safety 1)))
      (check-type s1 string-designator)
      (check-type s2 string-designator)
      (check-type start1 seqind)
      (check-type end1 (or null seqind))
      (check-type start2 seqind)
      (check-type end2 (or null seqind))
      (with-aref-shadow
       (let* ((s1 (if (typep s1 'fixnum) (code-char s1) s1))
	      (s2 (if (typep s2 'fixnum) (code-char s2) s2))
	      (l1 (length s1))
	      (l2 (length s2))
	      (e1 end1)(c1 0)
	      (e2 end2)(c2 0)
	      (end1 (or end1 l1))
	      (end2 (or end2 l2)))
	 (declare (ignorable c1 c2))
	 (unless (if e1 (<= start1 end1 l1) (<= start1 l1))
	   (error 'type-error "Bad array bounds"))
	 (unless (if e2 (<= start2 end2 l2) (<= start2 l2))
	   (error 'type-error "Bad array bounds"))
	 (do ((i1 start1 (1+ i1))
	      (i2 start2 (1+ i2)))
	     ((or (>= i1 end1) (>= i2 end2) (not (,= (setq c1 (aref s1 i1)) (setq c2 (aref s2 i2)))))
	      ,@body))))))

 (defmacro defchr (n (comp key))
   `(defun ,n (c1 &optional (c2 c1 c2p) &rest r) 
      (declare (optimize (safety 1)) (list r) (:dynamic-extent r));fixme
      (check-type c1 character)
      (or (not c2p)
	  (when (,comp (,key c1) (,key c2))
	    (or (null r) (apply ',n c2 r))))))
 
 (defmacro defnchr (n (test key))
   `(defun ,n (c1 &rest r) 
      (declare (optimize (safety 1)) (list r) (:dynamic-extent r));fixme
      (check-type c1 character)
      (cond ((null r))
	    ((member (,key c1) r :test ',test :key ',key) nil)
	    ((apply ',n r)))))


 (defmacro defstr1 (n query case &optional copy)
   `(defun ,n (s &key (start 0) end)
      (declare (optimize (safety 1)))
      (check-type s ,(if copy 'string-designator 'string))
      (check-type start seqind)
      (check-type end (or null seqind))
      (with-aref-shadow
       (flet ((cpy (s l)
		   (let ((n (make-array l :element-type 'character)))
		     (do ((j 0 (1+ j))) ((>= j l) n)
			 (aset (aref s j) n j)))))
	    (let* ((s (if (typep s 'fixnum) (code-char s) s))
		   (l (length s))
		   (e end)
		   (end (or end l))
		   (n ,(let ((x `(cpy s l))) (if copy x `(if (stringp s) s ,x)))))
	      (unless (if e (<= start end l) (<= start l)) 
		(error 'type-error "Bad sequence bounds"))
	      (do ((i start (1+ i))) ((>= i end) n)
		  (let ((ch (aref s i)))
		    (unless (,query ch)
		      (aset (,case ch) n i))))))))))

(defun character (c)
  (declare (optimize (safety 1)))
  (check-type c character-designator)
  (typecase
   c
   (character c)
   (unsigned-char (code-char c))
   (otherwise (code-char (stdesig-self c 0)))))


(defun char-int (c)
  (declare (optimize (safety 1)))
  (check-type c character-designator)
  (char-code c))

(defun int-char (c)
  (declare (optimize (safety 1)))
  (check-type c character-designator)
  (code-char c))

(defun char-name (c)
  (declare (optimize (safety 1)))
  (check-type c character-designator)
  (let ((c (char-code c)))
    (case c
	  (#.(char-code #\Return) "Return")
	  (#.(char-code #\Space) "Space")
	  (#.(char-code #\Rubout) "Rubout")
	  (#.(char-code #\Page) "Page")
	  (#.(char-code #\Tab) "Tab")
	  (#.(char-code #\Backspace) "Backspace")
	  (#.(char-code #\Newline) "Newline")
	  (otherwise (string c)))))

(defun name-char (s)
  (declare (optimize (safety 1)))
  (check-type s string-designator)
  (when (integerp s) (setq s (code-char s)))
  (cond ((cdr (assoc s '(("Return" . #\Return)
			 ("Space" . #\Space)
			 ("Rubout" . #\Rubout)
			 ("Page" . #\Page)
			 ("Tab" . #\Tab)
			 ("Backspace" . #\Backspace)
			 ("Newline" . #\Newline)
			 ("Linefeed" . #\Newline)) :test 'string-equal)))
	((code-char
	  (with-aref-shadow
	   (let ((l (length s)))
	     (case l
		   (1 (aref s 0))
		   ((2 3) (when (and (char= #\^ (aref s 0)) (or (= l 2) (char= #\\ (aref s 2))))
			    (code-char (- (char-code (aref s 1)) #.(- (char-code #\A) 1)))))
		   (4 (when (char= #\\ (aref s 0))
			(code-char 
			 (+ (* 64 (- (char-code (aref s 1)) #.(char-code #\0)))
			    (* 8 (- (char-code (aref s 2)) #.(char-code #\0)))
			    (- (char-code (aref s 3)) #.(char-code #\0)))))))))))))
		
   
(defun char-code (c)
  (declare (optimize (safety 1)))
  (check-type c character)
  (let ((b #.(- (1- (integer-length (- (address #\^A) (address #\^@)))))))
    (declare (ignorable b))
    (the unsigned-char (ash (the seqind (- (address c) (address #\^@))) b))))

(defun code-char (d)
;  (declare (optimize (safety 1)))
  (typecase d
	    (unsigned-char
	     (let ((b #.(1- (integer-length (- (address #\^A) (address #\^@))))))
	       (the character (nani (c+ (address #\^@) (ash d b))))))));FIXME


(defchr char=  (= address))
(defchr char>  (> address))
(defchr char>= (>= address))
(defchr char<  (< address))
(defchr char<= (<= address))

(defchr char-equal        (char= char-upcase))
(defchr char-greaterp     (char> char-upcase))
(defchr char-lessp        (char< char-upcase))
(defchr char-not-greaterp (char<= char-upcase))
(defchr char-not-lessp    (char>= char-upcase))

(defnchr char/=         (= address))
(defnchr char-not-equal (char-equal identity))

(defun upper-case-p (c)
  (declare (optimize (safety 1)))
  (check-type c character)
  (char>= #\Z c #\A))

(defun lower-case-p (c)
  (declare (optimize (safety 1)))
  (check-type c character)
  (char>= #\z c #\a))

(defun both-case-p (c)
  (declare (optimize (safety 1)))
  (check-type c character)
  (or (upper-case-p c) (lower-case-p c)))

(defun char-upcase (c)
  (declare (optimize (safety 1)))
  (check-type c character)
  (if (lower-case-p c)
      (nani (+ (address c) #.(- (address #\A) (address #\a))))
    c))

(defun char-downcase (c)
  (declare (optimize (safety 1)))
  (check-type c character)
  (if (upper-case-p c)
      (nani (+ (address c) #.(- (address #\a) (address #\A))))
    c))

(defun alphanumericp (c)
  (declare (optimize (safety 1)))
  (check-type c character)
  (or (char<= #\0 c #\9)
      (alpha-char-p c)))

(defun alpha-char-p (c)
  (declare (optimize (safety 1)))
  (check-type c character)
  (both-case-p c))

(defun digit-char-p (c &optional (r 10))
  (declare (optimize (safety 1)))
  (check-type c character)
  (check-type r (integer 0))
  (when (typep r 'fixnum)
    (let* ((r r)(r (1- r))(i (char-code c))
	   (j (- i #.(char-code #\0)))
	   (k (- i #.(- (char-code #\a) 10)))
	   (l (- i #.(- (char-code #\A) 10))))
      (cond ((and (<=  0 j r) (<= j 9)) j);FIXME infer across inlines
	    ((<= 10 k r 36) k)
	    ((<= 10 l r 36) l)))))

(defun digit-char (w &optional (r 10))
  (declare (optimize (safety 1)))
  (check-type w (integer 0))
  (check-type r (integer 0))
  (when (and (typep w 'fixnum) (typep r 'fixnum))
    (let ((w w)(r r))
      (when (< w r)
	(code-char (if (< w 10) (+ w #.(char-code #\0)) (+ w #.(- (char-code #\A) 10))))))))

(defun graphic-char-p (c)
  (declare (optimize (safety 1)))
  (check-type c character)
  (char<= #\Space c #\~))

(defun standard-char-p (c)
  (declare (optimize (safety 1)))
  (check-type c character)
  (or (graphic-char-p c) (char= c #\Newline)))

(defun char (x i)
  (declare (optimize (safety 1)))
  (check-type x string)
  (check-type i seqind)
  (aref x i))

;FIXME one function
(defun schar (x i)
  (declare (optimize (safety 1)))
  (check-type x simple-string)
  (check-type i seqind)
  (aref x i))




(defun string (x)
  (declare (optimize (safety 1)))
  (check-type x string-designator)
  (typecase 
   x
   (string x)
   (symbol (symbol-name x))
   (character (let ((n (make-array 1 :element-type 'character))) (setf (aref n 0) x) n))
   ((integer 0 255) (string (code-char x)))))



(defstr1  string-upcase   upper-case-p char-upcase   t)
(defstr1  string-downcase lower-case-p char-downcase t)
(defstr1 nstring-upcase   upper-case-p char-upcase)
(defstr1 nstring-downcase lower-case-p char-downcase)

(defstr string= (s1 s2) char=
  (and (>= i1 end1) (>= i2 end2)))

(defstr string/= (s1 s2) char=
  (unless (and (>= i1 end1) (>= i2 end2)) i1))

(defstr string> (s1 s2) char=
  (cond ((>= i1 end1) nil)
	((>= i2 end2) i1)
	((char> c1 c2) i1)))

(defstr string>= (s1 s2) char=
  (cond ((>= i2 end2) i1)
	((>= i1 end1) nil)
	((char> c1 c2) i1)))

(defstr string< (s1 s2) char=
  (cond ((>= i2 end2) nil)
	((>= i1 end1) i1)
	((char< c1 c2) i1)))

(defstr string<= (s1 s2) char=
  (cond ((>= i1 end1) i1)
	((>= i2 end2) nil)
	((char< c1 c2) i1)))


(defstr string-equal (s1 s2) char-equal
  (and (>= i1 end1) (>= i2 end2)))

(defstr string-not-equal (s1 s2) char-equal
  (unless (and (>= i1 end1) (>= i2 end2)) i1))

(defstr string-greaterp (s1 s2) char-equal
  (cond ((>= i1 end1) nil)
	((>= i2 end2) i1)
	((char-greaterp c1 c2) i1)))

(defstr string-not-lessp (s1 s2) char-equal
  (cond ((>= i2 end2) i1)
	((>= i1 end1) nil)
	((char-greaterp c1 c2) i1)))

(defstr string-lessp (s1 s2) char-equal
  (cond ((>= i2 end2) nil)
	((>= i1 end1) i1)
	((char-lessp c1 c2) i1)))

(defstr string-not-greaterp (s1 s2) char-equal
  (cond ((>= i1 end1) i1)
	((>= i2 end2) nil)
	((char-lessp c1 c2) i1)))



(defun string-left-trim (b s)
  (declare (optimize (safety 1)))
  (check-type b sequence)
  (let ((s (string s)))
    (do ((l (length s))
	 (i 0 (1+ i)))
	((or (>= i l) (not (find (aref s i) b)))
	 (if (= i 0) s (subseq s i))))))
      

(defun string-right-trim (b s)
  (declare (optimize (safety 1)))
  (check-type b sequence)
  (let* ((s (string s))
	 (l (length s)))
    (do ((i (1- l) (1- i)))
	((or (< i 0) (not (find (aref s i) b)))
	 (if (= i l) s (subseq s 0 (1+ i)))))))

(defun string-trim (b s)
  (declare (optimize (safety 1)))
  (check-type b sequence)
  (let* ((s (string s))
	 (l (length s)))
    (do ((i 0 (1+ i)))
	((or (>= i l) (not (find (aref s i) b)))
	 (do ((j (1- l) (1- j)))
	     ((or (< j i) (not (find (aref s j) b)))
	      (if (and (= i 0) (= j l)) s (subseq s i (1+ j)))))))))


;FIXME
;; (defun interpreted-function-p (x) 
;;   (typecase x (interpreted-function t)))

;; (defun seqindp (x)
;;   (typecase x (seqind t)))

(defun fixnump (x)
  (typecase x (fixnum t)))
(si::putprop 'fixnump t 'compiler::cmp-inline)


(defun constantp (x &optional env)
  (declare (ignore env))
  (typecase 
   x
   (symbol (= 1 (c-symbol-stype x)))
   (cons (eq 'quote (car x)))
   (otherwise t)))

;; FIXME these functions cannot be loaded interpreted, cause an infinite loop on typep/fsf

(defun functionp (x)
  (typecase x (function t)))

(defun compiled-function-p (x) 
  (typecase x (compiled-function t)))

(defun stringp (x)
  (typecase
   x
   (string t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_sc.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_callhash.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-

(in-package :si);FIXME this belongs in :compiler

(export '(*split-files* *sig-discovery* export-call-struct compress-fle))

(defstruct (call (:type list) (:constructor make-call))
  sig callees src file props name)
(defvar *cmr* nil)
(defvar *keep-state* nil)
(defvar *sig-discovery* nil)
(defvar *split-files* nil)

(defun break-state (sym x)
  (format t "Breaking state function ~s due to definition of ~s~%" x sym)
  (let ((o (old-src x)))
    (mapc (lambda (x) (remprop x 'state-function)) (car o))
    (mapc (lambda (x y) (unless (eq sym x) (eval `(defun ,x ,@(cdr y))))) (car o) (cadr o))
    (mapc (lambda (y) (push y *cmr*) (add-recompile y 'state-function (sig x) nil)) (car o)) 
    (fmakunbound x)
    (unintern x)))

(defconstant +et+ (mapcar (lambda (x) (cons (cmp-norm-tp x) x)) 
			  '(list cons proper-list proper-sequence sequence boolean null true array vector number immfix bfix bignum integer
				 function-designator
				 ratio short-float long-float float real number pathname hash-table function)))

(defun ex-type (tp) (or (cdr (assoc tp +et+)) tp))
(defun ex-sig (sig) (list (mapcar 'ex-type (car sig)) (ex-type (cadr sig))))
(defun unex-type (tp) (or (car (rassoc tp +et+)) tp))
(defun unex-sig (sig) (list (mapcar 'unex-type (car sig)) (unex-type (cadr sig))))

(defun export-call-struct (l)
  `(apply 'make-function-plist ',(ex-sig (pop l)) ',(pop l) ,(apply 'compress-fle (pop l)) ',l))

(defvar *sig-discovery-props* nil)

(defun symbol-function-plist (sym &aux (fun (symbol-to-function sym)))
  (when fun (c-function-plist fun)))

(defun sym-plist (sym &aux (pl (symbol-function-plist sym)))
  (when pl
    (or (cdr (assoc sym *sig-discovery-props*)) pl)))
				  
(defun needs-recompile (sym)
  (let* ((plist (sym-plist sym))
	 (callees (cadr plist)))
    (mapc (lambda (x &aux (s (car x)) (cmp-sig (cdr x))(act-sig (car (sym-plist s))))
	    (unless (eq sym s)
	      (when act-sig
		(unless (eq cmp-sig act-sig)
		  (return-from needs-recompile (list (list sym s cmp-sig act-sig))))))) callees)
    nil))

(defun same-file-all-callees (x y fn)
;  (let ((z (remove-if-not (lambda (x) (equal (file x) fn)) (callees x)))) ;FIXME remove inline
  (let (z)
    (dolist (l (callees x))
      (when (equal fn (file l));FIXME eq
	(push l z)))
    (do ((l (set-difference z y) (cdr l))
	 (r (union z y) (same-file-all-callees (car l) r fn)))
	((endp l) r))))

(defun same-file-all-callers (x y fn)
;  (let ((z (remove-if-not (lambda (x) (equal (file x) fn)) (callers x))));FIXME remove inline
  (let (z)
    (dolist (l (callers x))
      (when (equal fn (file l));FIXME eq
	(push l z)))
    (do ((l (set-difference z y) (cdr l))
	 (r (union z y) (same-file-all-callers (car l) r fn)))
	((endp l) r))))

;; (defun all-callees (x y)
;;   (let ((z (gethash x *ach*)))
;;     (if z (union z y)
;;       (let ((z (call-callees (gethash x *call-hash-table*))))
;; 	(do ((l (set-difference z y) (cdr l))
;; 	     (r (union z y) (all-callees (car l) r)))
;; 	    ((endp l) 
;; 	     (unless (intersection z y) (setf (gethash x *ach*) (set-difference r y)))
;; 	     r))))))

;; (defun all-callers (x y)
;;   (let ((z (gethash x *acr*)))
;;     (if z (union z y)
;;       (let ((z (call-callers (gethash x *call-hash-table*))))
;; 	(do ((l (set-difference z y) (cdr l))
;; 	     (r (union z y) (all-callers (car l) r)))
;; 	    ((endp l) 
;; 	     (unless (intersection z y) (setf (gethash x *acr*) (set-difference r y)))
;; 	     r))))))

(defun nsyms (n &optional syms)
  (declare (seqind n))
  (cond ((= n 0) (nreverse syms))
	((nsyms (1- n) (cons (gensym) syms)))))

(defun max-types (sigs &optional res)
  (cond ((not res) (max-types (cdr sigs) (ldiff (caar sigs) (member '* (caar sigs)))))
	((not sigs) res)
	((max-types (cdr sigs) 
		    (let ((z (ldiff (caar sigs) (member '* (caar sigs)))))
		      (append
		       (mapcar (lambda (x y) (or (not (equal x y)) x)) z res)
		     (early-nthcdr (length z) res)))))))

(defun early-nthcdr (i x)
  (declare (seqind i))
  (cond ((= 0 i) x)
	((early-nthcdr (1- i) (cdr x)))))

(defun old-src (stfn &optional src syms sts srcs)
  (cond (stfn (old-src nil (function-src stfn) syms sts srcs))
	((atom src) nil)
	((eq (car src) 'labels)
	 (list (mapcar 'car (cadr src)) 
	       (mapcar (lambda (x) (if (eq (caadr x) 'funcall) (cadadr x) (caadr x))) (cddr (caddr src)))))
	((or (old-src stfn (car src) syms sts srcs) (old-src stfn (cdr src) syms sts srcs)))))

(defun lambda-vars (ll)
  (remove '&optional (mapcar (lambda (x) (if (consp x) (car x) x)) ll)))

(defun inlinef (n syms sts fns)
    (unless (member-if (lambda (x) (intersection '(&rest &key &aux &allow-other-keys) (cadr x))) fns)
      (let* ((lsst (1- (length sts)))
	     (tps (max-types (mapcar 'sig syms)))
	     (min (reduce 
		   'min 
		   (mapcar (lambda (x) (length (ldiff (cadr x) (member '&optional (cadr x))))) fns)
		   :initial-value 64));FIXME
	     (max (reduce 'max (mapcar (lambda (x) (length (lambda-vars (cadr x)))) fns) :initial-value 0))
	     (reqs (nsyms min))
	     (opts (nsyms (- max min)))
	     (ll (append reqs (when (> max min) (cons '&optional opts))))
	     (all (reverse (append reqs opts))))
	`(defun ,n ,(cons 'state ll)
	   (declare (fixnum state) ,@(mapcar 'list tps reqs))
	   ,@(let (d (z (cddr (car fns)))) 
	       (when (stringp (car z)) (pop z))
	       (do nil ((or (not z) (not (consp (car z))) (not (eq (caar z) 'declare))) (nreverse d)) 
		   (let ((q (pop z)))
		     (when (and (consp (cadr q)) (eq 'optimize (caadr q))) 
		       (push q d)))))

	   (labels
	    ,(mapcan (lambda (x y z)
		       `((,x ,(cadr y) (,n ,z ,@(lambda-vars (cadr y)))))) syms fns sts)
	    (case state
		  ,@(mapcar
		     (lambda (x y)
		       `(,(if (= x lsst) 'otherwise x) 
			 (funcall ,y ,@(reverse (early-nthcdr (- max (length (lambda-vars (cadr y)))) all)))))
		     sts fns)))))))

(defun sig (x) (let ((h (call x))) (when h (call-sig h))))
(defun signature (x) (ex-sig (sig x)))
(defun props (x) (let ((h (call x))) (when h (call-props h))))
(defun src (x) (let ((h (call x))) (when h (call-src h))))
(defun file (x) (let ((h (call x))) (when h (call-file h))))
(defun name (x) (let ((h (call x))) (when h (call-name h))))
(defun callees (x) (let ((h (call x))) (when h (call-callees h))))
(defun callers (x) (get x 'callers))

;; (defun *s (x) 
;;   (let ((p (find-package x)))
;;     (remove-if-not
;;      (lambda (y) (eq (symbol-package y) p)) 
;;      (let (r) 
;;        (maphash (lambda (x y) (when (eq '* (cadr (call-sig y))) (push x r))) *call-hash-table*)
;;        r))))

(defun mutual-recursion-peers (sym)
  (unless (or (get sym 'state-function) (get sym 'mutual-recursion-group))
    (let ((y (sig sym)))
      (when (eq '* (cadr y))
	(let ((e (same-file-all-callees sym nil (file sym)))
	      (r (same-file-all-callers sym nil (file sym))))
	  (remove-if-not
	   (lambda (x) 
	     (and (eq (symbol-package x) (symbol-package sym))
		  (let ((h (call x)))
		    (when h (eq '* (cadr (call-sig h)))))))
	   (intersection e r)))))))

;(defun mutual-recursion-peers (sym)
;  (unless (or (get sym 'state-function) (get sym 'mutual-recursion-group))
;    (let ((y (sig sym)))
;      (when (eq '* (cadr y)) 
;	(let* ((e (same-file-all-callees sym nil (file sym)))
;	       (r (same-file-all-callers sym nil (file sym)))
;	       (i (intersection e r))
;	       (i1 (remove-if-not (lambda (x) (get x 'mutual-recursion-group)) i))
;	       (i2 (set-difference i i1))
;	       (i (remove-duplicates (union (mapcan (lambda (x) (list (get x 'mutual-recursion-group))) i1) i2))))
;	  (mapc (lambda (x) (break-state x x)) i1)
;	  (remove-if-not (lambda (x) (eq '* (cadr (sig x)))) i))))))

;	  (remove-if (lambda (x) (get x 'mutual-recursion-group))
;		     (remove-if-not (lambda (x) (eq '* (cadr (sig x)))) i)))))))

(defun convert-to-state (sym)
  (let ((syms (mutual-recursion-peers sym)))
    (when (and (remove sym syms) (member sym syms))
      (let* ((fns (mapcar 'function-src syms))
	     (n (intern (symbol-name (gensym (symbol-name sym))) (symbol-package sym)))
	     (*keep-state* n)
	     (sts (let (sts) (dotimes (i (length syms) (nreverse sts)) (push i sts))))
	     (ns (inlinef n syms sts fns)))
	(when ns
	  (eval ns)
	  (mapc (lambda (x y z) (let ((z (cadr z))) (eval `(defun ,x ,z (,n ,y ,@(lambda-vars z)))))) syms sts fns)
	  (mapc (lambda (x) (putprop x n 'state-function)) syms)
;	  (dolist (l syms) (add-hash l nil (list (list n)) nil nil))
	  (putprop n syms 'mutual-recursion-group)
	  (add-recompile n 'mutual-recursion nil nil)
	  n)))))
    
(defun temp-prefix nil
  (concatenate 'string
	       *tmp-dir*
	       "gazonk_"
	       (write-to-string (let ((p (getpid))) (if (>= p 0) p (- p))))
	       "_"));FIXME

(defun compiler-state-fns nil
  (let ((p (find-package "COMPILER")))
    (when p
      (do-symbols 
       (s p)
       (when (member s *cmr*)
	 (let* ((x (convert-to-state s))(*keep-state* x))
	   (when x
	     (compile x)
	     (mapc 'compile (get x 'mutual-recursion-group)))))))))

(defun dead-code (ps &aux r)
  (let ((p (find-package ps)))
    (when p
      (do-symbols
       (s p r)
       (when (fboundp s)
	 (unless (macro-function s)
	   (multiple-value-bind
	    (s k)
	    (find-symbol (symbol-name s) p)
	    (when (eq k :internal)
	      (unless (get s 'callers)
		(push s r))))))))))

(defun do-pcl (x &aux (*sig-discovery-props* x))
  (break)
  (si::chdir "../pcl")
  (mapc (lambda (x) (when (find-package x) (delete-package x))) '(:pcl :slot-accessor-name :iterate :walker))
  (mapc 'delete-file (directory "*.o"))
  (mapc 'load '("../clcs/package.lisp" "../clcs/myload1.lisp" "sys-package.lisp"))
  (let ((*features* (remove :kcl *features*))) (load "../pcl/defsys.lisp"))
  (setf (symbol-value (find-symbol "*DEFAULT-PATHNAME-EXTENSIONS*" (find-package :pcl))) (cons "lisp" "o")
	(symbol-value (find-symbol "*PATHNAME-EXTENSIONS*" (find-package :pcl))) (cons "lisp" "o"))
					;(load "sys-proclaim.lisp")
  (setq compiler::*keep-gaz* t compiler::*tmp-dir* "" si::*disable-recompile* t)
  (si::chdir "../pcl")
  (funcall (find-symbol "COMPILE-PCL" :pcl)))

(defun do-recomp (&rest excl &aux r *sig-discovery-props* *compile-verbose*)
  (labels ((d (&aux (*sig-discovery* t)(q (remove-duplicates (mapcar 'car (mapcan 'needs-recompile r)))))
	      (when q
		(format t "~%Pass 1 signature discovery on ~s functions ..." (length q))
		(mapc (lambda (x) (format t "~s " x) (compile x)) q) (d))))
	  (do-all-symbols (s) (push s r))(d)
	  (let* ((fl (mapcar 'car *sig-discovery-props*))
		 (pclp (member-if (lambda (x) (eq (symbol-package x) (find-package :pcl))) fl))
		 (fl (remove-duplicates (mapcar (lambda (x) (file x)) fl) :test 'string=))
		 (fl (set-difference fl (cons "pcl_" excl) :test (lambda (x y) (search y x)))))
	    (compiler::cdebug)
	    (format t "~%Recompiling original source files ...")
	    (mapc (lambda (x) (format t "~s~%" x) (compile-file x)) (remove nil fl))
	    (when pclp
	      (do-pcl *sig-discovery-props*)))))

(defun do-recompile (&optional (pn nil pnp))

  (unless *disable-recompile*

    (let ((*disable-recompile* t) rfns)

      (do nil ((and (not *cmr*) (= (length *needs-recompile*) 0)) (setq rfns (nreverse rfns)))

	  (when (= 0 (length *needs-recompile*)) 
	    (if (and pnp (not pn)) 
		(setq *cmr* nil);no new file in which to place the generated state functions
	      (do nil ((not *cmr*)) (convert-to-state (pop *cmr*)))))

	  (unless (= 0 (length *needs-recompile*))

	    (sort *needs-recompile*
		  (lambda (x y) (member (car x) (callees (car y)))))

	    (map nil (lambda (fn)
		       (when (eq (cadr fn) 'mutual-recursion)
			 (format t "Mutual recursion detected: ~s, recompiling ~s~%" 
				 (get (car fn) 'mutual-recursion-group) (car fn)))) *needs-recompile*)

	    (format t "Pass1 signature discovery on ~s functions ...~%" (length *needs-recompile*))

	    (let (fns)

	      (dolist (i *needs-recompile*)
		(let ((fn (car i)))
		  (pushnew fn rfns)
		  (push fn fns)))

	      (let ((*sig-discovery* t)(*compile-verbose* nil)) 
		(dolist (fn (nreverse fns))
		  (compile fn))))))

      (if (and pnp (not pn))

	  (let (files);FIXME mutual-recursion fns somewhere
	    (dolist (l rfns)
	      (let ((file (file l))) 
		(when file
		  (unless (or (search "pcl_boot" file);FIXME
			      (search "cmpmain" file)
			      (search "clcs_install" file))
		    (pushnew file files :test 'string=)))))
	    (when files (format t "Updating original source files ~s~%" files))
	    (dolist (l files)
	      (when (probe-file l) (compile-file l :system-p t :c-file t :h-file t :data-file t))))
	
	(when rfns
	  (with-temp-file
	      (s tpn) ((temp-prefix) "lsp")
	      (declare (ignore tpn))
	      (unless pnp (setq pn s))
	      (format t "Compiling and loading new source in ~s~%" pn)
	      (with-open-file 
	       (f pn :direction :output :if-exists :append :if-does-not-exist :create)
	       (with-compile-file-syntax
		(dolist (l rfns)
		  (prin1 `(defun ,l ,@(cdr (function-src l))) f)
		  (dolist (p '(state-function mutual-recursion-group))
		    (let ((x (get l p)))
		      (when x 
			(prin1 `(putprop ',l ',x ',p) f)))))
		(prin1 `(setq *cmr* nil) f)))
	      
	      (let* ((*split-files* 100000)
		     (o (compile-file pn :system-p t :c-file t :h-file t :data-file t)))
		(unless pnp (load o)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_callhash.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_type.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :si)

(export '(cmpt tmpsym *or-tp-hash* *and-tp-hash* *norm-tp-hash* *uniq-tp-hash* object-type
	       readable-tp returs-exactly eov contains-cons-tp uniq-tp cmp-norm-tp funcallable-symbol-function atomic-tp
	       type-and type-or1 type>= type<= min-ftp infer-tp))


(defvar *uniq-tp-hash* (make-hash-table :test 'equal))
(defvar *norm-tp-hash* (make-hash-table :test 'eq))
(defvar *and-tp-hash* (make-hash-table :test 'eq))
(defvar *or-tp-hash*  (make-hash-table :test 'eq))

(defmacro cmpt (tp)  `(and (consp ,tp) (member (car ,tp) '(returns-exactly values))))
(defmacro cmpdt (tp) `(and (consp ,tp) (member (car ,tp) '(and or not cons complex
							       returns-exactly values))))
(defmacro cmpao (tp) `(and (consp ,tp) (member (car ,tp) '(and or))))
(defmacro cmpmt (tp) `(and (consp ,tp) (member (car ,tp) '(not cons complex returns-exactly values))))

(defun lsort (l f)
  (when l
    (let* ((z (pop l))
	   (r (mapcar (lambda (x) (if (funcall f z x) x (prog1 z (setq z x)))) l)))
      (cons z (lsort r f)))))

(defun sort-tps (tps)
  (lsort tps (lambda (x y) (> (si::address x) (si::address y)))))

(defun build-tp (tp)
  (cond ((cmpdt tp) (cons (car tp) (mapcar 'uniq-tp (cdr tp))))
	(tp)))

(defun contains-cons-tp (tp)
  (cond ((atom tp) nil)
	((eq (car tp) 'member) (member-if 'consp (cdr tp)))
	((or (contains-cons-tp (car tp)) (contains-cons-tp (cdr tp))))))

(defun uniq-tp (tp) 
  (cond ((contains-cons-tp tp) tp)
	((gethash tp *uniq-tp-hash*))
	((let ((tp (build-tp tp)))
	   (setf (gethash tp *uniq-tp-hash*) tp)))))

(defun readable-tp (tp)
  (uniq-tp
   (cond ((cmpao tp) (cons (car tp) (sort-tps (mapcar 'readable-tp (cdr tp)))))
	 ((cmpmt tp) (cons (car tp) (mapcar 'readable-tp (cdr tp))))
	 ((si::si-classp tp) (or (si::si-class-name tp) t))
	 ((si::structurep tp) (or (si::s-data-name tp) t))
	 ((and (consp tp) (eq (car tp) 'member)) `(,(car tp) ,@(sort-tps (cdr tp))))
	 (tp))))

(defun dnt (tp)
  (uniq-tp
   (cond ((eq '* tp) tp)
	 ((cmpt tp)
	  (cond ((not (cdr tp)) '(returns-exactly))
		((and (eq (car tp) 'returns-exactly) (not (cddr tp))) (cmp-norm-tp (cadr tp)))
		(`(,(car tp) ,@(mapcar 'cmp-norm-tp (cdr tp))))))
	 ((let ((tp (resolve-type tp))) (if (cadr tp) '* (readable-tp (car tp))))))))
  
(defun cmp-norm-tp (tp)
  (multiple-value-bind 
   (r f) (gethash tp *norm-tp-hash*)
   (cond (f r)
	 ((let ((tp (uniq-tp tp)))
	    (multiple-value-bind 
	     (r f) (gethash tp *norm-tp-hash*)
	     (cond (f r)
		   ((let ((nt (dnt tp)))
		      (cond ((and (eq '* nt) (not (eq tp '*))) nt);don't hash unknown types
			    ((and (consp tp) (or (eq (car tp) 'si::type-max) (eq (car tp) 'si::type-min))) nt)
			    ((or (contains-cons-tp tp) (contains-cons-tp nt)) nt)
			    ((setf (gethash tp *norm-tp-hash*) nt (gethash nt *norm-tp-hash*) nt))))))))))))



(defconstant +null+ (cmp-norm-tp 'null))
(defconstant +cons+ (cmp-norm-tp 'cons))
(defconstant +proper-cons+ (cmp-norm-tp 'proper-cons))

(defmacro eov (type1 l1 type2 l2)
  `(if (and (= ,l1 ,l2) 
	    (eq (car ,type1) 'returns-exactly)
	    (eq (car ,type2) 'returns-exactly)) 
       'returns-exactly 'values))

(defun type-and-int (type1 type2 x)
  (cond ((eq type1 type2) type2)
	((eq type1 '*) type2)
	((eq type2 '*) type1)
	((not type1) nil)
	((not type2) nil)
	((equal type1 type2) type2)
	((and (cmpt type1) (cmpt type2))
	 (let* ((ntype1 (if (cmpt type1) (cdr type1) (when type1 (list type1))))
		(ntype2 (if (cmpt type2) (cdr type2) (when type2 (list type2))))
		(l1 (length ntype1))
		(l2 (length ntype2))
		(eov (eov type1 l1 type2 l2)))
	   (cond ((and (every 'type>= ntype1 ntype2) (>= l1 l2) (eq (car type2) eov)) type2)
		 ((and (every 'type>= ntype2 ntype1) (>= l2 l1) (eq (car type1) eov)) type1)
		 ((cmp-norm-tp `(,eov ,@(mapcar 'type-and ntype1 ntype2)))))))
	((cmpt type1) (type-and (or (cadr type1) +null+) type2))
	((cmpt type2) (type-and type1 (or (cadr type2) +null+)))
	((member type1 '(t object)) type2)
	((member type2 '(t object)) type1)
	((subtypep1 type2 type1) type2)
	((subtypep1 type1 type2) type1)
	((cmp-norm-tp x))))



(defun type-or1-int (type1 type2 x)
  (cond ((eq type1 type2) type2)
	((eq type1 '*) type1)
	((eq type2 '*) type2)
	((not type1) type2)
	((not type2) type1)
	((equal type1 type2) type2)
	((and (cmpt type1) (cmpt type2))
	 (let* ((ntype1 (if (cmpt type1) (cdr type1) (when type1 (list type1))))
		(ntype2 (if (cmpt type2) (cdr type2) (when type2 (list type2))))
		(l1 (length ntype1))
		(l2 (length ntype2))
		(n (- (max l1 l2) (min l1 l2)))
		(e (make-list n :initial-element +null+))
		(ntype1 (if (< l1 l2) (append ntype1 e) ntype1))
		(ntype2 (if (< l2 l1) (append ntype2 e) ntype2))
		(eov (eov type1 l1 type2 l2)))
	   (cond ((and (every 'type>= ntype2 ntype1) (>= l2 l1) (eq (car type2) eov)) type2)
		 ((and (every 'type>= ntype1 ntype2) (>= l1 l2) (eq (car type1) eov)) type1)
		 ((cmp-norm-tp `(,eov ,@(mapcar 'type-or1 ntype1 ntype2)))))))
	((cmpt type1) (type-or1 type1 (cmp-norm-tp `(values ,type2))))
	((cmpt type2) (type-or1 (cmp-norm-tp `(values ,type1)) type2))
	((member type1 '(t object)) type1)
	((member type2 '(t object)) type2)
	((subtypep1 type1 type2) type2)
	((subtypep1 type2 type1) type1)
	((cmp-norm-tp x))))

(defun atomic-tp (tp)
  (when (consp tp)
    (case (car tp)
      ((immfix bfix bignum ratio short-float long-float)
       (let* ((d (cdr tp))(dd (cadr d))(da (car d)))
	 (and (numberp da) (numberp dd) (= da dd) d)))
      ((member eql) (let ((d (cdr tp))) (unless (cdr d) d))))))
;(declaim (inline atomic-tp))

(defun t-to-nil (x) (unless (eq x t) x))
(setf (get 't-to-nil 'cmp-inline) t)

(defun type-and (t1 t2 &aux h1 h2 r f c1 c2);m1 m2
  (cond
   ((eq t1 t2) t2);accelerator
   ((eq t1 '*) t2);accelerator
   ((eq t2 '*) t1);accelerator
   ((not t1) nil)
   ((not t2) nil)
   ((when (setq c1 (t-to-nil (car (atomic-tp t1))) c2 (t-to-nil (car (atomic-tp t2)))) nil))
   (c2 (when (or (eql c1 c2) (typep c2 (if (cmpt t1) (cadr t1) t1))) t2))
   (c1 (when (typep c1 (if (cmpt t2) (cadr t2) t2)) t1))
   ((when (setq h1 (gethash t1 *and-tp-hash*)) (multiple-value-setq (r f) (gethash t2 h1)) f) r)
   ((when (setq h2 (gethash t2 *and-tp-hash*)) (multiple-value-setq (r f) (gethash t1 h2)) f) r)
   ((let ((x (uniq-tp `(and ,t1 ,t2))))
      (multiple-value-bind
       (r f)
       (gethash x *norm-tp-hash*)
       (if f r
	 (let ((q (type-and-int t1 t2 x)))
	   (unless (contains-cons-tp q)
	     (setf (gethash x *norm-tp-hash*) q)
	     (unless (contains-cons-tp t2) (when h1 (setf (gethash t2 h1) q)))
	     (unless (contains-cons-tp t1) (when h2 (setf (gethash t1 h2) q))))
	   q)))))))

(defmacro uniq-tp-from-stack (op t1 t2)
  (let ((s (tmpsym)))
    `(let ((,s (list ,op ,t1 ,t2)))
       (declare (:dynamic-extent ,s))
       (uniq-tp ,s))))

(defun type-or1 (t1 t2 &aux h1 h2 r f c1 c2);FIXME think about atomic types
  (flet ((to (t1 t2 &aux (x (uniq-tp-from-stack `or t1 t2))) (type-or1-int t1 t2 x)));(cmp-norm-tp x) FIXME no values support
	(cond
	 ((eq t1 t2) t2);accelerator
	 ((eq t1 '*) t1);accelerator
	 ((eq t2 '*) t2);accelerator
	 ((not t1) t2);FIXME atomic type logic requires eq -> and
	 ((not t2) t1)
	 ((when (setq c1 (t-to-nil (car (atomic-tp t1))) c2 (t-to-nil (car (atomic-tp t2)))) nil))
	 (c1 (if (or (eql c1 c2) (unless (cmpt t2) (typep c1 t2))) t2 
	       (if (consp c1) (type-or1 (if (cdr (last c1)) +cons+ +proper-cons+) t2) (to t1 t2))))
	 (c2 (if (unless (cmpt t1) (typep c2 t1)) t1 (if (consp c2) (type-or1 t1 (if (cdr c2) +cons+ +proper-cons+)) (to t1 t2))))
	 ((when (setq h1 (gethash t1 *or-tp-hash*)) (multiple-value-setq (r f) (gethash t2 h1)) f) r)
	 ((when (setq h2 (gethash t2 *or-tp-hash*)) (multiple-value-setq (r f) (gethash t1 h2)) f) r)
	 ((let ((x (uniq-tp `(or ,t1 ,t2))))
	    (multiple-value-bind
	     (r f)
	     (gethash x *norm-tp-hash*)
	     (if f r
	       (let ((q (type-or1-int t1 t2 x)))
		 (unless (contains-cons-tp q)
		   (setf (gethash x *norm-tp-hash*) q)
		   (unless (contains-cons-tp t2) (when h1 (setf (gethash t2 h1) q)))
		   (unless (contains-cons-tp t1) (when h2 (setf (gethash t1 h2) q))))
		 q))))))))

(defun type>= (t1 t2)
  (let ((z (type-and t1 t2)))
    (eq z t2)))

(defun type<= (t1 t2)
  (let ((z (type-and t2 t1)))
    (eq z t1)))

(defun min-ftp (tp)
  (let ((n (cmp-norm-tp tp)))
    (if (eq n '*)
	(cmp-norm-tp `(si::type-min (or nil ,tp)))
      n)))





(defconstant +ctps+ (mapcar (lambda (x) (list x (intern (string-concatenate "COMPLEX-" (string x))))) +complex-types+));FIXME
(defconstant +vtps+ (mapcar (lambda (x) (list x (intern (string-concatenate "VECTOR-"  (string x))))) +array-types+))
(defconstant +atps+ (mapcar (lambda (x) (list x (intern (string-concatenate "ARRAY-"   (string x))))) +array-types+))

#.`(progn
     ,@(mapcar (lambda (x &aux (s (cadr x))(x (car x))) 
		 `(deftype ,s (&optional (l '*) (h '*)) `(complex (,',x ,l ,h)))) +ctps+)
     ,@(mapcar (lambda (x &aux (s (cadr x))(x (car x))) 
		 `(deftype ,s (&optional (d '*)) `(vector ,',x ,d))) +vtps+)
     ,@(mapcar (lambda (x &aux (s (cadr x))(x (car x))) 
		 `(deftype ,s (&optional (d '*)) `(and (array ,',x ,d) (not vector)))) +atps+))


(defconstant +r+ `((immfix 1) 
		   (bfix  most-positive-fixnum)
		   (bignum (1+ most-positive-fixnum))
		   (ratio 1/2)
		   (short-float 1.0s0) 
		   (long-float 1.0)
		   ,@(mapcar (lambda (x &aux (v (case (car x) (integer 1) (ratio 1/2) (short-float 1.0s0) (long-float 1.0)))) 
			       `(,(cadr x) (complex ,v ,v))) +ctps+)
		   (standard-char #\a)
		   (non-standard-base-char #\Return)
		   (structure (make-dummy-structure)) 
		   (std-instance (set-d-tt 1 (make-dummy-structure))) 
		   (non-logical-pathname (init-pathname nil nil nil nil nil nil ""))
		   (logical-pathname (set-d-tt 1 (init-pathname nil nil nil nil nil nil "")))
		   (hash-table-eq (make-hash-table :test 'eq))
		   (hash-table-eql (make-hash-table :test 'eql))
		   (hash-table-equal (make-hash-table :test 'equal))
		   (hash-table-equalp (make-hash-table :test 'equalp))
		   (package *package*)
		   (file-input-stream (open-int "/dev/null" :input 'character nil nil nil nil :default))
		   (file-output-stream (open-int "/dev/null" :output 'character nil nil nil nil :default))
		   (file-io-stream (open-int "/dev/null" :io 'character nil nil nil nil :default))
		   (file-probe-stream (open-int "/dev/null" :probe 'character nil nil nil nil :default))
		   (file-synonym-stream (make-synonym-stream '*standard-output*))
		   (non-file-synonym-stream *standard-input*)
		   (broadcast-stream (make-broadcast-stream))
		   (concatenated-stream (make-concatenated-stream))
		   (two-way-stream *terminal-io*)
		   (echo-stream (make-echo-stream *standard-output* *standard-output*))
		   (string-input-stream (make-string-input-stream ""))
		   (string-output-stream (make-string-output-stream));FIXME user defined, socket
		   (random-state (make-random-state)) 
		   (readtable (standard-readtable)) 
		   (non-standard-generic-compiled-function (function eq))
		   (non-standard-generic-interpreted-function (set-d-tt 2 (lambda nil nil)))
		   (standard-generic-compiled-function (set-d-tt 1 (lambda nil nil)))
		   (standard-generic-interpreted-function (set-d-tt 3 (lambda nil nil)))
		   ,@(mapcar (lambda (x) `(,(cadr x) (make-vector ',(car x) 1 nil nil nil 0 nil nil))) +vtps+)
		   ,@(mapcar (lambda (x) `(,(cadr x) (make-array1 ',(car x) nil nil nil 0 '(1 1) nil))) +atps+)
                   (spice (alloc-spice))
		   (cons '(1))
		   (keyword :a)
		   (null nil)
		   (true t)
		   (gsym 'a)))

(defconstant +rn+ (mapcar (lambda (x) (cons (cmp-norm-tp (car x)) (cadr x))) +r+))

(defconstant +tfns1+ '(tp0 tp1 tp2 tp3 tp4 tp5 tp6 tp7 tp8))

(defconstant +rs+ (mapcar (lambda (x) (cons x (mapcar (lambda (y) (cons (car y) (funcall x (eval (cdr y))))) +rn+))) +tfns1+))

(defconstant +kt+ (mapcar 'car +rn+))

(eval-when
 (compile eval)
 (defun msym (x) (intern (string-concatenate (string x) "-TYPE-PROPAGATOR") :si)))

#.`(progn
     ,@(mapcar (lambda (x &aux (s (msym x))) 
		 `(progn 
		    (defun ,s (f x &aux (rl (load-time-value (cdr (assoc ',x +rs+)))))
		      (declare (ignore f))
		      (cmp-norm-tp (cons 'member (tps-ints (type-and-list (list x)) rl))))
		    (setf (get ',x 'type-propagator) ',s)
		    (setf (get ',x 'c1no-side-effects) t))) +tfns1+))

(mapc (lambda (x) 
	(setf (gethash x *and-tp-hash*) (make-hash-table :test 'eq :size 256))
	(setf (gethash x  *or-tp-hash*) (make-hash-table :test 'eq :size 256))) +kt+)


(defun type-and-list (tps)
  (mapcan (lambda (x) (mapcan (lambda (y &aux (z (type-and x y))) (when z `((,x ,y ,z)))) +kt+)) tps))

(defun best-type-of (c &aux (r (lreduce 'set-difference c :key 'car :initial-value +kt+))(tps (nconc (mapcar 'car c) (list r))))
  (or (caar (member-if (lambda (x &aux (f (pop x))
				  (z (mapcan (lambda (y) (lremove-duplicates (mapcar (lambda (z) (cdr (assoc z x))) y))) tps)))
			 (eq z (lremove-duplicates z))) +rs+))
      (caar +rs+)))

;; (defun malist (car cadr a)
;;   (mapcar (lambda (x) (cons x (mapcar car (lremove-if-not (lambda (z) (eq (funcall cadr z) x)) a)))) (lremove-duplicates (mapcar cadr a))))

(defun calist2 (a)
  (let* ((subs (lremove-duplicates (mapcar 'cadr (lremove-if (lambda (x) (eq (cadr x) (caddr x))) a))))
	 (x (mapcar (lambda (x) (cons (list x) (mapcar (lambda (x) (cons (car x) (caddr x)))
						       (lremove-if-not (lambda (y) (eq (cadr y) x)) a)))) subs))
	 (ra (lremove-if (lambda (x) (member (cadr x) subs)) a))
	 (y (mapcar (lambda (x) (list (mapcar 'cadr (lremove-if-not (lambda (y) (eq x (car y))) ra)) (cons x nil))) 
		    (lremove-duplicates (mapcar 'car ra)))))
    (nconc x y)))

;; (defun calist2 (a)
;;   (let* ((b (malist 'car 'cadr a))
;; 	 (b (mapcar 'cdr (malist 'car 'cadr b))))
;;     (mapcar (lambda (x) (cons x (lremove-duplicates (mapcar 'car (lremove-if-not (lambda (z) (member (cadr z) x)) a))))) b)))

(defun tps-ints (a rl)
  (lremove-duplicates (mapcar (lambda (x) (cdr (assoc (cadr x) rl))) a)))

(defun ints-tps (a rl)
  (lreduce (lambda (y x) (if (member (cdr x) a) (type-or1 y (car x)) y)) rl :initial-value nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_type.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/make.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*-  Mode: Lisp; Package: MAKE; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;; Copyright William F. Schelter 1989.

;; The author expressly permits copying and alteration of this file,
;; provided any modifications are clearly labeled, and this notice is
;; preserved.   The author provides no warranty and this software is
;; provided on an 'as is' basis.
(in-package "MAKE" :use '("LISP") #+gcl :external #+gcl 11
	    #+gcl :internal #+gcl 79)

(export '(make system-load system-compile))
(provide "MAKE")
;;;  *******  Description of Make Facility ************
;;  We provide a simple MAKE facility to allow
;;compiling and loading of a tree of files
;;If the tree is '(a b (d e g h) i)
;;   a will be loaded before b is compiled,
;;   b will be loaded before d, e, g, h are compiled
;;   d e g h will be loaded before i is compiled.

;;  A record is kept of write dates of loaded compiled files, and a file
;;won't be reloaded if it is the same version (unless a force flag is t).

;;Thus if you do (make :uinfor) twice in a row, the second one would not
;;load anything.  NOTE: If you change a, and a macro in it would affect
;;b, b still will not be recompiled.  You must choose the :recompile t
;;option, to force the recompiling if you change macro files.
;;Alternately you may specify dependency information (see :depends below).


;;****** Sample file which when loaded causes system ALGEBRA 
;;              to be compiled and loaded ******

;;(require "MAKE")
;;(use-package "MAKE")
;;(setf (get :algebra :make) '(a b (d e) l))
;;(setf (get :algebra :source-path) "/usr2/wfs/algebra/foo.lisp")
;;(setf (get :algebra :object-path) "/usr2/wfs/algebra/o/foo.o")
;;(make :algebra :compile t)

;;  More complex systems may need to do some special operations
;;at certain points of the make.  
;;the tree of files may contain some keywords which have special meaning.
;;eg. '(a b (:progn (gbc) (if make::*compile*
;;                                  (format t "A and B finally compiled")))
;;          (:load-source h i)
;;          (d e) l)

;;then during the load and compile phases the function (gbc) will be
;;called after a and b have been acted on, and during the compile phase
;;the message about "A and B finally.." will be printed.
;;the lisp files h and i will be loaded after merging the paths with 
;;the source directory.  This feature is extensible: see the definitions
;;of :load-source and :progn.

;;  The keyword feature is extensible, and you may specify what 
;;happens during the load or compile phase for your favorite keyword.
;;To do this look at the definition of :progn, and :load-source
;;in the source for make.


;;Dependency feature:

;;   This make NEVER loads or compiles files in an order different from
;;that specified by the tree.  It will omit loading files which are
;;loaded and up to date, but if two files are out of date, the first (in
;;the printed representation of the tree), will always be loaded before
;;the second.  A consequence of this is that circular dependencies can
;;never occur.
;;
;;  If the :make tree contains (a b c d (:depends (c d) (a b))) then c
;;and d depend on a and b, so that if a or b need recompilation then c
;;and d will also be recompiled.  Thus the general form of a :depends
;;clause is (:depends later earlier) where LATER and EARLIER are either
;;a single file or a list of files. Read it as LATER depends on EARLIER.
;;A declaration of a (:depends (c) (d)) would have no effect, since the
;;order in the tree already rules out such a dependence.

;;  An easy way of specifying a linear dependence is by using :serial.
;;The tree (a (:serial b c d) e)  is completely equivalent to the tree
;;(a b c d e (:depends c b)(:depends d (b c))), but with a long list of
;;serial files, it is inconvenient to specify them in the
;;latter representation.

;;A common case is a set of macros whose dependence is serial followed by a set
;;of files whose order is unimportant.  A conventient way of building that
;;tree is
;;
;;(let ((macros '(a b c d))
;;      (files '(c d e f g)))
;;  `((:serial ,@ macros)
;;    ,files
;;    (:depends ,files ,macros)))

;;  The depends clause may occur anywhere within the tree, since
;;an initial pass collects all dependency information.

;;  Make takes a SHOW keyword argument.  It is almost impossible to simulate
;;all the possible features of make, for show.  Nonetheless, it is good
;;to get an idea of the compiling and loading sequence for a new system.
;;As a byproduct, you could use the output, as a simple sequence of calls
;;to compile-file and load, to do the required work, when make is not around
;;to help.


;;*****  Definitions ********
(defvar *files-loaded* nil)
(defvar *show-files-loaded* nil) ;only for show option
(defvar *load* nil "Will be non nil inside load-files")
(defvar *compile* nil "Bound by compile-files to t")
(defvar *depends* nil)
(defvar *depends-new* nil)
(defvar *force* nil)
(defvar *when-compile* nil "Each compile-file evals things in this list and sets it to nil")
#+kcl(defvar *system-p* nil)
(defvar *compile-file-function* 'make-compile-file)
(defvar *load-function* 'make-load-file)
(defvar show nil)
(defvar *cflags* #-kcl nil
  #+kcl '(:system-p  *system-p*))


;;this is the main entry point

(defun make (system &key recompile compile batch object-path source-path
		    show proclaims
		    &aux files *depends* *when-compile*
		    *show-files-loaded*
		    #+gcl (*load-fn-too* proclaims)

		    )

  "SYSTEM is a tree of files, or a symbol with :make property.  It
loads all file files in system.  If COMPILE it will try to compile
files with newer source versions than object versions, before loading.
If RECOMPILE it will recompile all files.  This is equivalent to deleting all
objects and using :compile t.   SOURCE-PATH is merged with the name given
in the files list, when looking for a file to compile.  OBJECT-PATH is
merged with the name in the files list, when looking for a file to
load.  If SYSTEM is a symbol, then a null OBJECT-PATH would be set to
the :object-path property of SYSTEM.  Similarly for :source-path"

  (declare (special object-path source-path show)) batch
  (cond ((symbolp system)
	 (or object-path (setf object-path (get system :object-path)))
	 (or source-path (setf source-path (get system :source-path)))
	 (setf files (get system :make))
	 (or files
	     (if (get system :files)
		 (error "Use :make property, :files property is obssolet{!")))
	 )
	(t (setf files system)))
  #+gcl (when proclaims (compiler::emit-fn t) (compiler::setup-sys-proclaims))
  (let (#+lispm ( si::inhibit-fdefine-warnings
		 (if batch :just-warn  si::inhibit-fdefine-warnings)))
    (let ((*depends*  (if (or compile recompile) (get-depends system)))
	  *depends-new*)
    (dolist (v files)
	    (when (or compile recompile)
		    (compile-files v recompile))
	    (load-files v recompile)))
    #+gcl
    (if proclaims (compiler::write-sys-proclaims))
    ))

(defun system-load (system-name &rest names)
  "If :infor is a system, (system-load :uinfor joe betty) will load
joe and betty from the object-path for :uinfor"
  (load-files names t (get system-name :object-path)))

(defun system-compile (system-name &rest names)
				  
  "If :iunfor is a system, (system-compile :uinfor joe) will in the
source path for joe and compile him into the object path for :uinfor"
  (compile-files names t :source-path
		 (get system-name :source-path) :object-path
		 (get system-name :object-path)))

(defun get-depends (system-name &aux result)
  (dolist (v (get system-name :make))
  (cond    ((atom v) )
	   ((eq (car v) :serial)
	    (do ((w (reverse (cdr v))(cdr w)))
		((null (cdr w)))
		(push (list (car w) (cdr w)) result)))
	   ((eq (car v) :depends)
	    (push (cdr v) result ))))
    result)
	   
#+kcl
(setq si::*default-time-zone* 6)
#+winnt
(setq SYSTEM:*DEFAULT-TIME-ZONE*  (GET-SYSTEM-TIME-ZONE))

(defun print-date (&optional(stream *standard-output*)
			    (time (get-universal-time)))
  (multiple-value-bind (sec min hr day mon yr wkday)
		       (decode-universal-time time)
	(format stream "~a ~a ~a ~d:~2,'0d:~2,'0d ~a"
		(nth wkday '( "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
		(nth (1- mon) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
			   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
		day
		hr min sec yr)))
	       
;;This is an awfully roundabout downcase, but some machines
;;like symbolics swap cases on the pathname, so we have to do an extra 
;;swap!!
(defun lowcase (na &aux (*print-case* :downcase))
  (pathname-name (pathname  (format nil "~a" na))))

(defun our-merge (name path &optional ign  ) ign
  #+lispm (setq name (string-upcase (string name)))
    (make-pathname :name (string name)
		   :type (pathname-type path)
		   :version (pathname-version path)
		   :host (pathname-host path)
		   :directory (pathname-directory path)))


#+kcl
(setf (get :link 'load)
      #'(lambda (path to-link)
	  (declare (special object-path))
	  (si::faslink (our-merge	(lowcase  path) object-path)
		       to-link)))

(setf (get :link 'compile)
      #'(lambda (path to-link) 
	   to-link
	  (compile-files  path *force*)))

(setf (get :progn 'load)
      #'(lambda (&rest args)
	  (eval (cons 'progn args))))

(setf (get :progn 'compile) (get :progn 'load))

(setf (get :load-source 'load)
      #'(lambda (&rest args)
	  (declare (special source-path))
	  (load-files args *force* source-path)))

(setf (get :load-source-when-compile 'compile)
      (get :load-source 'load))

;;should nott use :lisp anymore
(setf (get :lisp 'load)
      #'(lambda (x) (error "please replace :lisp by :load-source")))

(setf (get :serial 'load) #'(lambda (&rest l)(load-files l)))
(setf (get :serial 'compile)
      #'(lambda (&rest l)
	  (dolist (v l)
	    (compile-files v)
	    (load-files v))))


(defun load-files (files &optional (*force* *force*) (object-path object-path)
			 &aux path tem (*load* t))
  (declare (special object-path source-path *force* show))
  (cond ((atom files)
	 (setq path (object files))
	 (cond (show
		(unless (member path *show-files-loaded* :test 'equalp)
			(push path *show-files-loaded*)
			(format t "~%(LOAD ~s)" (namestring path))))
	       ((null *load-function*))
	       ((or *force*
		    (or (not (setq tem
				   (member path *files-loaded*
					   :test 'equalp :key 'car)))
			(> (file-write-date  path) (cdr (car tem)))))
		(funcall *load-function* files)
		(push (cons path (file-write-date path)) *files-loaded*))))
	((keywordp (car files))
	 (let ((fun (get (car files) 'load)))
	   (cond (fun (apply fun (cdr files))))))
	(t (dolist (v files) (load-files v *force*  object-path)))))


(defun file-date (file)
  (if (probe-file file) (or (file-write-date file) 0) 0))

(defun source (file)
  (declare (special source-path))
   (our-merge  (lowcase file) source-path))

(defun object (file)
  (declare (special object-path))
   (our-merge  (lowcase file) object-path))


;;for lisp machines, and others where checking date is slow, this
;;we should try to cache some dates, and then remove them as we do
;;things like compile files...

(defun file-out-dated (file)
  (let ((obj-date (file-date (object file))))
    (or (<= obj-date (file-date (source file)))
	(dolist (v *depends*)
		(cond ((or (and (consp (car v))
				(member file (car v)))
			   (eq (car v) file))
		       (dolist (w (if (consp (second v))
				      (second v) (cdr v)))
			       (cond ((or (<= obj-date (file-date (source w)))
					  (member w *depends-new*))
				      (return-from file-out-dated t))))))))))


(defun make-compile-file ( l)
  (format t "~&Begin compile ~a at ~a~%" l (print-date nil))
  (dolist (v *when-compile*) (eval v))
  (setq *when-compile* nil)
  ;;Franz excl needs pathnames quoted, and some other lisp
  ;;would not allow an apply here.  Sad.
  (eval `(compile-file ',(source l) :output-file ',(object l)
		       ,@ *cflags*))
  (format t "~&End compile ~a at ~a~%" l (print-date nil))

  )

(defvar *load-fn-too* nil)
(defun make-load-file (l)
  (let ((na (object l)))
    (load na)
    (if (and *load-fn-too*
	     (probe-file
	      (setq na
		    (our-merge (lowcase l) (merge-pathnames "foo.fn" na)))))
	(load na))
	
  
  ))

;;these are versions which don't really compile or load files, but
;;do create a new "compiled file" and "fake load" to test date mechanism.
#+debug
(defun make-compile-file (file)
  (format t "~%Fake Compile ~a" (namestring (source file)))
    (dolist (v *when-compile*) (eval v))  (setq *when-compile* nil)
  (with-open-file (st (object file) :direction :output)
		  (format st "(print (list 'hi))")))
#+debug
(defun make-load-file (l)
  (format t "~%Fake loading ~a" (namestring(object l))))

 
		  

(defun compile-files (files &optional (*force*  *force*)
			    &key (source-path source-path)
			    (object-path object-path)
			    &aux
			    (*compile* t) )
  (declare (special object-path source-path *force* show))
  (cond ((atom files)
	 (when (or *force*  (file-out-dated files))
	      (push files  *depends-new*)
	       (cond
		(show
		 (format t "~%(COMPILE-FILE ~s)" (namestring (source files))))
		(t
		 (and *compile-file-function*
		      (funcall *compile-file-function* files))
		 ))))
	((keywordp (car files))
	 (let ((fun (get (car files) 'compile)))
	   (if fun (apply fun (cdr files)))))
	(t (dolist (v files) (compile-files v *force*)))))

;;Return the files for SYSTEM 

(defun system-files (system &aux *files*)
  (declare (special *files*))
  (let ((sys (get system :make)))
    (get-files1 sys))
  (nreverse *files*))

   
(defun get-files1 (sys)
  (declare (special *files*))
  (cond ((and sys (atom sys) )(pushnew sys *files*))
	((eq (car sys) :serial) (get-files1 (cdr sys)))
	((keywordp (car sys)))
	(t (dolist (v sys) (get-files1 v)))))

  
(defmacro make-user-init (files &aux (object-path
				      (if (boundp 'object-path) object-path
					  "foo.o")))
  (declare (special object-path))
    `(progn
       (clines "void gcl_init_or_load1 ();
#define init_or_load(fn,file) do {extern int fn(); gcl_init_or_load1(fn,file);}  while(0)

user_init{") ,@
     (sloop::sloop for x  in files
	for f  = (substitute #\- #\_ (lowcase x))
	for ff =  (namestring (truename (object x)))
	collect
	`(clines ,(Format nil "init_or_load(init_~a,\"~a\");" f ff)))
       (clines "}")))

    
      
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/make.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_info.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
(in-package :SI)

(eval-when (compile eval)
(defmacro while (test &body body)
  `(slooP::sloop while ,test do ,@ body))
 (defmacro f (op x y)
   (list op x y)))

(defconstant +crlu+ #v"")
(defconstant +crnp+ #v"[]")

(defvar *info-data* nil)
(defvar *current-info-data* nil)

(defun file-to-string (file &optional (start 0)
			    &aux (si::*ALLOW-GZIPPED-FILE* t)(len 0))
  (with-open-file
   (st file)
   (setq len (file-length st))
   (or (and (<= 0 start ) (<= start len))
       (error "illegal file start ~a" start))
   (let ((tem (make-array (- len start)
			  :element-type 'character)))
     (if (> start 0) (file-position st start))
     (si::fread tem 0 (length tem) st) tem)))

(defun atoi (string start &aux (ans 0) (ch 0)(len (length string)))
  (declare (string string))
  (declare (fixnum start ans ch len) )
  (while (< start len)
    (setq ch (char-code (aref string start)))
    (setq start (+ start 1))
    (setq ch (- ch #.(char-code #\0)))
    (cond ((and (>= ch 0) (< ch 10))
	   (setq ans (+ ch (* 10 ans))))
	  (t (return nil))))
  ans)
  
(defun info-get-tags (file &aux (lim 0) *match-data* tags files
			   (*case-fold-search* t))
  (declare (fixnum lim))
  (let ((s (file-to-string file)) (i 0))
    (declare (fixnum i) (string s))
    (cond ((f >= (string-match #v"[\n]+Indirect:" s 0) 0)
	   (setq i (match-end 0))
	   (setq lim (string-match +crlu+ s i))
	   (while
	       (f >= (string-match #v"\n([^\n]+): ([0-9]+)" s i lim) 0)
	     (setq i (match-end 0))
	     (setq files
		   (cons(cons
			 (atoi s (match-beginning 2))
			 (get-match s 1)
			 )
			files)))))
    (cond ((f >=  (si::string-match #v"[\n]+Tag Table:" s i) 0)
	   (setq i (si::match-end 0))
	   (cond ((f >= (si::string-match +crlu+ s i) 0)
		  (setq tags (subseq s i (si::match-end 0)))))))
    (if files (or tags (info-error "Need tags if have multiple files")))
    (list* tags (nreverse files))))

(defun re-quote-string (x &aux (i 0) ch)
  (declare (fixnum i))
  (let* ((x (if (stringp x) x (string x)))
	 (len (length x))
	 (tem x))
    (while (< i len)
      (setq ch (aref x i))
      (when (position ch "\\()[]+.*|^$?")
	(when (eq x tem)
	  (setq tem 
		(make-array len :adjustable t
			    :element-type 'character :fill-pointer 0))
	  (dotimes (j i) (setf (aref tem j) (aref x j))))
	(vector-push-extend #\\ tem))
      (unless (eq tem x) (vector-push-extend ch tem))
      (setq i (+ i 1)))
    (remove-if-not 'standard-char-p tem)))

(defun get-match (string i)
  (subseq string (match-beginning i) (match-end i)))

(defun get-nodes (pat node-string &aux (i 0) ans
		      (*case-fold-search* t) *match-data*)
  (declare (fixnum i))
  (when node-string
	(setq pat
	      (si::string-concatenate "Node: ([^]*" (re-quote-string
						       pat) "[^]*)"))
	(while (f >= (string-match pat node-string i) 0)
	  (setq i (match-end 0))
	  (setq ans (cons (get-match node-string 1) 
			  ans))
	  )
	(nreverse ans)))

(defun get-index-node ()
 (or (third *current-info-data*) 
     (let* (
	    s
	    (node-string (car (nth 1 *current-info-data*)))
	    (node
	     (and node-string (car (get-nodes "index" node-string)))))
       (when node
	   (setq s (show-info
		    node
		    nil
		    nil
		    ))
	(setf (third *current-info-data*) s)))))

(defun nodes-from-index (pat  &aux (i 0) ans
			      (*case-fold-search* t) *match-data*)
  (let ((index-string (get-index-node)))
    (when index-string
    (setq pat 
	  (si::string-concatenate #u"\n\\* ([^:\n]*" (re-quote-string
						  pat)
				  #u"[^:\n]*):[ \t]+([^\t\n,.]+)"))
    (while (f >= (string-match pat index-string i) 0)
      (setq i (match-end 0))
      (setq ans (cons (cons (get-match index-string 1)
			    (get-match index-string 2))
			  
			  
		      ans))
      )
    (nreverse ans))))

(defun get-node-index (pat node-string &aux (node pat) *match-data*)
  (cond ((null node-string) 0)
	(t
	 (setq pat
	       (si::string-concatenate "Node: "
				       (re-quote-string pat) "([0-9]+)"))
	 (cond ((f >= (string-match pat node-string) 0)
		(atoi node-string (match-beginning 1)))
	       (t (info-error "cant find node ~s" node) 0)))))

(defun all-matches (pat st &aux (start 0) *match-data*)
  (declare (fixnum start))
   (sloop::sloop while (>= (setq start (si::string-match pat st start)) 0)
         do nil;(print start)
         collect (list start (setq start (si::match-end 0)))))



(defmacro node (prop x)
  `(nth ,(position prop '(string begin end header name
				 info-subfile
				 file tags)) ,x)) 

(defun node-offset (node)
  (+ (car (node info-subfile node)) (node begin node)))

(defvar *info-paths*
  '("" "/usr/info/" "/usr/local/lib/info/" "/usr/local/info/"
    "/usr/local/gnu/info/" "/usr/share/info/"))

(defvar *old-lib-directory* nil)
(defun setup-info (name &aux tem file)
  (or (eq *old-lib-directory* si::*lib-directory*)
      (progn
	(setq *old-lib-directory* si::*lib-directory*)
	(push (si::string-concatenate
	       si::*lib-directory* "info/") *info-paths*)
	(setq *info-paths* (si::fix-load-path *info-paths*))))
  (cond ((or (equal name "DIR"))
	 (setq name "dir")))
;; compressed info reading -- search for gzipped files, and open with base filename
;; relying on si::*allow-gzipped-files* to uncompress
  (setq file (si::file-search name *info-paths* '("" ".info" ".gz") nil))
  (let ((ext (search ".gz" file)))
    (when ext
      (setq file (subseq file 0 ext))))
  (cond ((and (null file)
	      (not (equal name "dir")))
	 (let* ((tem (show-info "(dir)Top" nil nil))
		*case-fold-search*)
	   (cond ((f >= (string-match
			 (si::string-concatenate "\\(([^(]*" (re-quote-string name) "(.info)?)\\)")
			 tem)
		     0)
		  (setq file (get-match tem 1)))))))
  (cond (file
;	 (let* ((na (namestring (truename file))))
	 (let* ((na (namestring file)))
	   (cond ((setq tem (assoc na *info-data* :test 'equal))
		  (setq *current-info-data* tem))
		 (t (setq *current-info-data*
			  (list na (info-get-tags na) nil))
		    (setq *info-data* (cons *current-info-data* *info-data*))))))
	(t (format t "(not found ~s)" name)))
  nil)
			  
(defun get-info-choices (pat type)
      (if (eql type 'index)
	  (nodes-from-index pat )
	(get-nodes pat (car (nth 1 *current-info-data*)))))

(defun add-file (v file &aux (lis v))
  (while lis
    (setf (car lis) (list (car lis) file))
    (setq lis (cdr lis)))
  v)

(defvar *info-window* nil)
(defvar *tk-connection* nil)

(defun info-error (&rest l)
  (if *tk-connection*
      (tk::tkerror (apply 'format nil l))
    (apply 'error l)))

(defvar *last-info-file* nil)
;; cache last file read to speed up lookup since may be gzipped..
(defun info-get-file (pathname)
  (setq pathname
	(merge-pathnames pathname
			 (car *current-info-data*)))
  (cdr 
   (cond ((equal (car *last-info-file*) pathname)
	  *last-info-file*)
	 (t (setq *last-info-file*
		  (cons pathname (file-to-string pathname)))))))

(defun waiting (win)
  (and *tk-connection*
       (fboundp win)
       (winfo :exists win :return 'boolean)
       (funcall win :configure :cursor "watch")))

(defun end-waiting (win) (and (fboundp win)
			   (funcall win :configure :cursor "")))

(defun info-subfile (n  &aux )
;  "For an index N return (START . FILE) for info subfile
; which contains N.   A second value bounding the limit if known
; is returned.   At last file this limit is nil."
  (let ((lis (cdr (nth 1 *current-info-data*)))
	ans lim)
    (and lis (>= n 0)
	   (dolist (v lis)
		 (cond ((> (car v) n )
			(setq lim (car v))
			(return nil)))
		 (setq ans v)
		 ))
    (values (or ans (cons 0 (car *current-info-data*))) lim)))

;;used by search
(defun info-node-from-position (n &aux  (i 0))
  (let* ((info-subfile (info-subfile n))
	 (s (info-get-file (cdr info-subfile)))
	 (end (- n (car info-subfile))))
    (while (f >=  (string-match +crlu+ s i end) 0)
      (setq i (match-end 0)))
    (setq i (- i 1))
    (if (f >= (string-match
	       #v"[\n][^\n]*Node:[ \t]+([^\n\t,]+)[\n\t,][^\n]*\n"  s i) 0)
	(let* ((i (match-beginning 0))
	       (beg (match-end 0))
	       (name (get-match s 1))
	       (end(if (f >= (string-match +crnp+ s beg) 0)
		       (match-beginning 0)
		     (length s)))
	       (node (list* s beg end i name info-subfile
				 *current-info-data*)))
	  node))))
    
(defun show-info (name  &optional position-pattern
			(use-tk *tk-connection*)
			&aux info-subfile *match-data* 
			file
		       (initial-offset 0)(subnode -1))
  (declare (fixnum subnode initial-offset))
;;; (pat . node)
;;; node
;;; (node file)
;;; ((pat . node) file)
;  (print (list name position-pattern use-tk))
  (progn ;decode name
    (cond ((and (consp name) (consp (cdr name)))
	   (setq file (cadr name)
		 name (car name))))
    (cond ((consp name)
	   (setq position-pattern (car name) name (cdr name)))))
  (or (stringp name) (info-error "bad arg"))
  (waiting *info-window*)  
  (cond ((f >= (string-match #v"^\\(([^(]+)\\)([^)]*)" name) 0)
	 ;; (file)node
	 (setq file (get-match name 1))
	 (setq name (get-match name 2))
	 (if (equal name "")(setq name "Top"))))
  (if file  (setup-info file))
  (let ((indirect-index (get-node-index name
					(car (nth 1 *current-info-data*)))))
    (cond ((null  indirect-index)
	   (format t"~%Sorry, Can't find node ~a" name)
	   (return-from show-info nil)))
	
    (setq info-subfile (info-subfile indirect-index))
    (let* ((s
	    (info-get-file (cdr info-subfile)))
	   (start (- indirect-index (car info-subfile))))
      (cond ((f >= (string-match
		    ;; to do fix this ;; see (info)Add  for description;
		    ;;  the 
		    (si::string-concatenate
		     #u"[\n][^\n]*Node:[ \t]+"
		     (re-quote-string name) #u"[,\t\n][^\n]*\n") 
		    s start) 0)
	     (let* ((i (match-beginning 0))
		    (beg (match-end 0))
		    (end(if (f >= (string-match +crnp+ s beg) 0)
			    (match-beginning 0)
			  (length s)))
		    (node (list* s beg end i name info-subfile
				 *current-info-data*)))

	       (cond
		(position-pattern
		 (setq position-pattern (re-quote-string position-pattern))

		 (let (*case-fold-search* )
		   (if (or
			(f >= (setq subnode
				    (string-match
				     (si::string-concatenate
				      #u"\n -+ [A-Za-z ]+: "
				      position-pattern #u"[ \n]")
				     s beg end)) 0)
			(f >= (string-match position-pattern s beg end) 0))
		       (setq initial-offset
			     (- (match-beginning 0) beg))
		     ))))
	       (cond ( use-tk
		       (prog1 (print-node node initial-offset)
			 (end-waiting  *info-window*))
		       )
		     (t
		      (let ((e
			     (if (and (>= subnode 0)
				      (f >=
					 (string-match 
					  #v"\n -+ [a-zA-Z]"
					  s 
					  (let* ((bg (+ beg 1 initial-offset))
						 (sd (string-match #v"\n   " s bg end))
						 (nb (if (minusp sd) bg sd)))
					    nb) 
						       end)
					 0))
				 (match-beginning 0)
			       end)))
			;(print (list  beg initial-offset e end))
			(subseq s (+ initial-offset beg) e )
			;s
			)))))
	    (t (info-error "Cant find node  ~a?" name)
	       (end-waiting  *info-window*)
	       ))
	    )))

(defvar *default-info-files* '( "gcl-si.info" "gcl-tk.info" "gcl.info"))

(defun info-aux (x dirs)
  (sloop for v in dirs
		    do (setup-info v)
		    append (add-file (get-info-choices x 'node) v)
		    append (add-file (get-info-choices x 'index) v)))

(defun info-search (pattern &optional start end &aux limit)
;  "search for PATTERN from START up to END where these are indices in
;the general info file.   The search goes over all files."
  (or start (setq start 0))
  (while start
    (multiple-value-bind
     (file lim)
     (info-subfile start)
     (setq limit lim)
     (and end limit (<  end limit) (setq limit end))

     (let* ((s  (info-get-file (cdr  file)))
	   (beg (car file))
	   (i (- start beg))
	   (leng (length s)))
       (cond ((f >= (string-match pattern s i (if limit (- limit beg) leng)) 0)
	      (return-from info-search (+ beg (match-beginning 0))))))
     (setq start lim)))
  -1)

#+debug ; try searching
(defun try (pat &aux (tem 0) s )
 (while (>= tem 0)
  (cond ((>= (setq tem (info-search pat tem)) 0)
	 (setq s (cdr *last-info-file*))
	 (print (list
		 tem
		 (list-matches s 0 1 2)
		 (car *last-info-file*)
		 (subseq s
			 (max 0 (- (match-beginning 0) 50))
			 (min (+ (match-end 0) 50) (length s)))))
	 (setq tem (+ tem (- (match-end 0) (match-beginning 0))))))))
   
(defun idescribe (name)
    (let* ((items (info-aux name *default-info-files*)))
      (dolist (v items)
	      (when (cond ((consp (car v))
			   (equalp (caar v) name))
			  (t (equalp (car v) name)))
		(format t "~%From ~a:~%" v)
		(princ (show-info v nil nil))))))
  
(defun info (x &optional (dirs *default-info-files*)  &aux wanted
	       *current-info-data* file position-pattern)
  (unless (consp dirs)
    (setq dirs *default-info-files*))
  (let ((tem (info-aux x dirs)))
    (cond
     (*tk-connection*
      (offer-choices tem dirs)
       )
     (t

    (when tem
      (let ((nitems (length tem)))
	  (sloop for i from 0 for name in tem with prev
		 do (setq file nil position-pattern nil)
		 (progn ;decode name
		   (cond ((and (consp name) (consp (cdr name)))
			  (setq file (cadr name)
				name (car name))))
		   (cond ((consp name)
			  (setq position-pattern (car name) name (cdr name)))))
		 (format t "~% ~d: ~@[~a :~]~@[(~a)~]~a." i
			 position-pattern
			 (if (eq file prev) nil (setq prev file)) name))
  	  (if (> (length tem) 1)
	    (format t "~%Enter n, all, none, or multiple choices eg 1 3 : ")
	    (terpri))
	  (let ((line (if (> (length tem) 1) (read-line) "0"))
	        (start 0)
	        val)
	    (while (equal line "") (setq line (read-line)))
	    (while (multiple-value-setq
		    (val start)
		    (read-from-string line nil nil :start start))
	      (cond ((numberp val)
		     (setq wanted (cons val wanted)))
		    (t (setq wanted val) (return nil))))
	    (cond ((consp wanted)(setq wanted (nreverse wanted)))
		  ((symbolp wanted)
		   (setq wanted (and
				 (equal (symbol-name wanted) "ALL")
				 (sloop for i below (length tem) collect i)))))
	    (when wanted
 	      ;; Remove invalid (numerical) answers
	      (setf wanted (remove-if #'(lambda (x)
					  (and (integerp x) (>= x nitems)))
				      wanted))
	      (format t "~%Info from file ~a:" (car *current-info-data*)))
	    (sloop for i in wanted
		   do (princ(show-info (nth i tem)))))))))))

	     
;; idea make info_text window have previous,next,up bindings on keys
;; and on menu bar.    Have it bring up apropos menu. allow selection
;; to say spawn another info_text window.   The symbol that is the window
;; will carry on its plist the prev,next etc nodes, and the string-to-file
;; cache the last read file as well.   Add look up in index file, so that can
;; search an indtqex as well.   Could be an optional arg to show-node
;; 



(defun default-info-hotlist()
  (namestring (merge-pathnames "hotlist" (user-homedir-pathname))))

(defvar *info-window* nil)

(defun add-to-hotlist (node )
  (if (symbolp node) (setq node (get node 'node)))
  (cond
   (node
    (with-open-file
     (st (default-info-hotlist)
	 :direction :output
	 :if-exists :append
	 :if-does-not-exist :create)
     (cond ((< (file-position st) 10)
	    (princ  #u"\nFile:\thotlist\tNode: Top\n\n* Menu: Hot list of favrite info items.\n\n" st)))
     (format st "* (~a)~a::~%" 
	     (node file node)(node name node))))))

(defun list-matches (s &rest l)
  (sloop for i in l 
	 collect
	 (and (f >= (match-beginning i) 0)
	      (get-match s i))))


;;; Local Variables: ***
;;; mode:lisp ***
;;; version-control:t ***
;;; comment-column:0 ***
;;; comment-start: ";;; " ***
;;; End: ***


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_info.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_trace.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;        trace.lsp 
;;;;
;;;;        Tracer package for Common Lisp

;;;;;; Modified by Matt Kaufmann to allow tracing options.


;; If you are working in another package you should (import 'si::arglist)
;; to avoid typing the si::

;; (in-package 'lisp)

;; (export '(trace untrace))
;; (export 'step)


(in-package :system)

;;(proclaim '(optimize (safety 2) (space 3)))


(defvar *trace-level* 0)
(defvar *trace-list* nil)


(defmacro trace (&rest r)
  (if (null r)
      '(mapcar 'car *trace-list*)
    `(let ((old (copy-list *trace-list*)) finish-flg)
       (unwind-protect
	   (prog1 (mapcan 'trace-one ',r)
	     (setq finish-flg t))
	 (when (null finish-flg)
	       (format *standard-output* "~%Newly traced functions:  ~S"
		       (mapcar 'car (set-difference *trace-list* old :test #'equal))))))))

(defmacro untrace (&rest r)
  `(mapcan 'untrace-one ',(or r (mapcar 'car *trace-list*))))

(defun trace-one-preprocess (x)
  (if (symbolp x)
      (trace-one-preprocess (list x))
    (do ((tail (cdr x) (cddr tail))
	 (declarations)
	 (entryform `(cons (quote ,(car x)) arglist))
	 (exitform `(cons (quote ,(car x)) values))
	 (condform t)
	 (entrycondform t)
	 (exitcondform t)
	 (depth) (depthvar))
	((null tail)
	 (when depth
	   ;; Modify the :cond so that it first checks depth, and then
	   ;; modify the :entry so that it first increments depth.  Notice
	   ;; that :cond will be fully evaluated before depth is incremented.
	   (setq depthvar (gensym))
	   ;; now reset the condform
	   (if
	       (eq condform t)
	       (setq condform
		     `(< ,depthvar ,depth))
	     (setq condform `(if (< ,depthvar ,depth) ,condform nil)))
	   (setq declarations (cons (cons depthvar 0) declarations))
	   ;; I'll have the depth be incremented for all the entry stuff and no exit stuff,
	   ;; since I don't see any more uniform, logical way to do this.
	   (setq entrycondform
		 `(progn
		    (setq ,depthvar (1+ ,depthvar))
		    ,entrycondform))
	   (setq exitcondform
		 `(progn
		    (setq ,depthvar (1- ,depthvar))
		    ,exitcondform)))
	 `(,(car x) ,declarations
	   (quote ,condform)
	   (quote ,entrycondform) (quote ,entryform)
	   (quote ,exitcondform) (quote ,exitform)))
	(case (car tail)
	      (:declarations
	       (setq declarations
		     (do ((decls (cadr tail) (cdr decls))
			  (result))
			 ((null decls) result)
			 (setq result
			       (cons (if (symbolp (car decls))
					 (cons (car decls) nil)
				       (cons (caar decls) (eval (cadar decls))))
				     result)))))
	      (:cond (setq condform (cadr tail)))
	      (:entrycond (setq entrycondform (cadr tail)))
	      (:entry (setq entryform (cadr tail)))
	      (:exitcond (setq exitcondform (cadr tail)))
	      (:exit (setq exitform (cadr tail))) 
	      (:depth (setq depth (cadr tail)))
	      (otherwise nil)))))

(defun check-trace-spec (form)
  (or (symbolp form)
      (if (and (consp form) (null (cdr (last form))))
	  (check-trace-args form (cdr form) nil)
	(error "Each trace spec must be a symbol or a list terminating in NIL, but ~S is not~&."
	       form))))

(defun check-declarations (declarations &aux decl)
  (when (consp declarations)
	(setq decl (if (consp (car declarations)) (car declarations) (list (car declarations) nil)))
	(when (not (symbolp (car decl)))
	      (error "Declarations are supposed to be of symbols, but ~S is not a symbol.~&"
		     (car decl)))
	(when (cddr decl)
	      (error "Expected a CDDR of NIL in ~S.~&"
		     decl))
	(when (assoc (car decl) (all-trace-declarations))
	      (error "The variable ~A is already declared for tracing"
		     (car decl)))))

(defun check-trace-args (form args acc-keywords)
  (when args
	(cond
	 ((null (cdr args))
	  (error "A trace spec must have odd length, but ~S does not.~&"
		 form))
	 ((member (car args) acc-keywords)
	  (error "The keyword ~A occurred twice in the spec ~S~&"
		 (car args) form))
	 (t
	  (case (car args)
		((:entry :exit :cond :entrycond :exitcond)
		 (check-trace-args form (cddr args) (cons (car args) acc-keywords)))
		(:depth
		 (when (not (and (integerp (cadr args))
				 (> (cadr args) 0)))
		       (error
			"~&Specified depth should be a positive integer, but~&~S is not.~&"
			(cadr args)))
		 (check-trace-args form (cddr args) (cons :depth acc-keywords)))
		(:declarations
		 (check-declarations (cadr args))
		 (check-trace-args form (cddr args) (cons :declarations acc-keywords)))
		(otherwise
		 (error "Expected :entry, :exit, :cond, :depth, or :declarations~&~
                         in ~S where instead there was ~S~&"
			form (car args))))))))

(defun trace-one (form &aux f)
   (let* ((n (funid-sym-p form))
	  (n1 (or n (funid-sym (car form))))
	  (ofname (if n form (car form)))
	  (form (or n (cons n1 (cdr form))))
	  (fname n1))
     (check-trace-spec form)
     (when (null (fboundp fname))
       (format *trace-output* "The function ~S is not defined.~%" fname)
       (return-from trace-one nil))
     (when (special-operator-p fname)
       (format *trace-output* "~S is a special form.~%" fname)
       (return-from trace-one nil))
     (when (macro-function fname)
       (format *trace-output* "~S is a macro.~%" fname)
       (return-from trace-one nil))
     (when (get fname 'traced)
       (untrace-one ofname))
     (setq form (trace-one-preprocess form))
     (let ((x (get fname 'state-function))) (when x (break-state 'trace x)))
     (fset (setq f (gensym)) (symbol-function fname))
     (eval `(defun ,fname (&rest args) (trace-call ',f args ,@(cddr form))))
     (putprop fname f 'traced)
     (setq *trace-list* (cons (cons ofname (cadr form)) *trace-list*))
     (list ofname)))

(defun reset-trace-declarations (declarations)
  (when declarations
	(set (caar declarations) (cdar declarations))
	(reset-trace-declarations (cdr declarations))))

(defun all-trace-declarations ( &aux result)
  (dolist (v *trace-list*)
	  (setq result (append result (cdr v))))
  result)
	  
(defun trace-call (temp-name args cond entrycond entry exitcond exit
			 &aux (*trace-level* *trace-level*) (*print-circle* t) vals indent)
  (when (= *trace-level* 0)
	(reset-trace-declarations (all-trace-declarations)))
  (cond
   ((eval `(let ((arglist (quote ,args))) ,cond))
    (setq *trace-level* (1+ *trace-level*))
    (setq indent (min (* *trace-level* 2) 20))
    (fresh-line *trace-output*)
    (when (or (eq entrycond t)		;optimization for common value
	      (eval `(let ((arglist (quote ,args))) ,entrycond)))
	  ;; put out the prompt before evaluating
	  (format *trace-output*
		  "~V@T~D> "
		  indent *trace-level*)
	  (format *trace-output*
		  "~S~%"
		  (eval `(let ((arglist (quote ,args))) ,entry)))
	  (fresh-line *trace-output*))
    (setq vals (multiple-value-list (apply temp-name args)))
    (when (or (eq exitcond t)		;optimization for common value
	      (eval `(let ((arglist (quote ,args)) (values (quote ,vals)))
		       ,exitcond)))
	  ;; put out the prompt before evaluating
	  (format *trace-output*
		  "~V@T<~D "
		  indent
		  *trace-level*) 
	  (format *trace-output*
		  "~S~%"
		  (eval `(let ((arglist (quote ,args)) (values (quote ,vals))) ,exit))))
    (setq *trace-level* (1- *trace-level*))
    (values-list vals))
   (t (apply temp-name args))))

(defun traced-sym (fname)
  (let* ((sym (when (symbolp fname) (get fname 'traced)))
	 (fn (when (and sym (symbolp sym) (fboundp fname)) 
	       (function-lambda-expression (symbol-function fname))))
	 (fn (and (consp fn) (third fn)))
	 (fn (and (consp fn) (third fn))))
    (and (consp fn) (eq (car fn) 'trace-call) sym)))

(defun untrace-one (fname)
  (let* ((ofname fname)
	 (fname (funid-sym fname))
	 (sym (traced-sym fname))
	 (g (get fname 'traced)))
    (unless sym
      (cond ((not g) (warn "The function ~S is not traced.~%" fname))
	    ((fboundp fname) (warn "The function ~S was traced, but redefined.~%" ofname))
	    ((warn "The function ~S was traced, but is no longer defined.~%"  ofname))))
    (remprop fname 'traced)
    (setq *trace-list* (delete-if #'(lambda (u) (equal (car u) ofname)) *trace-list* :count 1))
    (when sym
      (fset fname (symbol-function sym)))
    (when g (list ofname))))

#| Example of tracing a function "fact" so that only the outermost call is traced.

(defun fact (n) (if (= n 0) 1 (* n (fact (1- n)))))

;(defvar in-fact nil)
(trace (fact :declarations ((in-fact nil))
	     :cond
	     (null in-fact)
	     :entry
	     (progn
	       (setq in-fact t)
	       (princ "Here comes input ")
	       (cons 'fact arglist))
             :exit
             (progn (setq in-fact nil)
		    (princ "Here comes output ")
                    (cons 'fact values))))

; Example of tracing fact so that only three levels are traced

(trace (fact :declarations
	     ((fact-depth 0))
	     :cond
	     (and (< fact-depth 3)
		  (setq fact-depth (1+ fact-depth)))
	     :exit
	     (progn (setq fact-depth (1- fact-depth)) (cons 'fact values))))
|#



(defvar *step-level* 0)
(defvar *step-quit* nil)
(defvar *step-function* nil)

(defvar *old-print-level* nil)
(defvar *old-print-length* nil)


(defun step-read-line ()
  (do ((char (read-char *debug-io*) (read-char *debug-io*)))
      ((or (char= char #\Newline) (char= char #\Return)))))

(defmacro if-error (error-form form)
  (let ((v (gensym)) (f (gensym)) (b (gensym)))
    `(let (,v ,f)
       (block ,b
         (unwind-protect (setq ,v ,form ,f t)
           (return-from ,b (if ,f ,v ,error-form)))))))

(defmacro step (form)
  `(let* ((*old-print-level* *print-level*)
          (*old-print-length* *print-length*)
          (*print-level* 2)
          (*print-length* 2))
     (read-line)
     (format *debug-io* "Type ? and a newline for help.~%")
     (setq *step-quit* nil)
     (stepper ',form nil)))

(defun stepper (form &optional env
                &aux values (*step-level* *step-level*) indent)
  (when (eq *step-quit* t)
    (return-from stepper (evalhook form nil nil env)))
  (when (numberp *step-quit*)
    (if (>= (1+ *step-level*) *step-quit*)
        (return-from stepper (evalhook form nil nil env))
        (setq *step-quit* nil)))
  (when *step-function*
    (if (and (consp form) (eq (car form) *step-function*))
        (let ((*step-function* nil))
          (return-from stepper (stepper form env)))
        (return-from stepper (evalhook form #'stepper nil env))))
  (setq *step-level* (1+ *step-level*))
  (setq indent (min (* *step-level* 2) 20))
  (loop
    (format *debug-io* "~VT~S " indent form)
    (finish-output *debug-io*)
    (case (do ((char (read-char *debug-io*) (read-char *debug-io*)))
              ((and (char/= char #\Space) (char/= char #\Tab)) char))
          ((#\Newline #\Return)
           (setq values
                 (multiple-value-list
                  (evalhook form #'stepper nil env)))
           (return))
          ((#\n #\N)
           (step-read-line)
           (setq values
                 (multiple-value-list
                  (evalhook form #'stepper nil env)))
           (return))
          ((#\s #\S)
           (step-read-line)
           (setq values
                 (multiple-value-list
                  (evalhook form nil nil env)))
           (return))
          ((#\p #\P)
           (step-read-line)
           (write form
                  :stream *debug-io*
                  :pretty t :level nil :length nil)
           (terpri))
          ((#\f #\F)
           (let ((*step-function*
                  (if-error nil
                            (prog1 (read-preserving-whitespace *debug-io*)
                                   (step-read-line)))))
             (setq values
                   (multiple-value-list
                    (evalhook form #'stepper nil env)))
             (return)))
          ((#\q #\Q)
           (step-read-line)
           (setq *step-quit* t)
           (setq values
                 (multiple-value-list
                  (evalhook form nil nil env)))
           (return))
          ((#\u #\U)
           (step-read-line)
           (setq *step-quit* *step-level*)
           (setq values
                 (multiple-value-list
                  (evalhook form nil nil env)))
           (return))
          ((#\e #\E)
           (let ((env1 env))
             (dolist (x
                      (if-error nil
                                (multiple-value-list
                                 (evalhook
                                  (if-error nil
                                            (prog1
                                             (read-preserving-whitespace
                                              *debug-io*)
                                             (step-read-line)))
                                  nil nil env1))))
                     (write x
                            :stream *debug-io*
                            :level *old-print-level*
                            :length *old-print-length*)
                     (terpri *debug-io*))))
          ((#\r #\R)
           (let ((env1 env))
             (setq values
                   (if-error nil
                             (multiple-value-list
                              (evalhook
                               (if-error nil
                                         (prog1
                                          (read-preserving-whitespace
                                           *debug-io*)
                                          (step-read-line)))
                               nil nil env1)))))
           (return))
          ((#\b #\B)
           (step-read-line)
           (let ((*ihs-base* (1+ *ihs-top*))
                 (*ihs-top* (1- (ihs-top)))
                 (*current-ihs* *ihs-top*))
             (simple-backtrace)))
          (t
           (step-read-line)
           (terpri)
           (format *debug-io*
                  "Stepper commands:~%~
		n (or N or Newline):	advances to the next form.~%~
		s (or S):		skips the form.~%~
		p (or P):		pretty-prints the form.~%~
                f (or F) FUNCTION:	skips until the FUNCTION is called.~%~
                q (or Q):		quits.~%~
                u (or U):		goes up to the enclosing form.~%~
                e (or E) FORM:		evaluates the FORM ~
					and prints the value(s).~%~
                r (or R) FORM:		evaluates the FORM ~
					and returns the value(s).~%~
                b (or B):		prints backtrace.~%~
		?:			prints this.~%")
           (terpri))))
  (when (or (constantp form) (and (consp form) (eq (car form) 'quote)))
        (return-from stepper (car values)))
  (if (endp values)
      (format *debug-io* "~V@T=~%" indent)
      (do ((l values (cdr l))
           (b t nil))
          ((endp l))
        (if b
            (format *debug-io* "~V@T= ~S~%" indent (car l))
            (format *debug-io* "~V@T& ~S~%" indent (car l)))))
  (setq *step-level* (- *step-level* 1))
  (values-list values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_trace.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_arraylib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;    arraylib.lsp
;;;;
;;;;                            array routines


;; (in-package :lisp)

(in-package :system)

;; (use-package :s)

;; (export 'strcat)

;; (defun strcat (&rest r)
;;   (declare (:dynamic-extent r))
;;   (apply 'string-concatenate (mapcar 'string-downcase r)))

(eval-when 
 (compile eval)

 (defun proto-array (tp) (make-vector tp 1 nil nil nil 0 nil nil))

 ;(car (assoc x s::+ks+ :test (lambda (x y) (subtypep x (get y 'compiler::lisp-type)))));FIXME vs bug in interpreter
 ;; (defun af (x &aux (x (caar (member x s::+ks+ :test (lambda (x y) (subtypep x (get (car y) 'compiler::lisp-type))))))) 
 ;;   (intern (string-concatenate "*" (string (or x :object))) :s))

 (defun af (x) (cdr (assoc x '((character . *char) (bit . *char) (non-negative-char . *char);fixme
			       (unsigned-char . *uchar) (signed-char . *char)
			       #+64bit (non-negative-int . *int) #+64bit (unsigned-int . *uint) #+64bit (signed-int . *int)
			       (non-negative-short . *short) (unsigned-short . *ushort)
			       (signed-short . *short) (short-float . *float) (long-float . *double)
			       (t . *object) (non-negative-fixnum . *fixnum) (fixnum . *fixnum)))))
 
 (defvar *array-type-info* (mapcar (lambda (x &aux (y (proto-array x))) 
				     (list x (c-array-elttype y) (c-array-eltsize y) (c-array-mode y) (af x)))
				   +array-types+))

 (defun maybe-cons (car cdr)
   (if (cdr cdr) (cons car cdr) (car cdr))))

#.`(defun set-array (r i s j &optional sw);assumes arrays of same type and indices in bounds
     (declare (optimize (safety 1))(seqind i j))
     (check-type r array)
     (check-type s array)
     (flet ((sp (r i s j gf sf &aux (x (when sw (funcall gf r i)))) 
		(funcall sf (funcall gf s j) r i)
		(when sw (funcall sf x s j))))
       (case 
	   (c-array-eltsize r)
	 ,@(mapcar (lambda (x &aux (z (pop x)) (y (maybe-cons 'or (mapcar (lambda (x) (list 'array x)) x)))
			      (w (fifth (assoc (car x) *array-type-info*))))
		     `(,z (infer-tp 
			   r ,y (infer-tp 
				 s ,y 
				 (sp r i s j 
				     ,(if (zerop z) `'0-byte-array-self `(lambda (r i) (,w (c-array-self r) i nil nil)))
				     ,(if (zerop z) `'set-0-byte-array-self `(lambda (v r i) (,w (c-array-self r) i t v))))))))
		   (mapcar (lambda (x) 
			     (cons x (mapcar 'car (lremove-if-not (lambda (y) (= x (caddr y))) *array-type-info*))))
			   (mapcar 'caddr (lremove nil *array-type-info*)))))))
(declaim (inline set-array))

#.`(defun array-element-type (x)
     (declare (optimize (safety 1)))
     (check-type x array)
     (case
	 (c-array-elttype x)
       ,@(mapcar (lambda (x &aux (tp (pop x))) `(,(car x) ',tp)) *array-type-info*)))

;; (defmacro check-bounds (a i)
;;   `(let ((q (array-total-size ,a)))
;;      (unless (< ,i q) (error 'type-error :datum ,i :expected-type `(integer 0 (,q))))))

#.`(defun row-major-aref-int (a i)
     (ecase
	 (c-array-elttype a)
       ,@(mapcar (lambda (y &aux (x (pop y)))
		   `(,(pop y) 
		     ,(case x
			(character `(code-char (*uchar (c-array-self a) i nil nil)))
			(bit `(0-byte-array-self a i))
			(otherwise `(,(caddr y) (c-array-self a) i nil nil)))))
		 *array-type-info*)))
(declaim (inline row-major-aref-int))

(defun row-major-aref (a i)
     (declare (optimize (safety 1)))
     (check-type a array)
     (check-type i seqind)
     (assert (< i (array-total-size a)) (i) 'type-error :datum i :expected-type `(integer 0 (,(array-total-size a))))
     (row-major-aref-int a i))

#.`(defun row-major-aset (v a i)
     (declare (optimize (safety 1)))
     (check-type a array)
     (check-type i seqind)
     (ecase
	 (c-array-elttype a)
       ,@(mapcar (lambda (y &aux (x (pop y)))
		   `(,(pop y) 
		     ,(case x
			(character `(progn (*uchar (c-array-self a) i t (char-code v)) v))
			(bit `(set-0-byte-array-self v a i))
			(otherwise `(,(caddr y) (c-array-self a) i t v)))))
		 *array-type-info*)))
(setf (get 'row-major-aset 'consider-inline) t)



(defun 0-byte-array-self (array index)
  (declare (optimize (safety 1)))
  (check-type array (array bit))
  (check-type index seqind)
  (let* ((off (+ index (c-array-offset array)))
	 (ind (ash off -3))
	 (byte (*uchar (c-array-self array) ind nil nil))
	 (shft (logand off 7))
	 (shft (- shft #+clx-little-endian 7)))
    (logand (ash byte shft) 1)))

(defun set-0-byte-array-self (bit array index)
  (declare (optimize (safety 1)))
  (check-type array (array bit))
  (check-type index seqind)
  (check-type bit bit)
  (let* ((off (+ index (c-array-offset array)))
	 (ind (ash off -3))
	 (byte (*uchar (c-array-self array) ind nil nil))
	 (shft (logand off 7))
	 #+clx-little-endian (shft (- 7 shft))
	 (val (ash 1 shft)))
    (*uchar (c-array-self array) ind t (if (zerop bit) (logandc2 byte val) (logior byte val)))
    bit))

(defun array-row-major-index (array &rest indices)
  (declare (:dynamic-extent indices)(optimize (safety 2)))
  (check-type array array)
  (assert (apply 'array-in-bounds-p array indices));FIXME type-error??
  (labels ((cpt (i j k l) (the seqind (if (zerop k) i (c+ i (c* j l)))));FIXME
	   (armi-loop (s &optional (j 0) (k 0)) 
		      (declare (rnkind k));FIXME
		      (if s (armi-loop (cdr s) (cpt (car s) j k (array-dimension array k)) (1+ k)) j)))
	  (armi-loop indices)))

;; (defun array-row-major-index (array &rest indices)
;;   (declare (:dynamic-extent indices))
;;   (labels ((cpt (i j k)	(check-type i seqind) (if (zerop j) i (+ i (the seqind (* j k)))));FIXME
;; 	   (armi-loop (s &optional (j 0) (k 0)) (if s (armi-loop (cdr s) (cpt (car s) j (array-dimension array k)) (1+ k)) j)))
;; 	  (armi-loop indices)))

(defun aref (a &rest q)
  (declare (optimize (safety 1)) (:dynamic-extent q))
  (check-type a array)
  (row-major-aref a (apply 'array-row-major-index a q)))

(defun si::aset (v a &rest q)
  (declare (optimize (safety 1)) (:dynamic-extent q))
  (check-type a array)
  (row-major-aset v a (apply 'array-row-major-index a q)))

(setf (symbol-function 'array-rank) (symbol-function 'c-array-rank)
      (symbol-function 'array-total-size) (symbol-function 'c-array-dim))

(defun array-in-bounds-p (a &rest i &aux (j 0))
  (declare (optimize (safety 1)) (:dynamic-extent i))
  (check-type a array)
  (and (not (member-if-not (lambda (x) (< -1 x (array-dimension a (prog1 j (incf j))))) i))
       (= j (c-array-rank a))))

;; (defun array-in-bounds-p (a &rest i)
;;   (declare (optimize (safety 1)) (:dynamic-extent i))
;;   (check-type a array)
;;   (let ((r (array-rank a)))
;;     (labels ((aibp-loop (i &optional (j 0))
;; 			(cond ((>= j r))
;; 			      ((not i) (error "bad indices"))
;; 			      ((< -1 (car i) (array-dimension a j)) (aibp-loop (cdr i) (1+ j))))))
;; 	    (aibp-loop i))))

(defun array-dimension (x i)
  (declare (optimize (safety 2)))
  (check-type x array)
  (check-type i rnkind)
  (let ((r (c-array-rank x)));FIXME
    (assert (< i r) (i) 'type-error :datum i :expected-type `(integer 0 (,r)))
    (if (= 1 r) (c-array-dim x) (array-dims x i))));(the seqind (*fixnum (c-array-dims x) i nil nil))

;; (defun array-dimension (x i)
;;   (declare (optimize (safety 2)))
;;   (check-type x array)
;;   (check-type i rnkind)
;;   (let ((r (c-array-rank x)));FIXME
;;     (let ((*dim* r)(i i))(check-type i (satisfies array-dimension-index-less-than-rank)))
;;     (if (= 1 r) (c-array-dim x) (the seqind (*fixnum (c-array-dims x) i nil nil)))))

;; (defun array-dimension (x i)
;;   (declare (optimize (safety 1)))
;;   (check-type x array)
;;   (check-type i rnkind)
;;   (let ((r (array-rank x)))
;;     (if (= 1 r) (c-array-dim x) (the seqind (*fixnum (c-array-dims x) i nil nil)))))
					;FIXME

(defun array-dimensions (x &aux (j 0))
  (declare (optimize (safety 1)))
  (check-type x array)
  (mapl (lambda (y) (setf (car y) (array-dimension x (prog1 j (incf j))))) (make-list (c-array-rank x))));FIXME c-array-rank propagator


(defun array-has-fill-pointer-p (x)
  (declare (optimize (safety 1)))
  (check-type x array)
  (= (c-array-hasfillp x) 1))

;; (defun upgraded-array-element-type (type &optional environment)
;;   (declare (ignore environment) (optimize (safety 1)))
;;   (cond ((not type))
;; 	((eq type '*) '*)
;; 	((car (member type +array-types+)))
;; 	((car (member type +array-types+ :test 'subtypep1)))
;; 	((subtypep1 type 'float) 'long-float)
;; 	(t)))

(defun fill-pointer (x)
  (declare (optimize (safety 2)))
  (check-type x fpvec)
  (c-vector-fillp x))
;  (fill-pointer-internal x)

(defun make-array (dimensions
		   &key (element-type t)
		   initial-element
		   (initial-contents nil icsp)
		   adjustable fill-pointer
		   displaced-to (displaced-index-offset 0)
		   static
		   &aux
		   (dimensions (if (and (listp dimensions) (not (cdr dimensions))) (car dimensions) dimensions))
		   (element-type (upgraded-array-element-type element-type)))
  (declare (optimize (safety 1)))
  (check-type fill-pointer (or boolean integer))
  (check-type displaced-to (or null array))
  (check-type displaced-index-offset integer)
  (etypecase 
      dimensions
    (list
     (assert (not fill-pointer))
     (dolist (d dimensions) (check-type d integer))
     (let ((x (make-array1 element-type static initial-element displaced-to displaced-index-offset dimensions adjustable)))
       (when (unless (member 0 dimensions) icsp)
	 (let ((i -1))
	   (labels ((set (d c) (cond (d (assert (eql (car d) (length c))) (map nil (lambda (z) (set (cdr d) z)) c))
				     ((row-major-aset c x (incf i))))))
	     (set dimensions initial-contents))))
       x))
    (integer
     (let ((x (make-vector element-type dimensions adjustable (when fill-pointer dimensions)
			   displaced-to displaced-index-offset static initial-element)))
       (when icsp (replace x initial-contents))
       (when (and fill-pointer (not (eq t fill-pointer))) (setf (fill-pointer x) fill-pointer))
       x))))

(defun vector (&rest objects)
  (declare (:dynamic-extent objects))
  (make-array (length objects) :element-type t :initial-contents objects))

(defun bit (bit-array &rest indices)
  (declare (:dynamic-extent indices))
  (apply 'aref bit-array indices))


(defun sbit (bit-array &rest indices)
  (declare (:dynamic-extent indices))
  (apply 'aref bit-array indices))


(defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-and bit-array1 bit-array2 result-bit-array))


(defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-ior bit-array1 bit-array2 result-bit-array))


(defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-xor bit-array1 bit-array2 result-bit-array))


(defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-eqv bit-array1 bit-array2 result-bit-array))

    
(defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-nand bit-array1 bit-array2 result-bit-array))

    
(defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-nor bit-array1 bit-array2 result-bit-array))

    
(defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-andc1 bit-array1 bit-array2 result-bit-array))

    
(defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-andc2 bit-array1 bit-array2 result-bit-array))

    
(defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-orc1 bit-array1 bit-array2 result-bit-array))

    
(defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-orc2 bit-array1 bit-array2 result-bit-array))

    
(defun bit-not (bit-array &optional result-bit-array)
  (bit-array-op boole-c1 bit-array bit-array result-bit-array))


(defun vector-push (new-element vector)
  (declare (optimize (safety 2)))
  (check-type vector fpvec)
  (let ((fp (fill-pointer vector)))
    (cond ((< fp (array-dimension vector 0))
           (si:aset new-element vector fp)
           (si:fill-pointer-set vector (1+ fp))
	   fp))))


(defun vector-push-extend (new-element vector &optional extension)
  (declare (optimize (safety 2)))
  (check-type vector fpvec)
  (let ((fp (fill-pointer vector))
	(dim (array-dimension vector 0)))
    (unless (< fp dim)
      (adjust-array vector (the seqind (+ dim (or extension (max 5 dim))))
		    :element-type (array-element-type vector)
		    :fill-pointer fp))
    (aset new-element vector fp)
    (setf (fill-pointer vector) (1+ fp))
    fp))


(defun vector-pop (vector)
  (declare (optimize (safety 2)))
  (check-type vector fpvec)
  (let ((fp (fill-pointer vector)))
    (when (= fp 0)
          (error "The fill pointer of the vector ~S zero." vector))
    (fill-pointer-set vector (1- fp))
    (aref vector (1- fp))))


(defun adjust-array (array new-dimensions
                     &rest r
		     &key element-type
			  initial-element
			  (initial-contents nil initial-contents-supplied-p)
			  (fill-pointer nil fill-pointer-supplied-p)
			  (displaced-to nil)
			  (displaced-index-offset 0)
			  (static nil static-supplied-p))

  (declare (ignore initial-element initial-contents static displaced-index-offset) ;FIXME
	   (:dynamic-extent r)
	   (optimize (safety 2)))

  (check-type array array)
  (check-type new-dimensions (or seqind proper-list))

  (when (and (listp new-dimensions) (not (cdr new-dimensions))) (setq new-dimensions (car new-dimensions)))
  
  (setq element-type (array-element-type array))
  (unless (eq element-type t)
    (setq r (cons element-type r) r (cons :element-type r)))

  (unless static-supplied-p
    (setq r (cons (staticp array) r) r (cons :static r)))

  (cond (fill-pointer-supplied-p
	 (let ((fill-pointer (or fill-pointer (when (array-has-fill-pointer-p array) (fill-pointer array)))))
	   (setf (cadr (member :fill-pointer r)) fill-pointer)))
	((array-has-fill-pointer-p array) (setq r (cons (fill-pointer array) r) r (cons :fill-pointer r))))
      
  (let ((x (apply 'make-array new-dimensions :adjustable t r)))	

    (unless (or displaced-to initial-contents-supplied-p)

      (cond ((or (seqindp new-dimensions)
		 (and (equal (cdr new-dimensions) (cdr (array-dimensions array)))
		      (or (not (eq element-type 'bit))
			  (when new-dimensions (= 0 (mod (the seqind (car (last new-dimensions))) char-length))))))
	     (copy-array-portion array x 0 0 (min (array-total-size x) (array-total-size array))))
	    ((let ((i -1))
	       (labels ((set (dim &optional (cur (make-list (length new-dimensions) :initial-element 0)) (ind cur))
			     (declare (:dynamic-extent cur))
			     (cond (dim (dotimes (i (pop dim)) (setf (car cur) i) (set dim (cdr cur) ind)))
				   ((incf i)
				    (when (apply 'array-in-bounds-p array ind) 
				      (row-major-aset (apply 'aref array ind) x i))))))
		 (set new-dimensions))))))

    (replace-array array x)

    (when (eq fill-pointer t)
      (setq fill-pointer (array-total-size array)))
    (when fill-pointer
      (setf (fill-pointer array) fill-pointer))
   
    array))

#.`(defun array-eltsize-propagator (f x)
     (cond
      ((and (consp x) (eq (car x) 'or)) (reduce 'type-or1 (mapcar (lambda (x) (array-eltsize-propagator f x)) (cdr x)) :initial-value nil))
      ,@(mapcar (lambda (x)
		  `((type>= (load-time-value (cmp-norm-tp '(array ,(pop x)))) x) (object-type ,(cadr x)))) si::*array-type-info*)
      ((type>= (load-time-value (cmp-norm-tp 'array)) x) 
       (load-time-value (cmp-norm-tp ',(cons 'member (lremove-duplicates (mapcar 'caddr *array-type-info*))))))))
(setf (get 'c-array-eltsize 'type-propagator) 'array-eltsize-propagator)
#.`(defun array-elttype-propagator (f x)
     (cond
      ((and (consp x) (eq (car x) 'or)) (reduce 'type-or1 (mapcar (lambda (x) (array-elttype-propagator f x)) (cdr x)) :initial-value nil))
      ,@(mapcar (lambda (x) `((type>= (load-time-value (cmp-norm-tp '(array ,(pop x)))) x) (object-type ,(car x)))) *array-type-info*)))
(setf (get 'c-array-elttype 'type-propagator) 'array-elttype-propagator)

(defun array-rank-propagator (f x)
  (cond
   ((and (consp x) (eq (car x) 'or)) (reduce 'type-or1 (mapcar (lambda (x) (array-rank-propagator f x)) (cdr x)) :initial-value nil))
   ((type>= (load-time-value (cmp-norm-tp 'vector)) x) (object-type 1))
   ((and (consp x) (eq (car x) 'array)) 
    (let ((x (caddr x))) 
      (typecase x
		(rnkind (object-type x))
		(list (object-type (length x)))
		(otherwise (load-time-value (cmp-norm-tp 'rnkind))))))))
(setf (get 'c-array-rank 'type-propagator) 'array-rank-propagator)

(defun svref (x i) 
  (declare (optimize (safety 1)))
  (check-type x (vector t))
  (check-type i seqind)
  (aref x i))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_arraylib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_describe.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;    describe.lsp
;;;;
;;;;                           DESCRIBE and INSPECT


;; (in-package 'lisp)

;; (export '(describe inspect))


(in-package :system)


(defvar *inspect-level* 0)
(defvar *inspect-history* nil)
(defvar *inspect-mode* nil)

(defvar *old-print-level* nil)
(defvar *old-print-length* nil)


(defun inspect-read-line ()
  (do ((char (read-char *query-io*) (read-char *query-io*)))
      ((or (char= char #\Newline) (char= char #\Return)))))

(defun read-inspect-command (label object allow-recursive)
  (unless *inspect-mode*
    (inspect-indent-1)
    (if allow-recursive
        (progn (princ label) (inspect-object object))
        (format t label object))
    (return-from read-inspect-command nil))
  (loop
    (inspect-indent-1)
    (if allow-recursive
        (progn (princ label)
               (inspect-indent)
               (prin1 object))
        (format t label object))
    (write-char #\Space)
    (force-output)
    (case (do ((char (read-char *query-io*) (read-char *query-io*)))
              ((and (char/= char #\Space) (char/= #\Tab)) char))
      ((#\Newline #\Return)
       (when allow-recursive (inspect-object object))
       (return nil))
      ((#\n #\N)
       (inspect-read-line)
       (when allow-recursive (inspect-object object))
       (return nil))
      ((#\s #\S) (inspect-read-line) (return nil))
      ((#\p #\P)
       (inspect-read-line)
       (let ((*print-pretty* t) (*print-level* nil) (*print-length* nil))
            (prin1 object)
            (terpri)))
      ((#\a #\A) (inspect-read-line) (throw 'abort-inspect nil))
      ((#\u #\U)
       (return (values t (prog1
                          (eval (read-preserving-whitespace *query-io*))
                          (inspect-read-line)))))
      ((#\e #\E)
       (dolist (x (multiple-value-list
                   (multiple-value-prog1
                    (eval (read-preserving-whitespace *query-io*))
                    (inspect-read-line))))
               (write x
                      :level *old-print-level*
                      :length *old-print-length*)
               (terpri)))       
      ((#\q #\Q) (inspect-read-line) (throw 'quit-inspect nil))
      (t (inspect-read-line)
         (terpri)
         (format t
                 "Inspect commands:~%~
		n (or N or Newline):	inspects the field (recursively).~%~
		s (or S):		skips the field.~%~
		p (or P):		pretty-prints the field.~%~
		a (or A):		aborts the inspection ~
					of the rest of the fields.~%~
		u (or U) form:		updates the field ~
					with the value of the form.~%~
		e (or E) form:		evaluates and prints the form.~%~
		q (or Q):		quits the inspection.~%~
		?:			prints this.~%~%")))))

(defmacro inspect-recursively (label object &optional place)
  (if place
      `(multiple-value-bind (update-flag new-value)
            (read-inspect-command ,label ,object t)
         (when update-flag (setf ,place new-value)))
      `(when (read-inspect-command ,label ,object t)
             (princ "Not updated.")
             (terpri))))

(defmacro inspect-print (label object &optional place)
  (if place
      `(multiple-value-bind (update-flag new-value)
           (read-inspect-command ,label ,object nil)
         (when update-flag (setf ,place new-value)))
      `(when (read-inspect-command ,label ,object nil)
             (princ "Not updated.")
             (terpri))))
          
(defun inspect-indent ()
  (fresh-line)
  (format t "~V@T"
          (* 4 (if (< *inspect-level* 8) *inspect-level* 8))))

(defun inspect-indent-1 ()
  (fresh-line)
  (format t "~V@T"
          (- (* 4 (if (< *inspect-level* 8) *inspect-level* 8)) 3)))


(defun inspect-symbol (symbol)
  (let ((p (symbol-package symbol)))
    (cond ((null p)
           (format t "~:@(~S~) - uninterned symbol" symbol))
          ((eq p (find-package "KEYWORD"))
           (format t "~:@(~S~) - keyword" symbol))
          (t
           (format t "~:@(~S~) - ~:[internal~;external~] symbol in ~A package"
                   symbol
                   (multiple-value-bind (b f)
                                        (find-symbol (symbol-name symbol) p)
                     (declare (ignore b))
                     (eq f :external))
                   (package-name p)))))

  (when (boundp symbol)
        (if *inspect-mode*
            (inspect-recursively "value:"
                                 (symbol-value symbol)
                                 (symbol-value symbol))
            (inspect-print "value:~%   ~S"
                           (symbol-value symbol)
                           (symbol-value symbol))))

  (do ((pl (symbol-plist symbol) (cddr pl)))
      ((endp pl))
    (unless (and (symbolp (car pl))
                 (or (eq (symbol-package (car pl)) (find-package 'system))
                     (eq (symbol-package (car pl)) (find-package 'compiler))))
      (if *inspect-mode*
          (inspect-recursively (format nil "property ~S:" (car pl))
                               (cadr pl)
                               (get symbol (car pl)))
          (inspect-print (format nil "property ~:@(~S~):~%   ~~S" (car pl))
                         (cadr pl)
                         (get symbol (car pl))))))
  
  (when (print-doc symbol t)
        (format t "~&-----------------------------------------------------------------------------~%"))
  )

(defun inspect-package (package)
  (format t "~S - package" package)
  (when (package-nicknames package)
        (inspect-print "nicknames:  ~S" (package-nicknames package)))
  (when (package-use-list package)
        (inspect-print "use list:  ~S" (package-use-list package)))
  (when  (package-used-by-list package)
         (inspect-print "used-by list:  ~S" (package-used-by-list package)))
  (when (package-shadowing-symbols package)
        (inspect-print "shadowing symbols:  ~S"
                       (package-shadowing-symbols package))))

(defun inspect-character (character)
  (format t
          (cond ((standard-char-p character) "~S - standard character")
                ((characterp character) "~S - character")
                (t "~S - character"))
          character)
  (inspect-print "code:  #x~X" (char-code character))
  (inspect-print "bits:  ~D" (char-bits character))
  (inspect-print "font:  ~D" (char-font character)))

(defun inspect-number (number)
  (case (type-of number)
    (fixnum (format t "~S - fixnum" number))
    (bignum (format t "~S - bignum" number))
    (ratio
     (format t "~S - ratio" number)
     (inspect-recursively "numerator:" (numerator number))
     (inspect-recursively "denominator:" (denominator number)))
    (complex
     (format t "~S - complex" number)
     (inspect-recursively "real part:" (realpart number))
     (inspect-recursively "imaginary part:" (imagpart number)))
    ((short-float single-float)
     (format t "~S - short-float" number)
     (multiple-value-bind (signif expon sign)
          (integer-decode-float number)
       (declare (ignore sign))
       (inspect-print "exponent:  ~D" expon)
       (inspect-print "mantissa:  ~D" signif)))
    ((long-float double-float)
     (format t "~S - long-float" number)
     (multiple-value-bind (signif expon sign)
          (integer-decode-float number)
       (declare (ignore sign))
       (inspect-print "exponent:  ~D" expon)
       (inspect-print "mantissa:  ~D" signif)))))

(defun inspect-cons (cons)
  (format t
          (case (car cons)
            ((lambda lambda-block lambda-closure lambda-block-closure)
             "~S - function")
            (quote "~S - constant")
            (t "~S - cons"))
          cons)
  (when *inspect-mode*
        (do ((i 0 (1+ i))
             (l cons (cdr l)))
            ((atom l)
             (inspect-recursively (format nil "nthcdr ~D:" i)
                                  l (cdr (nthcdr (1- i) cons))))
          (inspect-recursively (format nil "nth ~D:" i)
                               (car l) (nth i cons)))))

(defun inspect-string (string)
  (format t "~S - string";(if (simple-string-p string) "~S - simple string" "~S - string")
          string)
  (inspect-print  "dimension:  ~D"(array-dimension string 0))
  (when (array-has-fill-pointer-p string)
        (inspect-print "fill pointer:  ~D"
                       (fill-pointer string)
                       (fill-pointer string)))
  (when *inspect-mode*
        (dotimes (i (array-dimension string 0))
                 (inspect-recursively (format nil "aref ~D:" i)
                                      (char string i)
                                      (char string i)))))

(defun inspect-vector (vector)
  (format t "~S - vector";(if (simple-vector-p vector) "~S - simple vector" "~S - vector")
          vector)
  (inspect-print  "dimension:  ~D" (array-dimension vector 0))
  (when (array-has-fill-pointer-p vector)
        (inspect-print "fill pointer:  ~D"
                       (fill-pointer vector)
                       (fill-pointer vector)))
  (when *inspect-mode*
        (dotimes (i (array-dimension vector 0))
                 (inspect-recursively (format nil "aref ~D:" i)
                                      (aref vector i)
                                      (aref vector i)))))

(defun inspect-array (array)
  (format t (if (adjustable-array-p array)
                "~S - adjustable aray"
                "~S - array")
          array)
  (inspect-print "rank:  ~D" (array-rank array))
  (inspect-print "dimensions:  ~D" (array-dimensions array))
  (inspect-print "total size:  ~D" (array-total-size array)))

(defun inspect-structure (x &aux name)
  (format t "Structure of type ~a ~%Byte:[Slot Type]Slot Name   :Slot Value"
	  (setq name (type-of x)))
  (let* ((sd (get name 'si::s-data))
	 (spos (s-data-slot-position sd)))
    (dolist (v (s-data-slot-descriptions sd))
	    (format t "~%~4d:~@[[~s] ~]~20a:~s"   
		    (aref spos (nth 4 v))
		    (let ((type (nth 2 v)))
		      (if (eq t type) nil type))
		    (car v)
		    (structure-ref1 x (nth 4 v))))))
    
  
(defun inspect-object (object &aux (*inspect-level* *inspect-level*))
  (inspect-indent)
  (when (and (not *inspect-mode*)
             (or (> *inspect-level* 5)
                 (member object *inspect-history*)))
        (prin1 object)
        (return-from inspect-object))
  (incf *inspect-level*)
  (push object *inspect-history*)
  (catch 'abort-inspect
         (cond ((symbolp object) (inspect-symbol object))
               ((packagep object) (inspect-package object))
               ((characterp object) (inspect-character object))
               ((numberp object) (inspect-number object))
               ((consp object) (inspect-cons object))
               ((stringp object) (inspect-string object))
               ((vectorp object) (inspect-vector object))
               ((arrayp object) (inspect-array object))
	       ((structurep object)(inspect-structure object))
               (t (format t "~S - ~S" object (type-of object))))))


(defun describe (object &optional stream
			&aux (*standard-output* (cond ((eq stream t) *terminal-io*) ((not stream) *standard-output*) (stream)))
			     (*inspect-mode* nil)
                             (*inspect-level* 0)
                             (*inspect-history* nil)
                             (*print-level* nil)
                             (*print-length* nil))
;  "The lisp function DESCRIBE."
  (declare (optimize (safety 2)))
  (terpri)
  (catch 'quit-inspect (inspect-object object))
  (terpri)
  (values))

(defun inspect (object &aux (*inspect-mode* t)
                            (*inspect-level* 0)
                            (*inspect-history* nil)
                            (*old-print-level* *print-level*)
                            (*old-print-length* *print-length*)
                            (*print-level* 3)
                            (*print-length* 3))
;  "The lisp function INSPECT."
  (declare (optimize (safety 2)))
  (read-line)
  (princ "Type ? and a newline for help.")
  (terpri)
  (catch 'quit-inspect (inspect-object object))
  (terpri)
  (values))

(defun print-doc (symbol &optional (called-from-apropos-doc-p nil)
                         &aux (f nil) x)
  (flet ((doc1 (doc ind)
           (setq f t)
           (format t
                   "~&-----------------------------------------------------------------------------~%~53S~24@A~%~A"
                   symbol ind doc))
         (good-package ()
           (if (eq (symbol-package symbol) (find-package "LISP"))
               (find-package "SYSTEM")
               *package*)))

    (cond ((special-operator-p symbol)
           (doc1 (or (documentation symbol 'function) "")
                 (if (macro-function symbol)
                     "[Special form and Macro]"
                     "[Special form]")))
          ((macro-function symbol)
           (doc1 (or (documentation symbol 'function) "") "[Macro]"))
          ((fboundp symbol)
           (doc1
            (or (documentation symbol 'function)
                (if (consp (setq x (function-lambda-expression (symbol-function symbol))))
                    (case (car x)
                          (lambda (format nil "~%Args: ~S" (cadr x)))
                          (lambda-block (format nil "~%Args: ~S" (caddr x)))
                          (lambda-closure
                           (format nil "~%Args: ~S" (car (cddddr x))))
                          (lambda-block-closure
                           (format nil "~%Args: ~S" (cadr (cddddr x))))
                          (t ""))
                    ""))
            "[Function]"))
          ((setq x (documentation symbol 'function))
           (doc1 x "[Macro or Function]")))

    (cond ((constantp symbol)
           (unless (and (eq (symbol-package symbol) (find-package "KEYWORD"))
                        (null (documentation symbol 'variable)))
             (doc1 (or (documentation symbol 'variable) "") "[Constant]")))
          ((si:specialp symbol)
           (doc1 (or (documentation symbol 'variable) "")
                 "[Special variable]"))
          ((or (setq x (documentation symbol 'variable)) (boundp symbol))
           (doc1 (or x "") "[Variable]")))

    (cond ((setq x (documentation symbol 'type))
           (doc1 x "[Type]"))
          ((setq x (get symbol 'deftype-form))
           (let ((*package* (good-package)))
             (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFTYPE." x)
                   "[Type]"))))

    (cond ((setq x (documentation symbol 'structure))
           (doc1 x "[Structure]"))
          ((setq x (get symbol 'defstruct-form))
           (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFSTRUCT." x)
                 "[Structure]")))

    (cond ((setq x (documentation symbol 'setf))
           (doc1 x "[Setf]"))
          ((setq x (get symbol 'setf-update-fn))
           (let ((*package* (good-package)))
             (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFSETF."
                           `(defsetf ,symbol ,(get symbol 'setf-update-fn)))
                   "[Setf]")))
          ((setq x (get symbol 'setf-lambda))
           (let ((*package* (good-package)))
             (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFSETF."
                           `(defsetf ,symbol ,@(get symbol 'setf-lambda)))
                   "[Setf]")))
          ((setq x (get symbol 'setf-method))
           (let ((*package* (good-package)))
             (doc1
              (format nil
                "~@[~%Defined as: ~S~%See the doc of DEFINE-SETF-METHOD.~]"
                (if (consp x)
                    (case (car x)
                          (lambda `(define-setf-method ,@(cdr x)))
                          (lambda-block `(define-setf-method ,@(cddr x)))
                          (lambda-closure `(define-setf-method ,@(cddddr x)))
                          (lambda-block-closure
                           `(define-setf-method ,@(cdr (cddddr x))))
                          (t nil))
                    nil))
            "[Setf]"))))
    )
  (idescribe (symbol-name symbol))
  (if called-from-apropos-doc-p
      f
      (progn (if f
                 (format t "~&-----------------------------------------------------------------------------")
                 (format t "~&No documentation for ~:@(~S~)." symbol))
             (values))))

(defun apropos-doc (string &optional (package 'lisp) &aux f (package (or package (list-all-packages))))
  (setq string (string string))
  (do-symbols (symbol package) ;FIXME?  do-symbols takes package list
	      (when (search string (string symbol))
		(setq f (or (print-doc symbol t) f))))
  (if f
      (format t "~&-----------------------------------------------------------------------------")
      (format t "~&No documentation for ~S in ~:[any~;~A~] package."
              string package
              (and package (package-name (coerce-to-package package)))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_describe.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_evalmacros.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;	evalmacros.lsp


(in-package :si)

(export '(*debug* *compiler-check-args* *safe-compile* *compiler-new-safety*
		  *compiler-push-events* *space* *speed*
 		  *alien-declarations*
 		  lit sgen cmp-inline cmp-notinline cmp-type))

(eval-when (eval compile) (setq si:*inhibit-macro-special* nil))

(defun lit (&rest r)
  (error "lit called with args ~s~%" r))

(defmacro sgen (&optional (pref "G"))
  `(load-time-value (gensym ,pref)))

(defmacro defvar (var &optional (form nil form-sp) doc-string)
  (declare (optimize (safety 2)))
  `(progn (si:*make-special ',var)
	  ,(if doc-string
	       `(si:putprop ',var ,doc-string 'variable-documentation))
	  ,(if form-sp
	       `(or (boundp ',var)
		    (setq ,var ,form)))
	  ',var)
	  )

(defmacro defparameter (var form &optional doc-string)
  (declare (optimize (safety 2)))
  `(progn (si:*make-special ',var)
	  ,@(when doc-string `((si:putprop ',var ,doc-string 'variable-documentation)))
	  (setq ,var ,form)
	  ',var))

(defmacro defconstant (var form &optional doc-string)
  (declare (optimize (safety 2)))
  `(progn (si:*make-constant ',var ,form)
	  ,@(when doc-string `((si:putprop ',var ,doc-string 'variable-documentation)))
	  ',var))


;;; Each of the following macros is also defined as a special form.
;;; Thus their names need not be exported.

(defmacro and (&rest forms)
  (declare (optimize (safety 2)))
  (if (endp forms)
      t
      (let ((x (reverse forms)))
           (do ((forms (cdr x) (cdr forms))
                (form (car x) `(if ,(car forms) ,form)))
               ((endp forms) form)))))

(defmacro or (&rest forms)
  (declare (optimize (safety 2)))
  (unless (endp forms)
    (let ((x (reverse forms)))
      (do ((forms (cdr x) (cdr forms)) (gs (sgen "OR"))
	   (form (car x)
		 (if (or (constantp (car forms)) (symbolp (car forms)))
		     (let ((temp (car forms)))
		       `(if ,temp ,temp ,form))
		   `(let ((,gs ,(car forms)))
		      (declare (ignorable ,gs))
		      (if ,gs ,gs ,form)))))
               ((endp forms) form)))))

(defmacro locally (&rest body)
  (multiple-value-bind
   (doc decls ctps body)
   (parse-body-header body)
   `(let (,@(mapcan (lambda (x &aux (z (pop x))(z (if (eq z 'type) (pop x) z)))
		      (case z
			    ((ftype inline notinline optimize special) nil)
			    (otherwise (mapcar (lambda (x) (list x x)) x))))
		   (apply 'append (mapcar 'cdr decls))))
      ,@(when doc (list doc))
      ,@decls
      ,@ctps
      ,@body)))

(defmacro loop (&rest body &aux (tag (sgen "LOOP")))
  (declare (optimize (safety 2)))
  `(block nil (tagbody ,tag (progn ,@body) (go ,tag))))

(defun import (s &optional (p *package*))
  (import-internal s p)
  t)

(defun delete-package (p)
  (the boolean (values (delete-package-internal p))))

;(import 'while #+ansi-cl 'cl-user #-ansi-cl 'user)
(defmacro while (test &rest forms)
  (declare (optimize (safety 2)))
 `(loop (unless ,test (return)) ,@forms))

(defun setf-sym (funid)
  (values       
   (intern (si::string-concatenate
	    (let ((x (symbol-package funid))) (if x (package-name x) ""))
	    "::"
	    (symbol-name funid))
	   (load-time-value (or (find-package 'setf) (make-package 'setf))))))

(defun funid-sym (funid) funid)
(defun funid-sym-p (funid) t)
(defun funid (funid) funid)
(defun funid-p (funid) t)
(defun funid-to-sym (funid) (funid-sym funid))

(defmacro defmacro (name vl &rest body)
  (declare (optimize (safety 2)))
  `(progn
     (setf (macro-function ',name) ,(defmacro-lambda name vl body))
     ',name))

(defmacro define-symbol-macro (sym exp) 
  (declare (optimize (safety 2)) (ignore sym exp)) nil);FIXME placeholder

(defmacro defun (name lambda-list &rest body)
  (declare (optimize (safety 2)))
  (let* ((doc (parse-body-header body))
	 (rs (funid-sym name))
	 (bn (if (eq rs name) name (cadr name))))
    `(progn ,@(when doc `((setf (get ',rs 'function-documentation) ,doc)))
	    (setf (symbol-function ',rs) ,(block-lambda lambda-list bn body))
	    ',name)))
  
; assignment

(defmacro psetq (&rest args)
  (declare (optimize (safety 2)))
   (do ((l args (cddr l))
        (forms nil)
        (bindings nil))
       ((endp l) (list* 'let* (nreverse bindings) (nreverse (cons nil forms))))
       (let ((sym (gensym)))
            (push (list sym (cadr l)) bindings)
            (push (list 'setq (car l) sym) forms))))

; conditionals

(defmacro cond (&rest clauses &aux (form nil))
  (declare (optimize (safety 2)))
  (let ((x (reverse clauses)))
    (dolist (l x form)
      (cond ((endp (cdr l))
	     (setq form (cond ((eq l (car x)) `(values ,(car l)))
			      (`(or ,(car l) ,form)))))
	    ((let ((c (car l)))
	       (when (constantp c) (cond ((symbolp c) (symbol-value c)) ;fixme constant-eval
					 ((when (consp c) (eq (car c) 'quote)) (cadr c)) (t))))
	     (setq form (if (endp (cddr l)) (cadr l) `(progn ,@(cdr l)))))
	    ((setq form `(if ,(car l) ,(if (endp (cddr l)) (cadr l) `(progn ,@(cdr l))) ,form)))))))

(defmacro when (pred &rest body)
  (declare (optimize (safety 2)))
  `(if ,pred (progn ,@body)))

(defmacro unless (pred &rest body)
  (declare (optimize (safety 2)))
  `(if (not ,pred) (progn ,@body)))

; program feature

(defmacro prog (vl &rest body &aux (decl nil))
  (declare (optimize (safety 2)))
  (do ()
      ((or (endp body)
           (not (consp (car body)))
           (not (eq (caar body) 'declare)))
       `(block nil (let ,vl ,@decl (tagbody ,@body)))
       )
      (push (car body) decl)
      (pop body))
  )

(defmacro prog* (vl &rest body &aux (decl nil))
  (declare (optimize (safety 2)))
  (do ()
      ((or (endp body)
           (not (consp (car body)))
           (not (eq (caar body) 'declare)))
       `(block nil (let* ,vl ,@decl (tagbody ,@body)))
       )
      (push (car body) decl)
      (pop body))
  )

; sequencing

(defmacro prog1 (first &rest body &aux (sym (sgen "PROG1")))
  (declare (optimize (safety 2)))
  `(let ((,sym ,first)) (declare (ignorable ,sym)) ,@body ,sym))

(defmacro prog2 (first second &rest body &aux (sym (sgen "PROG2")))
  (declare (optimize (safety 2)))
  `(progn ,first (let ((,sym ,second)) (declare (ignorable ,sym)) ,@body ,sym)))

; multiple values

(defmacro multiple-value-list (form)
  (declare (optimize (safety 2)))
  `(multiple-value-call 'list ,form))

(defmacro multiple-value-setq (vars form)
  (declare (optimize (safety 2)))
  (let ((syms (mapcar (lambda (x) (declare (ignore x)) (gensym)) (or vars (list nil)))))
    `(multiple-value-bind ,syms ,form (setq ,@(mapcan 'list vars syms)) ,(car syms))))
;;   (do ((vl vars (cdr vl))
;;        (sym (sgen))
;;        (forms nil))
;;       ((endp vl) `(let ((,sym (multiple-value-list ,form))) (prog1 ,@(nreverse forms))))
;;       (push `(setq ,@(when forms `(,sym (cdr ,sym))) ,(car vl) (car ,sym)) forms)))

(defmacro multiple-value-bind (vars form &rest body)
  (declare (optimize (safety 2)))
  (do ((vl vars (cdr vl))
       (sym (sgen "MULTIPLE-VALUE-BIND"))
       (bind nil))
      ((endp vl) `(let* ((,sym (multiple-value-list ,form)) ,@(nreverse bind))
		    (declare (ignorable ,sym))
		    ,@body))
      (push `(,(car vl) (car ,sym)) bind)
      (unless (endp (cdr vl)) (push `(,sym (cdr ,sym)) bind))))

(defmacro do (control (test . result) &rest body
              &aux (decl nil) (label (sgen "DO")) (vl nil) (step nil))
  (declare (optimize (safety 2)))
  (do ()
      ((or (endp body)
           (not (consp (car body)))
           (not (eq (caar body) 'declare))))
      (push (car body) decl)
      (pop body))
  (dolist (c control)
    (declare (object c))
    (if (symbolp  c) (setq c (list c)))
    (push (list (car c) (cadr c)) vl)
    (unless (endp (cddr c))
      (push (car c) step)
      (push (caddr c) step)))
  `(block nil
          (let ,(reverse vl)
	    ,@decl
	    (tagbody
	     ,label (if (not ,test)
			(progn 
			  (tagbody ,@body)
			  (psetq ,@(reverse step))
			  (go ,label))
		      (return (progn ,@result)))))))

(defmacro do* (control (test . result) &rest body
               &aux (decl nil) (label (sgen "DO*")) (vl nil) (step nil))
  (declare (optimize (safety 2)))
  (do ()
      ((or (endp body)
           (not (consp (car body)))
           (not (eq (caar body) 'declare))))
      (push (car body) decl)
      (pop body))
  (dolist (c control)
          (declare (object c))
    (if(symbolp  c) (setq c (list c)))
        (push (list (car c) (cadr c)) vl)
    (unless (endp (cddr c))
            (push (car c) step)
            (push (caddr c) step)))
  `(block nil
          (let* ,(reverse vl)
                ,@decl
                (tagbody
                 ,label (if (not ,test)
			    (progn 
			      (tagbody ,@body)
			      (setq ,@(reverse step))
			      (go ,label))
			  (return (progn ,@result)))))))


(defmacro case (keyform &rest clauses &aux (key (sgen "CASE")) (c (reverse clauses)))
  (declare (optimize (safety 2)))
  (labels ((sw (x) `(eql ,key ',x))(dfp (x) (or (eq x t) (eq x 'otherwise)))
	   (v (x) (if (when (listp x) (not (cdr x))) (car x) x))
	   (m (x c &aux (v (v x))) (if (eq v x) (cons c v) v)))
	  `(let ((,key ,keyform))
	     (declare (ignorable ,key))
	     ,(let ((df (when (dfp (caar c)) (m (cdr (pop c)) 'progn))))
		(lreduce (lambda (y c &aux (a (pop c))(v (v a)))
			  (when (dfp a) (error 'program-error "default case must be last"))
			  `(if ,(if (when (eq a v) (listp v)) (m (mapcar #'sw v) 'or) (sw v)) ,(m c 'progn) ,y))
			c :initial-value df)))))



;; (defmacro case (keyform &rest clauses &aux (key (sgen "CASE")) f)
;;   (declare (optimize (safety 2)))
;;   (labels ((sw (x) `(eql ,key ',x))
;; 	   (df (aa ff) (when (member aa '(t otherwise)) (when ff (error 'program-error "default case must be last")) t)))
;; 	  `(let ((,key ,keyform))
;; 	     (declare (ignorable ,key))
;; 	     ,(reduce (lambda (c y &aux (ff f)) (setq f t)
;; 			(let* ((aa (pop c))
;; 			       (ka (or (atom aa) (cdr aa)))
;; 			       (da (if (and (listp c) (cdr c)) (cons 'progn c) (car c)))
;; 			       (v (if ka aa (car aa))))
;; 			  (if (df aa ff) da
;; 			    `(if ,(if (when ka (listp aa)) `(or ,@(mapcar #'sw v)) (sw v)) ,da ,y))))
;; 		      clauses :initial-value nil :from-end t))))

(defmacro ecase (keyform &rest clauses &aux (key (sgen "ECASE")))
  (declare (optimize (safety 2)))
  `(let ((,key ,keyform))
     (declare (ignorable ,key))
     (case ,key
	   ,@(mapcar (lambda (x) (if (member (car x) '(t otherwise)) (cons (list (car x)) (cdr x)) x)) clauses)
	   (otherwise
	    (error 'type-error :datum ,key
		   :expected-type '(member ,@(apply 'append (mapcar (lambda (x &aux (x (car x))) (if (listp x) x (list x))) clauses))))))))


(defmacro ccase (keyform &rest clauses &aux (key (sgen "CCASE")))
  (declare (optimize (safety 2)))
  `(let ((,key ,keyform))
     (declare (ignorable ,key))
     (loop
      (case ,key
	    ,@(mapcar (lambda (x &aux (k (pop x)))
			`(,(if (member k '(t otherwise)) (list k) k) (return ,(if (cdr x) (cons 'progn x) (car x))))) clauses)
	    (otherwise 
	     (check-type ,key (member ,@(apply 'append (mapcar (lambda (x &aux (x (car x))) (if (listp x) x (list x))) clauses)))))))))

;; (defmacro case (keyform &rest clauses &aux (form nil) (key (sgen "CASE")))
;;   (declare (optimize (safety 2)))
;;   (dolist (clause (reverse clauses) `(let ((,key ,keyform)) (declare (ignorable ,key)) ,form))
;;     (cond ((or (eq (car clause) 't) (eq (car clause) 'otherwise))
;;            (setq form `(progn ,@(cdr clause))))
;;           ((consp (car clause))
;;            (setq form `(if (or ,@(mapcar (lambda (x) `(eql ,key ',x)) (car clause)));(member ,key ',(car clause))
;;                            (progn ,@(cdr clause))
;;                            ,form)))
;;           ((car clause)
;;            (setq form `(if (eql ,key ',(car clause))
;;                            (progn ,@(cdr clause))
;;                            ,form))))))


(defmacro return (&optional (val nil))   (declare (optimize (safety 2))) `(return-from nil ,val))

(defmacro dolist ((var form &optional (val nil)) &rest body
                                                 &aux (temp (sgen "DOLIST")))
  (declare (optimize (safety 2)))
  `(do* ((,temp ,form (cdr ,temp))
	 (,var (car ,temp) (car ,temp)))
	((endp ,temp) ,val)
	(declare (ignorable ,temp))
	,@body))

;; In principle, a more complete job could be done here by trying to
;; capture fixnum type declarations from the surrounding context or
;; environment, or from within the compiler's internal structures at
;; compile time.  See gcl-devel archives for examples.  This
;; implementation relies on the fact that the gcc optimizer will
;; eliminate the bignum branch if the supplied form is a symbol
;; declared to be fixnum, as the comparison of a long integer variable
;; with most-positive-fixnum is then vacuous.  Care must be taken in
;; making comparisons with most-negative-fixnum, as the C environment
;; appears to treat this as positive or negative depending on the sign
;; of the other argument in the comparison, apparently to symmetrize
;; the long integer range.  20040403 CM.
(defmacro dotimes ((var form &optional (val nil)) &rest body)
  (declare (optimize (safety 2)))
  `(block 
    nil
    ,(cond
       ((symbolp form)
	(let ((temp (sgen "DOTIMES")))
	  `(cond ((< ,form 0)
		  (let ((,var 0))
		    (declare (fixnum ,var) (ignorable ,var))
		    ,val))
		 ((<= ,form most-positive-fixnum)
		  (let ((,temp ,form))
		    (declare (ignorable ,temp) (fixnum ,temp))
		    (do* ((,var 0 (1+ ,var))) ((>= ,var ,temp) ,val)
			 (declare (fixnum ,var))
			 ,@body)))
		 ((let ((,temp ,form))
		    (declare (ignorable ,temp))
		    (do* ((,var 0 (1+ ,var))) ((>= ,var ,temp) ,val)
			 ,@body))))))
       ((constantp form)
	(cond ((< form 0)
	       `(let ((,var 0))
		  (declare (fixnum ,var) (ignorable ,var))
		  ,val))
	      ((<= form most-positive-fixnum)
	       `(do* ((,var 0 (1+ ,var))) ((>= ,var ,form) ,val)
		     (declare (fixnum ,var))
		     ,@body))
	      (`(do* ((,var 0 (1+ ,var))) ((>= ,var ,form) ,val)
		     ,@body))))
       ((let ((temp (sgen "DOTIMES")))
	  `(let ((,temp ,form))
	     (declare (ignorable ,temp))
	     (cond ((< ,temp 0)
		    (let ((,var 0))
		      (declare (fixnum ,var) (ignorable ,var))
		      ,val))
		   ((<= ,temp most-positive-fixnum)
		    (let ((,temp ,temp))
		      (declare (ignorable ,temp) (fixnum ,temp))
		      (do* ((,var 0 (1+ ,var))) ((>= ,var ,temp) ,val)
			   (declare (fixnum ,var))
			   ,@body)))
		   ((do* ((,var 0 (1+ ,var))) ((>= ,var ,temp) ,val)
			 ,@body)))))))))


(defmacro declaim (&rest l)
  (declare (optimize (safety 2)))
  `(eval-when (compile eval load)
     ,@(mapcar (lambda (x) `(proclaim ',x)) l)))

(defmacro lambda (&whole l &rest args)
  (declare (optimize (safety 2)) (ignore args))
  `(function ,l))

(defmacro memq (a b) `(member ,a ,b :test 'eq))

(defmacro background (form) 
  (let ((x (sgen "BACKGROUND"))) 
    `(let ((,x (si::fork))) 
       (if (eql 0 (car ,x)) 
	   (progn (si::write-pointer-object ,form ,x)(bye)) 
	 ,x))))

(defmacro with-read-values ((i r b) (forms timeout) &body body)
  (let* ((m (sgen "WITH-READ-VALUES"))
	 (j (sgen "WITH-READ-VALUES"))
	 (k (sgen "WITH-READ-VALUES"))
	 (p (sgen "WITH-READ-VALUES"))
	 (pbl (length forms))
	 (pbm (1- (ash 1 pbl))))
  `(let* ((,m ,pbm)
	  (,b (list ,@(mapcar (lambda (x) `(background ,x)) forms))))
     (declare ((integer 0 ,pbm) ,m))
     (unwind-protect
	 (do nil ((= ,m 0))
	     (let ((,p (si::select-read ,b ,timeout)));;FAILURE code here on 0 return
	       (declare ((integer 0 ,pbm) ,p))
	       (do ((,i 0 (1+ ,i))(,j 1 (ash ,j 1)) (,k ,b (cdr ,k))) 
		   ((= ,i ,pbl) (setq ,m (logandc2 ,m ,p)))
		   (declare ((integer 0 ,pbl) ,i) ((integer 1 ,(1+ pbm)) ,j))
		   (when (/= 0 (logand ,j ,p))
		     (let ((,r (si::read-pointer-object (car ,k))))
		       ,@body)))))
       (dolist (,b ,b (cdr ,b)) (si::kill ,b 0))))))
  
(defmacro p-let (bindings &body body) 
  (let* ((i (sgen "PLET")) (r (sgen "PLET")) (c (sgen "PLET"))
	 (pb (remove-if 'atom bindings)))
  `(let* (,@(mapcar 'car pb) ,@(remove-if 'consp bindings))
     (with-read-values 
      (,i ,r ,c) (,(mapcar 'cadr pb) -1)
      (case ,i
	    ,@(let ((g -1)) 
		(mapcar (lambda (x) `(,(incf g) (setq ,(car x) ,r))) pb))))
     ,@body)))

(defmacro p-and (&rest forms) 
  (let* ((i (sgen "P-AND")) (r (sgen "P-AND")) (c (sgen "P-AND")) (top (sgen "P-AND")))
    `(block ,top
       (with-read-values 
	(,i ,r ,c) (,forms -1)
	(unless ,r
	  (dolist (,c ,c) (si::kill ,c 0))
	  (return-from ,top nil)))
       t)))

(defmacro p-or (&rest forms) 
  (let* ((i (sgen "P-OR")) (r (sgen "P-OR")) (c (sgen "P-OR")) (top (sgen "P-OR")))
    `(block ,top
       (with-read-values 
	(,i ,r ,c) (,forms -1)
	(when ,r
	  (dolist (,c ,c) (si::kill ,c 0))
	  (return-from ,top t)))
       nil)))


(defmacro define-compiler-macro (name vl &rest body)
  (declare (optimize (safety 2)))
  (let ((n (funid-sym name)))
    `(progn (putprop ',n
		     ,(defmacro-lambda (if (eq n name) name (cadr name)) vl body)
		     'compiler-macro-prop)
	    ',name)))

(defun compiler-macro-function (name)
  (let ((name (funid-sym name)))
    (get name 'compiler-macro-prop)))

(defun undef-compiler-macro (name)
  (let ((name (funid-sym name)))
    (remprop name 'compiler-macro-prop)))


(defvar *safe-compile* nil)
(defvar *compiler-check-args* nil)
(defvar *compiler-new-safety* nil)
(defvar *compiler-push-events* nil)
(defvar *speed* 3)
(defvar *space* 0)
(defvar *debug* 0)

(defvar *alien-declarations* nil)

(defvar *uniq-list* (make-hash-table :test 'equal))

(defun uniq-list (list) (or (gethash list *uniq-list*) (setf (gethash list *uniq-list*) list)))

(defun normalize-function-plist (plist)
  (labels ((mn (tp &aux (n (cmp-norm-tp tp))) (if (unless (eq tp n) (eq n '*)) (return-from normalize-function-plist nil) n))
	   (norm-sig (sig) (uniq-list (list (mapcar #'mn (car sig)) (mn (cadr sig))))))
  (setf (car plist) (norm-sig (car plist)))
  (setf (cadr plist ) (mapcar (lambda (x) (uniq-list (cons (car x) (norm-sig (cdr x))))) (cadr plist)))
  plist))

(defvar *function-plists* nil);rely on defvar not resetting to nil on loading this file compiled

(defun make-function-plist (&rest args)
  (cond ((and (fboundp 'cmp-norm-tp) (fboundp 'typep))
	 (mapc 'normalize-function-plist *function-plists*)
	 (unintern '*function-plists*)
	 (defun make-function-plist (&rest args) (normalize-function-plist args))
	 (normalize-function-plist args))
	((car (push args *function-plists*)))))


(defun proclaim (decl &aux (a (car decl))(d (cdr decl)))
 (declare (optimize (safety 1)))
 (check-type decl list)
 (check-type (cdr decl) list)
 (case a
   (special (mapc (lambda (x) (check-type x symbol) (*make-special x)) d))
   (optimize
    (mapc (lambda (y &aux (x (if (symbolp y) (list y 3) y)))
	    (check-type x (cons t (cons (integer 0 3) null)))
	    (let ((a (pop x))(ad (car x)))
	      (ecase a
		(debug (setq *debug* ad))
		(safety (setq *compiler-check-args* (>= ad 1))
			(setq *safe-compile* (>= ad 2))
			(setq *compiler-new-safety* (>= ad 3))
			(setq *compiler-push-events* (>= ad 4)))
		(space (setq *space* ad))
		(speed (setq *speed* ad))
		(compilation-speed (setq *speed* (- 3 ad)))))) d))
   (type  (let ((q (car d))) (check-type q  type-spec) (proclaim d)))
   (ftype (let ((q (car d))) (check-type q ftype-spec) (proclaim d)))
   ((inline notinline)
    (mapc (lambda (x &aux (y (funid-sym x)))
	    (check-type x function-name)
	    (putprop y t (if (eq a 'inline) 'cmp-inline    'cmp-notinline))
	    (remprop y   (if (eq a 'inline) 'cmp-notinline 'cmp-inline))) d))
   ((ignore ignorable) (mapc (lambda (x) (check-type x function-name)) d))
   (declaration (mapc (lambda (x) (check-type x symbol) (pushnew x *alien-declarations*)) d))
   (otherwise
    (cond ((typep a 'type-spec) (proclaim-var a d))
	  ((typep a 'ftype-spec)); (add-function-proclamation (pop d) (cdr a) d))
	  ((unless (member a *alien-declarations*) (warn "The declaration specifier ~s is unknown." a)))
	  ((symbolp a) (let ((y (get a :proclaim))) (when y (mapc (lambda (x) (funcall y x)) d)))))))
 nil)


(defun proclaim-var (tp l &aux (tp (cmp-norm-tp tp)))
  (unless (or (eq tp '*) (eq tp t))
    (mapc (lambda (x)
	    (check-type x symbol)
	    (assert (setq tp (type-and tp (get x 'cmp-type t))))
	    (putprop x tp 'cmp-type)) l)));sch-global, improper-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_evalmacros.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_iolib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;   iolib.lsp
;;;;
;;;;        The IO library.


;; (in-package 'lisp)

;; (export '(with-open-stream with-input-from-string with-output-to-string parse-integer))
;; (export '(read-from-string))
;; (export '(write-to-string prin1-to-string princ-to-string))
;; (export 'file-string-length)
;; (export 'with-open-file)
;; (export '(y-or-n-p yes-or-no-p))
;; (export 'dribble)
;; (export 'with-standard-io-syntax)
;; (export 'logical-pathname-translations)
;; (export 'load-logical-pathname-translations)
;; (export 'formatter)
;; (export 'pprint-dispatch)
;; (export 'set-pprint-dispatch)
;; (export 'copy-pprint-dispatch)
;; (export 'ensure-directories-exist) ; from ECLS
;; (export 'print-unreadable-object) ; from ECLS
;; (export 'with-compilation-unit)
;; (export '(concatenated-stream-streams 
;; 	  broadcast-stream-streams 
;; 	  two-way-stream-input-stream
;; 	  echo-stream-input-stream
;; 	  two-way-stream-output-stream
;; 	  echo-stream-output-stream
;; 	  synonym-stream-symbol
;; 	  read-byte
;; 	  write-byte
;; 	  read-sequence
;; 	  write-sequence
;; 	  open))

(in-package :system)

(defun concatenated-stream-streams (stream)
  (declare (optimize (safety 2)))
  (check-type stream concatenated-stream)
  (c-stream-object0 stream))
(defun broadcast-stream-streams (stream)
  (declare (optimize (safety 2)))
  (check-type stream broadcast-stream)
  (c-stream-object0 stream))
(defun two-way-stream-input-stream (stream)
  (declare (optimize (safety 2)))
  (check-type stream two-way-stream)
  (c-stream-object0 stream))
(defun echo-stream-input-stream (stream)
  (declare (optimize (safety 2)))
  (check-type stream echo-stream)
  (c-stream-object0 stream))
(defun two-way-stream-output-stream (stream)
  (declare (optimize (safety 2)))
  (check-type stream two-way-stream)
  (c-stream-object1 stream))
(defun echo-stream-output-stream (stream)
  (declare (optimize (safety 2)))
  (check-type stream echo-stream)
  (c-stream-object1 stream))
(defun synonym-stream-symbol (stream)
  (declare (optimize (safety 2)))
  (check-type stream synonym-stream)
  (c-stream-object0 stream))

(defun maybe-clear-input (&optional (x *standard-input*))
  (cond ((not (typep x 'stream)) nil)
	((typep x 'synonym-stream) (maybe-clear-input (symbol-value (synonym-stream-symbol x))))
	((typep x 'two-way-stream) (maybe-clear-input (two-way-stream-input-stream x)))
	((terminal-input-stream-p x) (clear-input t))))

(defun decl-vars (decls);FIXME complete and centralize
  (remove-duplicates
   (mapcan (lambda (x) 
	     (when (eq (car x) 'declare)
	       (mapcan (lambda (x) (cond ((member (car x) '(type ftype)) (cddr x))
					 ((member (car x) '(optimize)) nil)
					 ((cdr x)))) (cdr x)))) (copy-tree decls))))


(defmacro with-open-stream ((var stream) . body)
;  (declare (optimize (safety 2)))
  (multiple-value-bind 
   (ds b)
   (find-declarations body)
   `(let* (,@(mapcar (lambda (x) (list x x)) (remove var (decl-vars ds)))
	     (,var ,stream))
      ,@ds
      (unwind-protect
	  (progn ,@b)
	(close ,var)))))



(defmacro with-input-from-string ((var string &key index start end) . body)
;  (declare (optimize (safety 2)))
  (multiple-value-bind 
   (ds b)
   (find-declarations body)
   (let ((r (gensym)))
     `(let* (,@(mapcar (lambda (x) (list x x)) (remove var (decl-vars ds)))
	       (,var (make-string-input-stream ,string ,start ,end)))
	,@ds 
	(let ((,r (multiple-value-list (progn ,@b))))
	  ,@(when index `((setf ,index (si:get-string-input-stream-index ,var))))
	  (values-list ,r))))))


(defmacro with-output-to-string ((var &optional string &key element-type) . body)
;  (declare (optimize (safety 2)))
  (multiple-value-bind 
   (ds b)
   (find-declarations body)
   (let ((e (gensym)))
     `(let* (,@(mapcar (lambda (x) (list x x)) (remove var (decl-vars ds)))
	      (,var ,(if string `(make-string-output-stream-from-string ,string) `(make-string-output-stream))))
	,@ds 
	(let (,@(when element-type `((,e ,element-type))));FIXME
	  ,@b
	  ,@(unless string `((get-output-stream-string ,var))))))))


(defun read-from-string (string
                         &optional (eof-error-p t) eof-value
                         &key (start 0) (end nil end-p) preserve-whitespace)
  (declare (optimize (safety 2)))
  (check-type string string)
  (unless end-p (setq end (length string)))
  (let ((stream (make-string-input-stream string start end)))
    (if preserve-whitespace
        (values (read-preserving-whitespace stream eof-error-p eof-value)
                (si:get-string-input-stream-index stream))
        (values (read stream eof-error-p eof-value)
                (si:get-string-input-stream-index stream)))))


(defun write (x &key stream 
		(array            *print-array*)
		(base             *print-base*)
		(case             *print-case*)
		(circle           *print-circle*)
		(escape           *print-escape*)
		(gensym           *print-gensym*)
		(length           *print-length*)
		(level            *print-level*)
		(lines            *print-lines*)
		(miser-width      *print-miser-width*)
		(pprint-dispatch  *print-pprint-dispatch*)
		(pretty           *print-pretty*)
		(radix            *print-radix*)
		(readably         *print-readably*)
		(right-margin     *print-right-margin*))
  (write-int x stream array base case circle escape gensym
	     length level lines miser-width pprint-dispatch
	     pretty radix readably right-margin))

(defun write-to-string (object &rest rest &key
			    ( escape nil escape-supplied-p )
			    ( radix nil radix-supplied-p )
			    ( base nil base-supplied-p )
			    ( circle nil circle-supplied-p )
			    ( pretty nil pretty-supplied-p )
			    ( level nil level-supplied-p )
			    ( length nil length-supplied-p )
			    ( case nil case-supplied-p )
			    ( gensym nil gensym-supplied-p )
			    ( array nil array-supplied-p )
			    ( lines nil lines-supplied-p )
			    ( miser-width nil miser-width-supplied-p )
			    ( pprint-dispatch nil pprint-dispatch-supplied-p )
			    ( readably nil readably-supplied-p )
			    ( right-margin nil right-margin-supplied-p )
                        &aux (stream (make-string-output-stream)))
  (declare (optimize (safety 2)))
  (let*((*print-array*
	  (if array-supplied-p array *print-array*))
	(*print-base*
	  (if base-supplied-p base *print-base*))
	(*print-case*
	  (if case-supplied-p case *print-case*))
	(*print-circle*
	  (if circle-supplied-p circle *print-circle*))
	(*print-escape*
	  (if escape-supplied-p escape *print-escape*))
	(*print-gensym*
	  (if gensym-supplied-p gensym *print-gensym*))
	(*print-length*
	  (if length-supplied-p length *print-length*))
	(*print-level*
	  (if level-supplied-p level *print-level*))
	(*print-lines*
	  (if lines-supplied-p lines *print-lines*))
	(*print-miser-width*
	  (if miser-width-supplied-p miser-width *print-miser-width*))
	(*print-pretty*
	  (if pretty-supplied-p pretty *print-pretty*))
	(*print-radix*
	  (if radix-supplied-p radix *print-radix*))
	(*print-readably*
	  (if readably-supplied-p readably *print-readably*))
	(*print-right-margin*
	  (if right-margin-supplied-p right-margin *print-right-margin*))
	(*print-pprint-dispatch*
	  (if pprint-dispatch-supplied-p pprint-dispatch *print-pprint-dispatch*)))
      (apply #'write object :stream stream rest)
      (get-output-stream-string stream)))

(defun prin1-to-string (object
                        &aux (stream (make-string-output-stream)))
  (declare (optimize (safety 2)))
  (prin1 object stream)
  (get-output-stream-string stream))


(defun princ-to-string (object
                        &aux (stream (make-string-output-stream)))
  (declare (optimize (safety 2)))
  (princ object stream)
  (get-output-stream-string stream))

(defun file-string-length (ostream object)
  (declare (optimize (safety 2)))
  (let ((ostream (if (typep ostream 'broadcast-stream) 
		     (car (last (broadcast-stream-streams ostream)))
		   ostream)))
    (cond ((not ostream) 1)
	  ((subtypep1 (stream-element-type ostream) 'character)
	   (length (let ((*print-escape* nil)) (write-to-string object)))))))

(defmacro with-temp-file ((s pn) (tmp ext) &rest body) 
  (multiple-value-bind
   (doc decls ctps body)
   (parse-body-header body)
   (declare (ignore doc))
   `(let* ((,s (temp-stream ,tmp ,ext)) 
	   (,pn (stream-object1 ,s))) 
      ,@decls
      ,@ctps
      (unwind-protect (progn ,@body) (progn (close ,s) (delete-file ,s))))))

(defmacro with-open-file ((stream . filespec) . body)
;  (declare (optimize (safety 2)))
  (multiple-value-bind 
   (ds b)
   (find-declarations body)
   `(let* (,@(mapcar (lambda (x) (list x x)) (remove stream (decl-vars ds)))
	     (,stream (open ,@filespec)))
      ,@ds 
      (unwind-protect
	  (progn ,@b)
	(if ,stream (close ,stream))))))

(defun pprint-dispatch (obj &optional (table *print-pprint-dispatch*))
  (declare (optimize (safety 2)))
  (let ((fun (si:get-pprint-dispatch obj table)))
    (if fun (values fun t) (values 'si:default-pprint-object nil))))

(setq *print-pprint-dispatch* '(pprint-dispatch . nil))

(defun set-pprint-dispatch (type-spec function &optional
			    (priority 0)
			    (table *print-pprint-dispatch*))
  (declare (optimize (safety 2)))
  (unless (typep priority 'real)
    (error 'type-error :datum priority :expected-type 'real))
  (let ((a (assoc type-spec (cdr table) :test 'equal)))
    (if a (setf (cdr a) (list function priority))
	(rplacd (last table) `((,type-spec ,function ,priority)))))
  nil)

(defun copy-pprint-dispatch (&optional table)
  (declare (optimize (safety 2)))
  (unless table
    (setq table *print-pprint-dispatch*))
  (unless (and (eq (type-of table) 'cons)
  	(eq (car table) 'pprint-dispatch))
    (error 'type-error :datum table :expected-type 'pprint-dispatch))
  (copy-seq table ))

(defun y-or-n-p (&optional string &rest args)
  (do ((reply))
      (nil)
    (when string (format *query-io* "~&~?  (Y or N) " string args))
    (setq reply (read *query-io*))
    (cond ((string-equal (symbol-name reply) "Y")
           (return-from y-or-n-p t))
          ((string-equal (symbol-name reply) "N")
           (return-from y-or-n-p nil)))))


(defun yes-or-no-p (&optional string &rest args)
  (do ((reply))
      (nil)
    (when string (format *query-io* "~&~?  (Yes or No) " string args))
    (setq reply (read *query-io*))
    (cond ((string-equal (symbol-name reply) "YES")
           (return-from yes-or-no-p t))
          ((string-equal (symbol-name reply) "NO")
           (return-from yes-or-no-p nil)))))

(defun sharp-a-reader (stream subchar arg)
  (declare (ignore subchar) (optimize (safety 2)))
  (let ((initial-contents (read stream nil nil t)))
    (unless *read-suppress*
      (do ((i 0 (1+ i))
	   (d nil (cons (length ic) d))
	   (ic initial-contents (if (zerop (length ic)) ic (elt ic 0))))
	  ((>= i arg)
	   (make-array (nreverse d)
		       :initial-contents initial-contents))))))

(set-dispatch-macro-character #\# #\a 'sharp-a-reader)
(set-dispatch-macro-character #\# #\A 'sharp-a-reader)

;; defined in defstruct.lsp
(set-dispatch-macro-character #\# #\s 'sharp-s-reader)
(set-dispatch-macro-character #\# #\S 'sharp-s-reader)

(defvar *dribble-stream* nil)
(defvar *dribble-io* nil)
(defvar *dribble-namestring* nil)
(defvar *dribble-saved-terminal-io* nil)

(defun dribble (&optional (pathname "DRIBBLE.LOG" psp) (f :supersede))
  (declare (optimize (safety 2)))
  (cond ((not psp)
         (when (null *dribble-stream*) (error "Not in dribble."))
         (if (eq *dribble-io* *terminal-io*)
             (setq *terminal-io* *dribble-saved-terminal-io*)
             (warn "*TERMINAL-IO* was rebound while DRIBBLE is on.~%~
                   You may miss some dribble output."))
         (close *dribble-stream*)
         (setq *dribble-stream* nil)
         (format t "~&Finished dribbling to ~A." *dribble-namestring*))
        (*dribble-stream*
         (error "Already in dribble (to ~A)." *dribble-namestring*))
        (t
         (let* ((namestring (namestring pathname))
                (stream (open pathname :direction :output
                                       :if-exists f
                                       :if-does-not-exist :create)))
           (setq *dribble-namestring* namestring
                 *dribble-stream* stream
                 *dribble-saved-terminal-io* *terminal-io*
                 *dribble-io* (make-two-way-stream
                               (make-echo-stream *terminal-io* stream)
                               (make-broadcast-stream *terminal-io* stream))
                 *terminal-io* *dribble-io*)
           (multiple-value-bind (sec min hour day month year)
               (get-decoded-time)
             (format t "~&Starts dribbling to ~A (~d/~d/~d, ~d:~d:~d)."
                     namestring year month day hour min sec))))))

; simple formatter macro

(defmacro formatter ( control-string )
  (declare (optimize (safety 2)))
  `(progn
     (lambda (*standard-output* &rest arguments)                                
       (let ((*format-unused-args* nil))
	 (apply 'format t ,control-string arguments)
	 *format-unused-args*))))

(defun stream-external-format (s)
  (declare (optimize (safety 1)))
  (check-type s stream)
  :default)

;;; copied from ECL under LGPL by Michael Koehne
;;;    with-standard-io-syntax


(defmacro with-standard-io-syntax (&body body)
  (declare (optimize (safety 2)))
  `(let* ((*package* (find-package :cl-user))
	  (*print-array* t)
	  (*print-base* 10)
	  (*print-case* :upcase)
	  (*print-circle* nil)
	  (*print-escape* t)
	  (*print-gensym* t)
	  (*print-length* nil)
	  (*print-level* nil)
	  (*print-lines* nil)
	  (*print-miser-width* nil)
	  (*print-pprint-dispatch* *print-pprint-dispatch*);FIXME
	  (*print-pretty* nil)
	  (*print-radix* nil)
	  (*print-readably* t)
	  (*print-right-margin* nil)
	  (*read-base* 10)
	  (*read-default-float-format* 'single-float)
	  (*read-eval* t)
	  (*read-suppress* nil)
	  (*readtable* (copy-readtable (si::standard-readtable))));FIXME copy?
     ,@body))

(defmacro print-unreadable-object
	  ((object stream &key type identity) &body body)
  (declare (optimize (safety 2)))
  (let ((q `(princ " " ,stream)))
    `(if *print-readably* 
	 (error 'print-not-readable :object ,object)
       (progn
	 (princ "#<" ,stream)
	 ,@(when type `((prin1 (type-of ,object) ,stream) ,q))
	 ,@body
	 ,@(when identity
	     (let ((z `(princ (address ,object) ,stream)))
	       (if (and (not body) type) (list z) (list q z))))
	 (princ ">" ,stream)
	 nil))))
;     (print-unreadable-object-function ,object ,stream ,type ,identity ,(when body `(lambda nil ,@body)))))

; i know this should be in cmpnew - but its easier here.

(defmacro with-compile-file-syntax (&body body)
  `(let ((*print-radix* nil)
	 (*print-base* 10)
	 (*print-circle* t)
	 (*print-pretty* nil)
	 (*print-level* nil)
	 (*print-length* nil)
	 (*print-case* :downcase)
	 (*print-gensym* t)
	 (*print-array* t)
	 (*print-package* t)
	 (*print-structure* t))
     ,@body))

(defmacro with-compilation-unit (opt &rest body)   
  (declare (optimize (safety 2)))
  (declare (ignore opt)) 
  `(let ((res (multiple-value-list (let ((*disable-recompile* t)) ,@body))))
     (do-recompile nil)
     (values-list res)))

(defun get-byte-stream-nchars (s)
  (check-type s stream)
  (let* ((tp (stream-element-type s))
	 (tp (if (consp tp) (cadr tp) char-length))
	 (nc (ceiling tp char-length)))
    nc))

(defun parse-integer (s &key start end (radix 10) junk-allowed)
  (declare (optimize (safety 1)))
  (parse-integer-int s start end radix junk-allowed))


(defun write-byte (j s)
  (declare (optimize (safety 2)))
  (let ((nc (get-byte-stream-nchars s))
	(ff (1- (expt 2 char-length))))
    (do ((k 0 (1+ k))(i j (ash i (- char-length)))) ((= k nc) j)
	(write-char (code-char (logand i ff)) s))))

(defun read-byte (s &optional (eof-error-p t) eof-value)
  (declare (optimize (safety 2)))
  (let ((nc (get-byte-stream-nchars s)))
    (do ((j 0 (1+ j)) 
	 (i 0 (logior i
	       (ash (char-code (let ((ch (read-char s eof-error-p eof-value)))
				 (if (and (not eof-error-p) (eq ch eof-value))
				     (return-from read-byte ch)
				   ch))) (* j char-length)))))
	((= j nc) i))))


(defun read-sequence (seq strm &key (start 0) (end nil))
  (declare (optimize (safety 2)))
  (check-type seq sequence)
  (check-type start (integer 0))
  (when end (check-type end (integer 0)))
  (let* ((end (or end (length seq)))
	 (seq (if (and (consp seq) (> start 0)) (nthcdr start seq) seq))
	 (tp (stream-element-type strm)))
    (if (eq tp 'character)
	(if (consp seq)
	    (do ((i start (1+ i))(seq seq (cdr seq))) ((= i end) i) 
		(declare (seqind i))
		(setf (car seq) (let ((el (read-char strm nil 'eof)))
				  (if (eq el 'eof) (return i) el))))
	  (do ((i start (1+ i))) ((= i end) i) 
	      (declare (seqind i))
	      (setf (aref seq i) (let ((el (read-char strm nil 'eof)))
				   (if (eq el 'eof) (return i) el)))))
      (if (consp seq)
	  (do ((i start (1+ i))(seq seq (cdr seq))) ((= i end) i) 
	      (declare (seqind i))
	      (setf (car seq) (let ((el (read-byte strm nil 'eof)))
				  (if (eq el 'eof) (return i) el))))
	(do ((i start (1+ i))) ((= i end) i) 
	    (declare (seqind i))
	    (setf (aref seq i) (let ((el (read-byte strm nil 'eof)))
				 (if (eq el 'eof) (return i) el))))))))


(defun write-sequence (seq strm &key (start 0) (end nil))
  (declare (optimize (safety 2)))
  (check-type seq sequence)
  (check-type start (integer 0))
  (when end (check-type end (integer 0)))
  (let* ((end (or end (length seq)))
	 (seq (if (and (consp seq) (> start 0)) (nthcdr start seq) seq))
	 (tp (stream-element-type strm)))
    (if (eq tp 'character)
	(if (consp seq)
	    (do ((i start (1+ i))(seq seq (cdr seq))) ((= i end) i) 
		(declare (seqind i))
		(write-char (car seq) strm))
	  (do ((i start (1+ i))) ((= i end) i) 
	      (declare (seqind i))
	      (write-char (aref seq i) strm)))
      (if (consp seq)
	  (do ((i start (1+ i))(seq seq (cdr seq))) ((= i end) i) 
	      (declare (seqind i))
	      (write-byte (car seq) strm))
	(do ((i start (1+ i))) ((= i end) i) 
	    (declare (seqind i))
	    (write-byte (aref seq i) strm)))))
  seq)

(defun restrict-stream-element-type (tp)
  (cond ((member tp '(unsigned-byte signed-byte)) tp)
	((or (member tp '(character :default)) (si::subtypep1 tp 'character)) 'character)
	((si::subtypep1 tp 'integer) 
	 (let* ((ntp (car (expand-ranges (cmp-norm-tp tp))))
		(min (cadr ntp))(max (caddr ntp))
		(s (if (or (eq min '*) (< min 0)) 'signed-byte 'unsigned-byte))
		(lim (unless (or (eq min '*) (eq max '*)) (max (integer-length min) (integer-length max))))
		(lim (if (and lim (eq s 'signed-byte)) (1+ lim) lim)))
	   (if lim `(,s ,lim) s)))
	((check-type tp (member character integer)))))

(defun open (f &key (direction :input)
	       (element-type 'character)
	       (if-exists nil iesp)
	       (if-does-not-exist nil idnesp)
	       (external-format :default))
  (let* ((f (pathname f))
	 (pf (translate-logical-pathname f)))
    (when (wild-pathname-p pf)
      (error 'file-error :pathname pf :format-control "Pathname is wild."))
    (let ((s (open-int pf direction (restrict-stream-element-type element-type)
		       if-exists iesp if-does-not-exist idnesp external-format)))
      (when (typep s 'stream) (c-set-stream-object1 s f) s))))

(defun load-pathname (p print if-does-not-exist
			&aux (pp (merge-pathnames p))
			(epp (reduce (lambda (y x) (or y (probe-file (translate-pathname x "" p))))
				     '(#P".o" #P".lsp" #P".lisp" #P"") :initial-value nil)));FIXME newest?
  (if epp
      (let* ((*load-pathname* pp)(*load-truename* epp))
	(if (eql -1 (string-match #v"(^o|\\.o)$" (pathname-type epp)))
	    (let ((s (open epp)))
	      (unwind-protect (load-stream s print)
		(close s)))
	  (load-fasl epp print)))
    (when if-does-not-exist
      (error 'file-error :pathname pp :format-control "File does not exist."))))

(defun load (p &key (verbose *load-verbose*) (print *load-print*) (if-does-not-exist :error)
	       (external-format :default) &aux (*readtable* *readtable*)(*package* *package*))
  (declare (optimize (safety 1)))
  (check-type p (or stream pathname-designator))
  (when verbose (format t ";; Loading ~s~%" p))
  (prog1
      (typecase p
	(pathname-designator (load-pathname (pathname p) print if-does-not-exist))
	(stream (load-stream p print)))
    (when verbose (format t ";; Finished loading ~s~%" p))))

(defun ensure-directories-exist (ps &key verbose &aux created)
  (declare (optimize (safety 1)))
  (check-type ps pathname-designator)
  (when (wild-pathname-p ps)
    (error 'file-error :pathname ps :format-control "Pathname is wild"))
  (labels ((d (x y &aux (z (ldiff x y)) (n (namestring (make-pathname :directory z))))
	      (when (when z (stringp (car (last z))))
		(unless (eq :directory (stat n))
		  (mkdir n)
		  (setq created t)
		  (when verbose (format *standard-output* "Creating directory ~s~%" n))))
	      (when y (d x (cdr y)))))
    (let ((pd (pathname-directory ps)))
      (d pd (cdr pd)))
    (values ps created)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_iolib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_listlib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;    listlib.lsp
;;;;
;;;;                        list manipulating routines

; Rewritten 11 Feb 1993 by William Schelter and Gordon Novak to use iteration
; rather than recursion, as needed for large data sets.


;; (in-package 'lisp)
;; (export '(endp nthcdr last butlast nbutlast ldiff tailp list-length make-list 
;; 	       rest acons pairlis copy-list copy-alist nconc nreconc nth first
;; 	       second third fourth fifth sixth seventh eighth ninth tenth copy-tree
;; 	       tree-equal mapl mapcar maplist mapc mapcan mapcon append revappend 
;; 	       member member-if member-if-not adjoin adjoin-if adjoin-if-not assoc
;; 	       assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not subst subst-if
;; 	       subst-if-not nsubst nsubst-if nsubst-if-not sublis nsublis intersection
;; 	       nintersection union nunion set-difference nset-difference set-exclusive-or
;; 	       nset-exclusive-or subsetp nth-value))

(in-package :system)

(eval-when
 (compile eval)
 
 (defmacro defktn (fn ll &rest args &aux (a (member '&aux ll))(ll (ldiff ll a)))
   `(progn
      (defun ,fn (,@ll &key test test-not key &aux ,@(cdr a)
		       (kf (when key (coerce key 'function)))
		       (tf (when test (coerce test 'function)))
		       (tnf (when test-not (coerce test-not 'function))))
	(declare (optimize (safety 1)))
	(check-type ,(cadr ll) proper-list)
	(check-type key (or null function-designator))
	(check-type test (or null function-designator))
	(check-type test-not (or null function-designator))
	,@(sublis '((key . kf)(test . tf)(test-not . tnf)) args))
      ,@(let* ((s (gensym))(ts (gensym))
	       (x `(defun ,s 
		     (fd list &key key)
		     (declare (optimize (safety 1)))
		     (check-type fd function-designator)
		     (check-type list proper-list)
		     (check-type key (or null function-designator))
		     (,fn (coerce fd 'function) list ,ts 'funcall :key key))))
	  (list (sublis `((,s . ,(intern (string-concatenate (string fn) "-IF")))(,ts . :test)) x)
		(sublis `((,s . ,(intern (string-concatenate (string fn) "-IF-NOT")))(,ts . :test-not)) x)))))
 
 (defmacro defnfn (n ll &rest body &aux (a (member '&aux ll))(ll (ldiff ll a)))
   `(defun ,n ,(append ll `(&key test test-not key &aux ,@(cdr a)
				 (kf (when key (coerce key 'function)))
				 (tf (when test (coerce test 'function)))
				 (tnf (when test-not (coerce test-not 'function)))))  
      (declare (optimize (safety 1)))
      ,@(mapcar (lambda (x) `(check-type ,x proper-list)) ll)
      (check-type key (or null function-designator))
      (check-type test (or null function-designator))
      (check-type test-not (or null function-designator))
      ,@(sublis '((key . kf)(test . tf)(test-not . tnf)) body)))
 
 (defmacro comp-key (key) 
   `(if (eq ,key #'identity) 0 1))
 
 (defmacro do-key (key n x) 
   (let ((xx (sgen)))
     `(let ((,xx ,x)) (case ,n (0 ,xx) (otherwise (funcall ,key ,xx))))))
 
 (defmacro comp-test (test test-not) 
   `(+ (if ,test-not 1 0)
       (let ((,test ,test))
	 (if (eq ,test #'eq) 0
	   (if (eq ,test #'eql) 2
	     (if (eq ,test #'equal) 4
	       (if (eq ,test #'equalp) 6 
		 (if (eq ,test #'funcall) 8 10))))))))
 
 (defmacro do-test (test nn x y) 
   (let ((n (sgen))(nx (sgen))(ny (sgen)))
     `(let* ((,n ,nn)(,nx ,x)(,ny ,y))
	(case ,n 
	      (0 (eq ,nx ,ny))
	      (1 (not (eq ,nx ,ny)))
	      (2 (eql ,nx ,ny))
	      (3 (not (eql ,nx ,ny)))
	      (4 (equal ,nx ,ny))
	      (5 (not (equal ,nx ,ny)))
	      (6 (equalp ,nx ,ny))
	      (7 (not (equalp ,nx ,ny)))
	      (8 (funcall ,nx ,ny))
	      (9 (not (funcall ,nx ,ny)))
	      (10 (funcall ,test ,nx ,ny))
	      (otherwise (not (funcall ,test ,nx ,ny)))))))
 
 
 (defmacro bump-test (nn i) 
   (let ((n (sgen)))
     `(let ((,n ,nn))
	(case ,n 
	      (2 (if (eql-is-eq ,i) 0 ,n))
	      (3 (if (eql-is-eq ,i) 1 ,n))
	      (4 (if (equal-is-eq ,i) 0 ,n))
	      (5 (if (equal-is-eq ,i) 1 ,n))
	      (6 (if (equalp-is-eq ,i) 0 ,n))
	      (7 (if (equalp-is-eq ,i) 1 ,n))
	      (otherwise ,n)))))
 
 (defmacro bump-test-list (n l) 
   `(case ,n 
	  (2 (if (member-if-not 'eql-is-eq ,l) ,n 0))
	  (3 (if (member-if-not 'eql-is-eq ,l) ,n 1))
	  (4 (if (member-if-not 'equal-is-eq ,l) ,n 0))
	  (5 (if (member-if-not 'equal-is-eq ,l) ,n 1))
	  (6 (if (member-if-not 'equalp-is-eq ,l) ,n 0))
	  (7 (if (member-if-not 'equalp-is-eq ,l) ,n 1))
	  (otherwise ,n)))
 
 (defconstant +list-ll+ '(&key key test test-not))
 
 (defmacro deflist (n (il list ifp plp) &body body)
   `(progn
      (defun ,n ,(append `(,@il ,list) +list-ll+)
	(declare (optimize (safety 2)))
	,@(when plp `((check-type ,list proper-list)))
	(and test test-not (error "both test and test not supplied"))
	(let* ((key (or key #'identity))(key (if (functionp key) key (funcallable-symbol-function key)))
	       (key-comp  (comp-key key))
	       (test (or test test-not #'eql))
	       (test (if (functionp test) test (funcallable-symbol-function test)))
	       (test-comp (comp-test test test-not))
	       (test-comp (bump-test test-comp ,(car (last il)))))
	  ,@body))
      ,@(when ifp
	  `((defun ,(intern (string-concatenate (string n) "-IF")) 
	      ,(append `(,@il ,list) +list-ll+)
	      (declare (optimize (safety 2))(ignore test test-not))
	      (,n ,@il ,list :test 'funcall :key key))
	    (defun ,(intern (string-concatenate (string n) "-IF-NOT")) 
	      ,(append `(,@il ,list) +list-ll+)
	      (declare (optimize (safety 2)) (ignore test test-not))
	      (,n ,@il ,list :test-not 'funcall :key key))))))
 
 (defmacro defllist (n (l1 l2 plp) &body body)
   `(progn
      (defun ,n ,(append `(,l1 ,l2) +list-ll+)
	(declare (optimize (safety 2)))
	(check-type ,l1 proper-list)
	,@(when plp `((check-type ,l2 proper-list)))
	(let* ((key (or key #'identity))(key (if (functionp key) key (funcallable-symbol-function key)))
	       (key-comp  (comp-key key)))
	  ,@body))))

 (defmacro collect (r rp form)
   `(let ((tmp ,form))
      (setq ,rp (cond (,rp (rplacd ,rp tmp) tmp) ((setq ,r tmp))))))
 
 (defmacro cons-length (x)
   (declare (optimize (safety 2)))
   `(let ((,x ,x))
      (if (not ,x) 0
	(do ((i 1 (1+ i))(s ,x (cdr s))(f (cdr ,x) (cddr f)))
	    ((>= i array-dimension-limit) (- array-dimension-limit))
	    (cond ((eq s f) (return i))
		  ((endp f) (return (1+ (- (+ i i)))))
		  ((endp (cdr f)) (return (- (+ i i))))))))))



(defun mapl (fd list &rest r &aux (fun (coerce fd 'function)))
  (declare (optimize (safety 1))(:dynamic-extent r)(notinline make-list));FIXME
  (check-type fd function-designator)
  (check-type list proper-list)
  (let ((q (when r (make-list (length r)))))
    (declare (:dynamic-extent q))
    (labels ((a-cons (x) (check-type x list) (or x (return-from mapl list)))
	     (lmap (f x h) (cond (x (funcall f x) (lmap f (cdr x) h)) (h)))
	     (last nil (lmap (lambda (x) (rplaca x (if r (a-cons (pop r)) (a-cons (cdar x))))) q q)))
	    (lmap (lambda (x) (apply fun x (last))) list list))))

;; (defun mapl (fd list &rest r &aux (fun (coerce fd 'function)))
;;   (declare (optimize (safety 1))(:dynamic-extent r)(notinline make-list));FIXME
;;   (check-type fd function-designator)
;;   (check-type list proper-list)
;;   (let ((q (when r (make-list (length r)))))
;;     (declare (:dynamic-extent q))
;;     (labels ((a-cons (x) (check-type x list) (or x (return-from mapl list)))
;; 	     (lmap (f x) (when x (funcall f x) (lmap f (cdr x))))
;; 	     (last nil (lmap (lambda (x) (rplaca x (if r (a-cons (pop r)) (a-cons (cdar x))))) q) q))
;; 	    (lmap (lambda (x) (apply fun x (last))) list) list)))


(defun mapc (fd list &rest r &aux (fun (coerce fd 'function)))
  (declare (optimize (safety 1))(:dynamic-extent r))
  (check-type fd function-designator)
  (check-type list proper-list)
  (let ((q (when r (make-list (length r)))))
    (declare (:dynamic-extent q))
    (apply 'mapl (lambda (x &rest r) (apply fun (car x) (mapl (lambda (x) (setf (car x) (car (pop r)))) q))) list r)))


(defun mapcar (fd list &rest r &aux (fun (coerce fd 'function)) res rp)
  (declare (optimize (safety 1))(:dynamic-extent r))
  (check-type fd function-designator)
  (check-type list proper-list)
  (apply 'mapc (lambda (x &rest z &aux (tem (cons (apply fun x z) nil)))
		 (setq rp (if rp (cdr (rplacd rp tem)) (setq res tem)))) list r)
  res)

(defun mapcan (fd list &rest r &aux (fun (coerce fd 'function)) res rp)
  (declare (optimize (safety 1))(:dynamic-extent r))
  (check-type fd function-designator)
  (check-type list proper-list)
  (apply 'mapc (lambda (x &rest z &aux (tem (apply fun x z)))
		 (if rp (rplacd rp tem) (setq res tem))
		 (when (consp tem) (setq rp (last tem)))) list r)
  res)

(defun maplist (fd list &rest r &aux (fun (coerce fd 'function)) res rp)
  (declare (optimize (safety 1))(:dynamic-extent r))
  (check-type fd function-designator)
  (check-type list proper-list)
  (apply 'mapl (lambda (x &rest z &aux (tem (cons (apply fun x z) nil)))
		 (setq rp (if rp (cdr (rplacd rp tem)) (setq res tem)))) list r)
  res)

(defun mapcon(fd list &rest r &aux (fun (coerce fd 'function)) res rp)
  (declare (optimize (safety 1))(:dynamic-extent r))
  (check-type fd function-designator)
  (check-type list proper-list)
  (apply 'mapl (lambda (x &rest z &aux (tem (apply fun x z)))
		 (if rp (rplacd rp tem) (setq res tem))
		 (when (consp tem) (setq rp (last tem)))) list r)
  res)



(defktn member (item list &aux (tx (tp8 item)))
  (unless (mapl (lambda (x &aux (k (car x))(k (if key (funcall key k) k))) 
		  (when (cond (test (funcall test item k)) (test-not (not (funcall test-not item k))) ((eql-with-tx item k tx)))
		    (return-from member x))) list)))

(defktn adjoin (item list)
  (if (member (if key (funcall key item) item) list :key key :test test :test-not test-not)
      list
    (cons item list)))

(defktn assoc (item list &aux (tx (tp8 item)))
  (unless (mapc (lambda (x) (check-type x list)
		  (when x
		    (let* ((k (car x))(k (if key (funcall key k) k)))
		      (when (cond (test (funcall test item k)) (test-not (not (funcall test-not item k))) ((eql-with-tx item k tx)))
			(return-from assoc x))))) list)))


(defktn rassoc (item list &aux (tx (tp8 item)))
  (unless (mapc (lambda (x) (check-type x list)
		  (when x
		    (let* ((k (cdr x))(k (if key (funcall key k) k)))
		      (when (cond (test (funcall test item k)) (test-not (not (funcall test-not item k))) ((eql-with-tx item k tx)))
			(return-from rassoc x))))) list)))

(defnfn intersection (l1 l2)
  (mapcan (lambda (x) 
	    (when (member (if key (funcall key x) x) l2 :test test :test-not test-not :key key)
	      (cons x nil))) l1))

(defnfn union (l1 l2 &aux rp)
  (prog1 (or (mapcan (lambda (x) 
		       (unless (member (if key (funcall key x) x) l2 :test test :test-not test-not :key key)
			 (setq rp (cons x nil)))) l1) l2)
    (when rp (rplacd rp l2))))


(defnfn set-difference (l1 l2)
  (mapcan (lambda (x) 
	    (unless (member (if key (funcall key x) x) l2 :test test :test-not test-not :key key)
	      (cons x nil))) l1))

(defnfn set-exclusive-or (l1 l2 &aux rp (rr (copy-list l2)))
  (prog1 (or (mapcan (lambda (x &aux (k (if key (funcall key x) x))) 
		       (if (member k l2 :test test :test-not test-not :key key)
			   (unless (setq rr (delete k rr :test test :test-not test-not :key key)))
			 (setq rp (cons x nil)))) l1) rr)
    (when rp (rplacd rp rr))))

     
(defnfn nintersection (l1 l2 &aux r rp)
  (mapl (lambda (x &aux (k (car x))) 
	  (when (member (if key (funcall key k) k) l2 :test test :test-not test-not :key key)
	    (if rp (rplacd rp x) (setq r x))(setq rp x))) l1)
  (when rp (rplacd rp nil))
  r)

(defnfn nunion (l1 l2 &aux r rp)
  (mapl (lambda (x &aux (k (car x))) 
	  (unless (member (if key (funcall key k) k) l2 :test test :test-not test-not :key key)
	    (if rp (rplacd rp x) (setq r x))(setq rp x))) l1)
  (when rp (rplacd rp l2))
  (or r l2))

(defnfn nset-difference (l1 l2 &aux r rp)
  (mapl (lambda (x &aux (k (car x))) 
	  (unless (member (if key (funcall key k) k) l2 :test test :test-not test-not :key key)
	    (if rp (rplacd rp x) (setq r x))(setq rp x))) l1)
  (when rp (rplacd rp nil))
  r)


(defnfn nset-exclusive-or (l1 l2 &aux r rp (rr (copy-list l2)))
  (mapl (lambda (x &aux (k (car x))(k (if key (funcall key k) k))) 
	    (if (member k l2 :test test :test-not test-not :key key)
		(unless (setq rr (delete k rr :test test :test-not test-not :key key)))
	      (progn (if rp (rplacd rp x) (setq r x))(setq rp x)))) l1)
  (when rp (rplacd rp rr))
  (or r rr))
  

(defnfn subsetp (l1 l2)
  (mapc (lambda (x) 
	  (unless (member (if key (funcall key x) x) l2 :test test :test-not test-not :key key)
	    (return-from subsetp nil))) l1) t)


(defun endp (x)
  (declare (optimize (safety 2)))
  (check-type x list)
  (not x))

(defun nthcdr (n x)
  (declare (optimize (safety 2)))
  (check-type n (integer 0))
  (check-type x list)
  (when x
    (let ((n (cond ((<= n array-dimension-limit) n) 
		   ((let ((j (cons-length x))) (when (> j 0) (mod n j))))
		   ((return-from nthcdr nil)))))
      (do ((x x (cdr x))(n n (1- n))) ((or (<= n 0) (endp x)) x)))))

(defun last (x &optional (n 1));FIXME check for circle
  (declare (optimize (safety 2)))
  (check-type x list)
  (check-type n (integer 0))
  (let* ((n (min array-dimension-limit n))
	 (w (cond ((= n 1) (cdr x))
		  ((do ((n n (1- n))(w x (cdr w))) ((<= n 0) w)
		       (unless (consp w) (return-from last x)))))))
    (do ((x x (cdr x)) (w w (cdr w)))
	((atom w) x))))

(defun butlast (x &optional (n 1));FIXME check for circle
  (declare (optimize (safety 2)))
  (check-type x list)
  (check-type n (integer 0))
  (let* ((n (min array-dimension-limit n))
	 (w (cond ((= n 1) (cdr x))
		  ((do ((n n (1- n))(w x (cdr w))) ((<= n 0) w)
		       (unless (consp w) (return-from butlast nil)))))))
    (do (r rp (x x (cdr x)) (w w (cdr w)))
	((atom w) r)
	(let ((tmp (cons (car x) nil))) (collect r rp tmp)))))

(defun nbutlast (x &optional (n 1));FIXME check for circle
  (declare (optimize (safety 2)))
  (check-type x list)
  (check-type n (integer 0))
  (let* ((n (min array-dimension-limit n))
	 (w (cond ((= n 1) (cdr x))
		  ((do ((n n (1- n))(w x (cdr w))) ((<= n 0) w)
		       (unless (consp w) (return-from nbutlast nil)))))))
    (do ((r x) (rp nil x) (x x (cdr x)) (w w (cdr w)))
	((atom w) (when rp (rplacd rp nil) r)))))

(defun ldiff (l tl &aux (test #'eql))
  (declare (optimize (safety 2)) (ignorable test))
  (check-type l list)
  (do (r rp (tc (bump-test (comp-test test nil) tl)) (l l (cdr l))) 
      ((cond ((do-test test tc l tl)) ((atom l) (when rp (rplacd rp l)))) r)
      (let ((tmp (cons (car l) nil))) (collect r rp tmp))))

(defun tailp (tl l &aux (test #'eql))
  (declare (optimize (safety 2)) (ignorable test))
  (check-type l list)
  (do (r (tc (bump-test (comp-test test nil) tl)) (l l (cdr l))) 
      ((cond ((setq r (do-test test tc l tl))) ((atom l))) r)))
	   
(defun list-length (l)
  (declare (optimize (safety 2)))
  (check-type l list)
  (cond ((endp l) 0) 
	((endp (setq l (cdr l))) 1)
	((endp (setq l (cdr l))) 2)
	((endp (setq l (cdr l))) 3)
	((endp (setq l (cdr l))) 4)
	((let ((x (cons-length l)))
	   (when (<= x 0) (+ 4 (- x)))))))

(defun make-list (n &key initial-element)
  (declare (optimize (safety 2)))
  (check-type n seqind)
  (do (r (n n (1- n))) ((<= n 0) r)
      (push initial-element r)))

(defun rest (l)
  (declare (optimize (safety 2)))
  (check-type l list)
  (cdr l))

(defun acons (key datum alist)
  (declare (optimize (safety 2)))
  (cons (cons key datum) alist))

(defun pairlis (k d &optional a)
  (declare (optimize (safety 1)))
  (check-type k proper-list)
  (check-type d proper-list)
  (mapc (lambda (x y) (setq a (acons x y a))) k d)
  a)

(defun copy-list (l)
  (declare (optimize (safety 2)))
  (check-type l list)
  (do (r rp (l l (cdr l))) ((atom l) (when rp (rplacd rp l)) r)
      (let ((tmp (cons (car l) nil))) (collect r rp tmp))))

(defun copy-alist (l)
  (declare (optimize (safety 1)))
  (check-type l proper-list)
  (maplist (lambda (x &aux (e (car x))) (if (consp e) (cons (car e) (cdr e)) e)) l))


;; (defun copy-alist (l)
;;   (declare (optimize (safety 2)))
;;   (check-type l proper-list)
;;   (do (r rp (l l (cdr l))) ((endp l) r)
;;       (let ((tmp (cons (let ((e (car l))) (cond ((consp e) (cons (car e) (cdr e))) (e))) nil))) 
;; 	(collect r rp tmp))))

(defun nconc (&rest l &aux r rp)
  (declare (:dynamic-extent l))
  (mapl (lambda (l &aux (it (car l)))
	  (if rp (rplacd rp it) (setq r it))
	  (when (and (cdr l) (consp it)) (setq rp (last it)))) l)
  r)
	

;; (defun nconc (&rest l)
;;   (declare (optimize (safety 2)) (:dynamic-extent l))
;;   (do (r rp (l l (cdr l)))
;;       ((endp l) r)
;;       (let ((it (car l)))
;; 	(if rp (rplacd rp it) (setq r it))
;; 	(when (and (cdr l) (consp it)) (setq rp (last it))))))

(defun nreconc (list tail &aux r)
  (declare (optimize (safety 1)))
  (check-type list proper-list)
  (mapl (lambda (x) (when r (setq tail (rplacd r tail))) (setq r x)) list)
  (if r (rplacd r tail) tail))

;; (defun nreconc (list tail)
;;   (declare (optimize (safety 2)))
;;   (check-type list proper-list)
;;   (do (cdp (p tail)(pp list)) ((endp pp) p)
;;       (setq cdp (cdr pp) p (rplacd pp p) pp cdp)))

(defun nth (n x)
  (declare (optimize (safety 2)))
  (check-type n (integer 0))
  (check-type x list)
  (car (nthcdr n x)))

(defun first (x)   (declare (optimize (safety 2))) (check-type x list) (car x))
(defun second (x)  (declare (optimize (safety 2))) (check-type x list) (cadr x))
(defun third (x)   (declare (optimize (safety 2))) (check-type x list) (caddr x))
(defun fourth (x)  (declare (optimize (safety 2))) (check-type x list) (cadddr x))
(defun fifth (x)   (declare (optimize (safety 2))) (check-type x list) (car (cddddr x)))
(defun sixth (x)   (declare (optimize (safety 2))) (check-type x list) (cadr (cddddr x)))
(defun seventh (x) (declare (optimize (safety 2))) (check-type x list) (caddr (cddddr x)))
(defun eighth (x)  (declare (optimize (safety 2))) (check-type x list) (cadddr (cddddr x)))
(defun ninth (x)   (declare (optimize (safety 2))) (check-type x list) (car (cddddr (cddddr x))))
(defun tenth (x)   (declare (optimize (safety 2))) (check-type x list) (cadr (cddddr (cddddr x))))

; Courtesy Paul Dietz
(defmacro nth-value (n expr)
  (declare (optimize (safety 2)))
  `(nth ,n (multiple-value-list ,expr)))

(defun copy-tree (tr)
  (declare (optimize (safety 2)))
  (do (st cs a (g (sgen))) (nil)
      (declare (:dynamic-extent st cs))
      (cond ((atom tr)
	     (do nil ((or (not cs) (eq g (car cs))))
		 (setq a (pop cs) st (cdr st) tr (cons a tr)))
	     (unless cs (return tr))
	     (setf (car cs) tr tr (cdar st)))
	    ((setq st (cons tr st) cs (cons g cs) tr (car tr))))))

(defun tree-equal (tr1 tr2 &key test test-not)
  (declare (optimize (safety 2)))
  (and test test-not (error "both test and test not supplied"))
  (let* ((test (or test test-not #'eql))
	 (test (if (functionp test) test (funcallable-symbol-function test)))
	 (test-comp (comp-test test test-not)))
    (do (st1 cs1 st2 (g (sgen))) (nil)
	(declare (:dynamic-extent st1 cs1 st2))
	(cond ((and (atom tr1) (consp tr2)) (return nil))
	      ((and (consp tr1) (atom tr2)) (return nil))
	      ((atom tr1)
	       (unless (do-test test test-comp tr1 tr2) (return nil))
	       (do nil ((or (not cs1) (eq g (car cs1))))
		   (setq cs1 (cdr cs1) tr1 (pop st1) tr2 (pop st2)))
	       (unless cs1 (return t))
	       (setf (car cs1) tr1 tr1 (cdar st1) tr2 (cdar st2)))
	      ((setq st1 (cons tr1 st1) cs1 (cons g cs1) tr1 (car tr1) 
		     st2 (cons tr2 st2) tr2 (car tr2)))))))

(deflist subst ((n o) tr t nil)
  (do (st cs a c rep (g (sgen))) (nil)
      (declare (:dynamic-extent st cs))
      (setq rep (do-test test test-comp o (do-key key key-comp tr)))
      (cond ((or rep (atom tr))
	     (setq tr (if rep n tr))
	     (do nil ((or (not cs) (eq g (car cs))))
		 (setq a (pop cs) c (pop st) tr (if (and (eq a (car c)) (eq tr (cdr c))) c (cons a tr))))
	     (if cs (setf (car cs) tr tr (cdar st)) (return tr)))
	    ((setq st (cons tr st) cs (cons g cs) tr (car tr))))))

(deflist nsubst ((n o) tr t nil)
  (do (st cs rep (g (sgen))) (nil)
      (declare (:dynamic-extent st cs))
      (setq rep (do-test test test-comp o (do-key key key-comp tr)))
      (cond ((or rep (atom tr))
	     (setq tr (if rep n tr))
	     (do nil ((or (not cs) (eq g (car cs))))
		 (setf (caar st) (pop cs) (cdar st) tr tr (pop st)))
	     (if cs (setf (car cs) tr tr (cdar st)) (return tr)))
	    ((setq st (cons tr st) cs (cons g cs) tr (car tr))))))

(defllist sublis (al tr nil)
  (or (unless al tr)
      (do (st cs a c rep (g (sgen))) (nil)
	(declare (:dynamic-extent st cs))
	(setq rep (assoc (do-key key key-comp tr) al :test test :test-not test-not))
	(cond ((or rep (atom tr))
	       (setq tr (if rep (cdr rep) tr))
	       (do nil ((or (not cs) (eq g (car cs))))
		 (setq a (pop cs) c (pop st) tr (if (and (eq a (car c)) (eq tr (cdr c))) c (cons a tr))))
	       (if cs (setf (car cs) tr tr (cdar st)) (return tr)))
	      ((setq st (cons tr st) cs (cons g cs) tr (car tr)))))))
  
(defllist nsublis (al tr nil)
  (or (unless al tr)
      (do (st cs rep (g (sgen))) (nil)
	(declare (:dynamic-extent st cs))
	(setq rep (assoc (do-key key key-comp tr) al :test test :test-not test-not))
	(cond ((or rep (atom tr))
	       (setq tr (if rep (cdr rep) tr))
	       (do nil ((or (not cs) (eq g (car cs))))
		 (setf (caar st) (pop cs) (cdar st) tr tr (pop st)))
	       (if cs (setf (car cs) tr tr (cdar st)) (return tr)))
	      ((setq st (cons tr st) cs (cons g cs) tr (car tr)))))))

(defun append (&rest l &aux r rp)
  (declare (:dynamic-extent l))
  (mapl (lambda (x &aux (y (car x)))
	  (declare (optimize (safety 2)))
	  (if (cdr x)
	      (mapc (lambda (x) (collect r rp (cons x nil))) y)
	    (collect r rp y))) l) r)

(defun revappend (list tail)
  (declare (optimize (safety 1)))
  (check-type list proper-list)
  (mapc (lambda (x) (setq tail (cons x tail))) list)
  tail)

;; (defun revappend (list tail)
;;   (declare (optimize (safety 2)))
;;   (check-type list proper-list)
;;   (do (cdp (p tail)(pp list)) ((endp pp) p)
;;       (setq cdp (cdr pp) p (cons (car pp) p) pp cdp)))

(defun not (x)
  (if x nil t))

(defun null (x)
  (if x nil t))

(defun get-properties (p i &aux s)
  (declare (optimize (safety 1)));FIXME, safety 2 and no check-type loses signature info
  (check-type p proper-list)
  (check-type i proper-list)
  (cond ((endp p) (values nil nil nil))
	((member (setq s (car p)) i :test 'eq) (values s (cadr p) p))
	((endp (setq s (cdr p))) (error "Bad plist"))
	(t (get-properties (cdr s) i))))

(defun rplaca (x y)
  (declare (optimize (safety 1)))
  (check-type x cons)
  (c-set-cons-car x y)
  x)

(defun rplacd (x y)
  (declare (optimize (safety 1)))
  (check-type x cons)
  (c-set-cons-cdr x y)
  x)

;(defun listp (x) (typep x 'list));(typecase x (list t)))
(defun consp (x) (when x (listp x)))
(defun atom (x) (not (consp x)))

(defun getf (l i &optional d &aux s)
  (declare (optimize (safety 1)))
  (check-type l proper-list)
  (cond ((endp l) d) 
	((eq (car l) i) (cadr l)) 
	((endp (setq s (cdr l))) (error "Bad plist"))
	((getf (cdr s) i d))))

(defun identity (x) x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_listlib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_top.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;  top.lsp
;;;;
;;;;  Top-level loop, break loop, and error handlers
;;;;
;;;;  Revised on July 11, by Carl Hoffman.


(in-package :system)

(export '(loc *tmp-dir* *error-p* *debug-print-level* *break-readtable* *break-enable*
	      vs ihs-vs ihs-fun frs-vs frs-bds frs-ihs bds-var bds-val super-go))

;FIXME ?
(eval-when 
 (compile)
 (defvar *command-args* nil))

(defvar +)
(defvar ++)
(defvar +++)
(defvar -)
(defvar *)
(defvar **)
(defvar ***)
(defvar /)
(defvar //)
(defvar ///)


;; setup file search and autoload

(defvar *fixed-load-path* nil)
(defvar *load-path* nil)
(defvar *load-types* '(".o" ".lsp" ".lisp"))

(defvar *lisp-initialized* nil)
(defconstant +top-level-quit-tag+ (cons nil nil))
(defvar *quit-tag* +top-level-quit-tag+)
(defvar *quit-tags* nil)
(defvar *break-level* '())
(defvar *break-env* nil)
(defvar *ihs-base* 1)
(defvar *ihs-top* 1)
(defconstant +top-ihs+ 1)
(defvar *current-ihs* +top-ihs+)
(defvar *frs-base* 0)
(defvar *frs-top* 0)
(defvar *break-enable* t)
(defvar *break-message* "")

(defvar *break-on-warnings* nil)

(defvar *break-readtable* nil)

(defvar *top-level-hook* nil)


(defvar *top-eof* (cons nil nil))
(defvar *no-prompt* nil)

(defun user-package nil
  (find-package (if (member :ansi-cl *features*) "CL-USER" "USER")))

(defun emergency-reset nil
  (let ((x (load-time-value 
	    (mapcar (lambda (x) (cons x (symbol-function x))) 
		     '(format read find-package package-name 
			      reset-stack-limits eq bye eval fresh-line prin1 terpri))))
	(y (load-time-value (copy-readtable nil)))
	(z (load-time-value (user-package))))
    (dolist (x x) 
      (emergency-fset (car x) (cdr x)))
    (setq *readtable* y)
    (setq *package* z)
    (format t "Emergency reset complete~%")))

(defun show-lib-syms nil
  (when (find-package "LIB")
    (do-external-symbols 
     (p "LIB")
     (print (list p (symbol-value p) (find-package p)))
     (do-external-symbols 
      (s p)
      (print (list s (symbol-value s) (when (fboundp s) (symbol-function s))))))))

(defun coerce-to-package (p)
  (cond ((packagep p) p)
	((find-package p))
	(t 
	 (cerror "Input new package" 'package-error
		 :package p 
		 :format-control "~a is not a package"
		 :format-arguments (list p)) 
	 (coerce-to-package (eval (read))))))
;(declaim (inline coerce-to-package))

(defun reset-lib-syms nil
  (when (find-package "LIB")
    (do-external-symbols 
     (p "LIB")
     (setf (symbol-value p) (dlopen (lib-name p)))
     (do-external-symbols 
      (s p)
      (setf (symbol-value s) (dlsym (symbol-value p) s)))))
  (cfdl))

(defun top-level1 ()
  (let ((+ nil) (++ nil) (+++ nil)
        (- nil) 
        (* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil))
    (setq *lisp-initialized* t)
    (catch *quit-tag*
      (progn 
	(cond
	 (*multiply-stacks* (setq *multiply-stacks* nil))
	 ((stat "init.lsp") (load "init.lsp"))))
      (and (functionp *top-level-hook*)(funcall   *top-level-hook*)))

    (when (boundp '*system-banner*)
      (format t *system-banner*)
      (format t "Temporary directory for compiler files set to ~a~%" *tmp-dir*))

    (loop
     (when 
	 (catch +top-abort-tag+
	   (loop
	    (when 
		(catch *quit-tag*
		  (setq +++ ++ ++ + + -)
		  (if *no-prompt* (setq *no-prompt* nil)
		    (format t "~%~a>"
			    (if (eq *package* (user-package)) ""
			      (package-name *package*))))
		  (reset-stack-limits)
		  ;; have to exit and re-enter to multiply stacks
		  (cond (*multiply-stacks* (Return-from top-level1)))
		  (setq - (locally (declare (notinline read))
				   (read *standard-input* nil *top-eof*)))
		  (when (eq - *top-eof*) (bye))
					;              (si::clear-c-stack 4096)
		  (let ((values (multiple-value-list
				 (locally (declare (notinline eval)) (eval -)))))
		    (setq /// // // / / values *** ** ** * * (car /))
		    (fresh-line)
		    (dolist (val /)
		      (locally (declare (notinline prin1)) (prin1 val))
		      (terpri))
		    nil))
	      (setq *evalhook* nil *applyhook* nil)
	      (terpri *error-output*)
	      (break-current)))
	   nil)
       (emergency-reset)))))
  
(defun default-system-banner ()
  (let (gpled-modules)
    (dolist (l '(:unexec :bfd :readline :xgcl))
      (when (member l *features*)
	(push l gpled-modules)))
    (format nil "GCL (GNU Common Lisp)  ~a.~a.~a ~a  ~a  ~a~%~a~%~a ~a~%~a~%~a~%~%~a~%" 
	    *gcl-major-version* *gcl-minor-version* *gcl-extra-version*
	    (if (member :ansi-cl *features*) "ANSI" "CLtL1")
	    (if (member :gprof *features*) "profiling" "")
	    (si::gcl-compile-time)
	    "Source License: LGPL(gcl,gmp,pargcl), GPL(unexec,bfd,xgcl)"
	    "Binary License: "
	    (if gpled-modules (format nil "GPL due to GPL'ed components: ~a" gpled-modules)
	      "LGPL")
	    "Modifications of this banner must retain notice of a compatible license"
	    "Dedicated to the memory of W. Schelter"
	    "Use (help) to get some basic information on how to use GCL.")))

 (defvar *system-banner*)

(defun gcl-top-level nil
  
  (set-up-top-level)
  
  (setq *package* (user-package))
  (setq *ihs-top* (ihs-top))
  (top-level1))

(defun top-level nil (gcl-top-level))

(defun set-dir (sym val)
   (let ((tem (or val (and (boundp sym) (symbol-value sym)))))
      (if tem (set sym (coerce-slash-terminated tem)))))

(defvar *error-p* nil)

(defun process-some-args (args &optional compile &aux *load-verbose*)
  (when args
    (let ((x (pop args)))
      (cond ((equal x "-load") (load (pop args)))
	    ((equal x "-eval") (eval (read-from-string (pop args))))
	    ((equal x "-batch") (setq *top-level-hook* 'bye))
	    ((equal x "-o-file") (unless (read-from-string (car args))
				   (push (cons :o-file nil) compile)
				   (pop args)))
	    ((equal x "-h-file") (push (cons :h-file t) compile))
	    ((equal x "-data-file") (push (cons :data-file t) compile))
	    ((equal x "-c-file") (push (cons :c-file t) compile))
	    ((equal x "-system-p") (push (cons :system-p t) compile))
	    ((equal x "-compile") (push (cons :compile (pop args)) compile))
	    ((equal x "-o") (push (cons :o (pop args)) compile))
	    ((equal x "-libdir") (set-dir '*lib-directory* (pop args)))
	    ((equal x "-dir") (set-dir '*system-directory* (pop args)))
	    ((equal x "-f") (do-f (car (setq *command-args* args))))
	    ((equal x "--") (setq *command-args* args args nil))))
    (process-some-args args compile))

  (when compile
    (let* (*break-enable* 
	   (file (cdr (assoc :compile compile)))
	   (o (cdr (assoc :o compile)))
	   (compile (remove :o (remove :compile compile :key 'car) :key 'car))
	   (compile (if o (cons (cons :output-file (or o file)) compile) compile))
	   (result (system:error-set `(apply 'compile-file ,file ',(mapcan (lambda (x) (list (car x) (cdr x))) compile)))))
      (bye (if (or *error-p* (equal result '(nil))) 1 0)))))

(defun dbl-read (&optional (stream *standard-input*) (eof-error-p t) (eof-value nil))

  (tagbody
   top
   (let ((ch (read-char stream eof-error-p eof-value)))
     (cond ((eql ch #\newline) (go top))
	   ((eq ch eof-value) (return-from dbl-read eof-value)))
     (unread-char ch stream)))

  (let* ((x (read stream eof-error-p eof-value))
	 (ch (read-char-no-hang stream eof-error-p eof-value)))
    (cond ((and ch (unread-char ch stream)))
	  ((and (keywordp x) ch)
	   (cons x (read-from-string (string-concatenate "(" (read-line stream eof-error-p eof-value) ")"))))
	  (x))))

(defvar *debug-print-level* 3)

(defun terminal-interrupt (correctablep)
  (let ((*break-enable* t))
    (if correctablep
        (cerror "Type :r to resume execution, or :q to quit to top level."
		"Console interrupt.")
        (error "Console interrupt -- cannot continue."))))


(defun break-call (key args &optional (prop 'si::break-command) &aux fun)
  (setq fun (complete-prop key 'keyword prop))
  (or fun (return-from break-call nil))
  (setq fun (get fun prop))
  (cond (fun
	 (setq args (cons fun args))
	 (or (symbolp fun) (setq args (cons 'funcall args)))
	 (evalhook args nil nil *break-env*)
	 )
	(t (format *debug-io* "~&~S is undefined break command.~%" key))))

(defun break-quit (&optional (level 0)
                   &aux (current-level (length *break-level*)))
  (when (and (>= level 0) (< level current-level))
    (let ((x (do ((v *quit-tags* (cdr v)) (i 0 (1+ i))) ((= i (- current-level level 1)) (car v)) (declare (fixnum i)))))
      (throw (cdr x) (cdr x))))
  (break-current))

(defun break-previous (&optional (offset 1))
  (do ((i (1- *current-ihs*) (1- i)))
      ((or (< i *ihs-base*) (<= offset 0))
       (set-env)
       (break-current))
    (when (ihs-visible i)
      (setq *current-ihs* i)
      (setq offset (1- offset)))))

(defun set-current ()
  (do ((i *current-ihs* (1- i)))
      ((or (ihs-visible i) (<= i *ihs-base*))
       (setq *current-ihs* i)
       (set-env)
       (format *debug-io* "Broken at ~:@(~S~).~:[  Type :H for Help.~;~]"
               (ihs-fname *current-ihs*)
               (cdr *break-level*)))))

(defun break-next (&optional (offset 1))
  (do ((i *current-ihs* (1+ i)))
      ((or (> i *ihs-top*) (< offset 0))
       (set-env)
       (break-current))
    (when (ihs-visible i)
      (setq *current-ihs* i)
      (setq offset (1- offset)))))

(defun break-go (ihs-index)
  (setq *current-ihs* (min (max ihs-index *ihs-base*) *ihs-top*))
  (if (ihs-visible *current-ihs*)
      (progn (set-env) (break-current))
      (break-previous)))

(defun break-message ()
  (princ *break-message* *debug-io*)
  (terpri *debug-io*)
  (values))

(defun describe-environment (&optional (env *break-env*) (str *debug-io*))
  (or (eql (length env) 3) (error "bad env"))
    (let ((fmt "~a~#[none~;~S~;~S and ~S~
         ~:;~@{~#[~;and ~]~S~^, ~}~].~%"))
      (apply 'format str fmt "Local variables: "
	     (mapcar #'car (car *break-env*)))
      (apply 'format str fmt "Local functions: "
	     (mapcar #'car (cadr *break-env*)))
      (apply 'format str fmt "Local blocks: "
	     (mapcan #'(lambda (x) (when (eq (cadr x) 'block) (list (car x))))
                 (caddr *break-env*)))
      (apply 'format str fmt "Local tags: "
	     (mapcan #'(lambda (x) (when (eq (cadr x) 'tag) (list (car x))))
                 (caddr *break-env*)))))

(defun break-vs (&optional (x (ihs-vs *ihs-base*)) (y (ihs-vs *ihs-top*)))
  (setq x (max x (ihs-vs *ihs-base*)))
  (setq y (min y (1- (ihs-vs (1+ *ihs-top*)))))
  (do ((ii *ihs-base* (1+ ii)))
      ((or (>= ii *ihs-top*) (>= (ihs-vs ii) x))
       (do ((vi x (1+ vi)))
           ((> vi y) (values))
         (do ()
             ((> (ihs-vs ii) vi))
           (when (ihs-visible ii) (print-ihs ii))
           (incf ii))
         (format *debug-io* "~&VS[~d]: ~s" vi (vs vi))))))

(defun break-local (&optional (n 0) &aux (x (+ (ihs-vs *current-ihs*) n)))
  (break-vs x x))

(defun break-bds (&rest vars &aux (fi *frs-base*))
  (do ((bi (1+ (frs-bds (1- *frs-base*))) (1+ bi))
       (last (frs-bds (1+ *frs-top*))))
      ((> bi last) (values))
    (when (or (null vars) (member (the symbol (bds-var bi)) vars))
      (do ()
          ((or (> fi *frs-top*) (> (frs-bds fi) bi)))
        (print-frs fi)
        (incf fi))
      (format *debug-io* "~&BDS[~d]: ~s = ~s" bi (bds-var bi) (let ((x (bds-val bi))) (if (zerop x) "unbound" (nani x)))))))

(defun simple-backtrace ()
  (princ "Backtrace: " *debug-io*)
  (do* ((i *ihs-base* (1+ i))
        (b nil t))
       ((> i *ihs-top*) (terpri *debug-io*) (values))
    (when (ihs-visible i)
      (when b (princ " > " *debug-io*))
      (write (ihs-fname i) :stream *debug-io* :escape t
             :case (if (= i *current-ihs*) :upcase :downcase)))))

(defun ihs-backtrace (&optional (from *ihs-base*) (to *ihs-top*))
  (setq from (max from *ihs-base*))
  (setq to (min to *ihs-top*))
  (do* ((i from (1+ i))
        (j (or (sch-frs-base *frs-base* from) (1+ *frs-top*))))
       ((> i to) (values))
    (when (ihs-visible i) (print-ihs i))
    (do () ((or (> j *frs-top*) (> (frs-ihs j) i)))
      (print-frs j)
      (incf j))))

(defun print-ihs (i &aux (*print-level* 2) (*print-length* 4));FIXME
  (format t "~&~:[  ~;@ ~]IHS[~d]: ~s ---> VS[~d]"
          (= i *current-ihs*)
          i
          (let ((fun (ihs-fun i)))
            (cond ((or (symbolp fun) (compiled-function-p fun)) fun)
;		  ((when (interpreted-function-p fun) (setq fun (interpreted-function-lambda fun)) nil))
                  ((consp fun)
                   (case (car fun)
                     (lambda fun)
                     ((lambda-block lambda-block-expanded) (cdr fun))
                     (lambda-closure (cons 'lambda (cddddr fun)))
                     (lambda-block-closure (cddddr fun))
                     (t (cond
			 ((and (symbolp (car fun))
			       (or (special-operator-p (car fun))
				   (fboundp (car fun))))
			  (car fun))
			 (t '(:zombi))))))
                  (t (print fun)
		   :zombi)))
          (ihs-vs i)))

(defun print-frs (i)
  (format *debug-io* "~&    FRS[~d]: ~s ---> IHS[~d],VS[~d],BDS[~d]"
          i (frs-kind i) (frs-ihs i) (frs-vs i) (frs-bds i)))

(defun frs-kind (i &aux x)
  (case (frs-class i)
    (:catch
     (if (spicep (frs-tag i))
         (or (and (setq x (member (frs-tag i) (vs (+ (frs-vs i) 2))
                                  :key #'caddr :test #'eq))
                  (if (eq (cadar x) 'block)
                      `(block ,(caar x) ***)
                      `(tagbody ,@(reverse (mapcar #'car
                                             (remove (frs-tag i) x
                                                     :test-not #'eq
                                                     :key #'caddr)))
                                ***)))
             `(block/tagbody ,(frs-tag i)))
         `(catch ',(frs-tag i) ***)))
    (:protect '(unwind-protect ***))
    (t `(system-internal-catcher ,(frs-tag i)))))

(defun break-current ()
  (if (> *current-ihs* +top-ihs+)
      (format *debug-io* "Broken at ~:@(~S~)." (ihs-fname *current-ihs*))
    (format *debug-io* "~&Top level."))
  (values))



(defvar *break-hidden-packages* nil)

(defun ihs-visible (i &aux (tem (ihs-fname i)))
  (and tem (not (member (the symbol tem) *break-hidden-packages*))))


(defun ihs-fname (ihs-index)
  (let ((fun (ihs-fun ihs-index)))
    (cond ((symbolp fun) fun)
	  ((when (compiled-function-p fun) (compiled-function-name fun)));FIXME
	  ((functionp fun) (fun-name fun));(name fun)
	   ;; (multiple-value-bind ;FIXME faster
	   ;;  (x y fun) 
	   ;;  (function-lambda-expression fun)
	   ;;  (declare (ignore x y))
	   ;;  fun))
          ((consp fun)
           (case (car fun)
             (lambda 'lambda)
             ((lambda-block lambda-block-expanded) (cadr fun))
             (lambda-block-closure (cadr (cdddr fun)))
             (lambda-closure 'lambda-closure)
             (t (if (and (symbolp (car fun))
			 (or (special-operator-p (car fun))
			     (fboundp (car fun))))
		    (car fun) :zombi)
		    )))
          (:zombi))))

(defun ihs-not-interpreted-env (ihs-index)
  (let ((fun (ihs-fun ihs-index)))
    (cond ((and (consp fun)
		(> ihs-index 3)
		;(<= (ihs-vs ihs-index) (ihs-vs (- ihs-index 1)))
		)
	   nil)
	  (t t))))

(defun set-env ()
  (setq *break-env*
        (if (ihs-not-interpreted-env *current-ihs*)
            nil
            (let ((i (ihs-vs *current-ihs*)))
              (list (vs i) (vs (1+ i)) (vs (+ i 2)))))))

(defun list-delq (x l)
  (cond ((null l) nil)
        ((eq x (car l)) (cdr l))
        (t (rplacd l (list-delq x (cdr l))))))

(defun super-go (i tag &aux x)
  (when (and (>= i *frs-base*) (<= i *frs-top*) (spicep (frs-tag i)))
    (if (setq x (member (the symbol (frs-tag i)) (vs (+ (frs-vs i) 2))
                        :key #'caddr :test #'eq))
        ; Interpreted TAGBODY.
        (when (and (eq (cadar x) 'tag)
                   (member (the symbol tag) (mapcar #'car (remove (frs-tag i) x
                                                     :test-not #'eq
                                                     :key #'caddr))))
          (internal-super-go (frs-tag i) tag t))
        ; Maybe, compiled cross-closure TAGBODY.
        ; But, it may also be compiled cross-closure BLOCK, in which case
        ; SUPER-GO just RETURN-FROMs with zero values.
        (internal-super-go (frs-tag i) tag nil)))
  (format *debug-io* "~s is invalid tagbody identification for ~s." i tag))

(defun break-backward-search-stack (sym &aux string)
  (setq string (string sym))
  (do* ((ihs (1- *current-ihs*) (1- ihs))
        (fname (ihs-fname ihs) (ihs-fname ihs)))
      ((< ihs *ihs-base*)
       (format *debug-io* "Search for ~a failed.~%" string))
    (when (and (ihs-visible ihs)
               (search string (symbol-name fname) :test #'char-equal))
      (break-go ihs)
      (return))))

(defun break-forward-search-stack (sym &aux string)
  (setq string (string sym))
  (do* ((ihs (1+ *current-ihs*) (1+ ihs))
        (fname (ihs-fname ihs) (ihs-fname ihs)))
      ((> ihs *ihs-top*)
       (format *debug-io* "Search for ~a failed.~%" string))
    (when (and (ihs-visible ihs)
               (search string (symbol-name fname) :test #'char-equal))
      (break-go ihs)
      (return))))


(putprop :b 'simple-backtrace 'break-command)
(putprop :r '(lambda () :resume) 'break-command)
(putprop :resume (get :r 'break-command) 'break-command)
(putprop :bds 'break-bds 'break-command)
(putprop :blocks 'break-blocks 'break-command)
(putprop :bs 'break-backward-search-stack 'break-command)
(putprop :c 'break-current 'break-command)
(putprop :fs 'break-forward-search-stack 'break-command)
(putprop :functions 'break-functions 'break-command)
(putprop :go 'break-go 'break-command)
(putprop :h 'break-help 'break-command)
(putprop :help 'break-help 'break-command)
(putprop :ihs 'ihs-backtrace 'break-command)
(putprop :env '(lambda () (describe-environment *break-env*)) 'break-command)
(putprop :m 'break-message 'break-command)
(putprop :n 'break-next 'break-command)
(putprop :p 'break-previous 'break-command)
(putprop :q 'break-quit 'break-command)
(putprop :s 'break-backward-search-stack 'break-command)
(putprop :vs 'break-vs 'break-command)

(defun break-help ()
  (dolist (v '( "
Break-loop Command Summary ([] indicates optional arg)
--------------------------

:bl [j]     show local variables and their values, or segment of vs if compiled
              in j stack frames starting at the current one.
:bt [n]     BACKTRACE [n steps]
:down [i]   DOWN i frames (one if no i)
:env        describe ENVIRONMENT of this stack frame (for interpreted).
:fr [n]     show frame n
:loc [i]    return i'th local of this frame if its function is compiled (si::loc i)
"
":r          RESUME (return from the current break loop).
:up [i]     UP i frames (one if no i)

Example: print a bactrace of the last 4 frames

>>:bt 4

Note:  (use-fast-links nil) makes all non system function calls
be recorded in the stack.   (use-fast-links t) is the default

Low level commands:
------------------
:p [i]           make current the i'th PREVIOUS frame (in list show by :b)
:n [i]           make current the i'th NEXT frame (in list show by :b)
:go [ihs-index]  make current the frame corresponding ihs-index
"
":m               print the last break message.
:c               show function of the current ihs frame.
:q [i]           quit to top level
:r               resume from this break loop.
:b               full backtrace of all functions and special forms.
:bs [name]       backward search for frame named 'name'
:fs  [name]      search for frame named 'name'
:vs [from] [to]  Show value stack between FROM and TO
:ihs [from] [to] Show Invocation History Stack
"
"
:bds ['v1 'v2 ..]Show previous special bindings of v1, v2,.. or all if no v1

")) (format  *debug-io* v))
  (format *debug-io* "~%Here is a COMPLETE list of bindings.   To
add a new one, add an 'si::break-command property:")
  (do-symbols (v (find-package "KEYWORD"))
	      (cond ((get v 'si::break-command)
		     (format  *debug-io*
			      "~%~(~a -- ~a~)" v (get v 'si::break-command)))))
	  (values)
	  )


;;make sure '/' terminated

(defun coerce-slash-terminated (v)
  (let ((n (length v)))
    (if (and (> n 0) (eql (aref v (1- n)) #\/))
	v
      (string-concatenate v "/"))))

(defun fix-load-path (l)
  (when (not (equal l *fixed-load-path*))
      (do ((x l (cdr x)) )
	  ((atom x))
	  (setf (car x) (coerce-slash-terminated (car x))))
      (do ((v l (cdr v)))
	  ((atom v))
	  (do ((w v (cdr w)))
	      ((atom (cdr w)))
	      (cond ((equal (cadr w) (car v))
		     (setf (cdr w)(cddr w)))))))
  (setq *fixed-load-path* l))

(defun file-search (NAME &optional (dirs *load-path*)
			  (extensions *load-types*) (fail-p t) &aux  tem)
  "Search for NAMME in DIRS with EXTENSIONS.
First directory is checked for first name and all extensions etc."
  (fix-load-path dirs)
  (dolist (v dirs)
      (dolist (e extensions)
	  (if (probe-file (setq tem (si::string-concatenate v name e)))
	    (return-from file-search tem))))
  (if fail-p
      (let ((*path* nil))
	(declare (special *path*))
	(cerror
	 "Do (setq si::*path* \"pathname\") for path to use then :r to continue"
	 "Lookup failed in directories:~s for name ~s with extensions ~s"
	 dirs name extensions)
	*path*)))

(defun aload (path)
  (load (file-search path *load-path* *load-types*)))

(defun autoload (sym path &aux (si::*ALLOW-GZIPPED-FILE* t))
  (or (fboundp sym)
      (setf (symbol-function sym)
	    #'(lambda (&rest l)
		(aload path)
		(apply sym l)))))

(defun autoload-macro (sym path &aux (si::*ALLOW-GZIPPED-FILE* t))
  (or (fboundp sym)
      (setf (macro-function sym)
	    #'(lambda (form env)
		(aload path)
		(funcall sym form env)))))

;(eval-when (compile) (proclaim '(optimize (safety 0))) )
(defvar si::*command-args* nil)

(defvar *tmp-dir*)

(defun ensure-dir-string (str)
  (if (and (eq (stat str) :directory) (not (eql #\/ (aref str (1- (length str))))))
      (string-concatenate str "/")
    str))

(defun get-temp-dir ()
  (dolist (x `(,@(mapcar 'si::getenv '("TMPDIR" "TMP" "TEMP")) "/tmp" ""))
    (when x
      (let ((x (coerce-slash-terminated x)))
	(when (eq (stat x) :directory)
	  (return-from get-temp-dir x))))))

(defun reset-sys-paths (s)
  (declare (string s))
  (setq *lib-directory* s)
  (setq *system-directory* (string-concatenate s "unixport/"))
  (let (nl)
    (dolist (l '("cmpnew/" "gcl-tk/" "lsp/" "xgcl-2/"))
      (push (string-concatenate s l) nl))
    (setq *load-path* nl))
  nil)

(defun dir-name (s &aux (i (string-match "/[^/]*$" s)))
  (if (eql -1 i) "" (subseq s 0 i)))

(defvar *lib-directory* (coerce-slash-terminated (dir-name (dir-name (kcl-self)))))

(defun set-up-top-level (&aux (i (argc)) tem)
  (declare (fixnum i))
  (reset-lib-syms)
  (setq *tmp-dir* (get-temp-dir))
  (dotimes (j i) (push (argv j) tem))
  (setq *command-args* (nreverse tem))
  (setq tem *lib-directory*)
  (process-some-args *command-args*)
  (let ((dir (getenv "GCL_LIBDIR")))
    (when dir (setq *lib-directory* (coerce-slash-terminated dir))))
  (unless (and *load-path* (equal tem *lib-directory*))
    (reset-sys-paths *lib-directory*)))

(defun do-f (file &aux *break-enable*)
  (catch *quit-tag*
    (labels ((read-loop (st &aux (tem (read st nil 'eof))) (when (eq tem 'eof) (bye)) (eval tem) (read-file st))
	     (read-file (st) (read-line st) (read-loop st)))
	    (if file
		(with-open-file
		 (st file)
		 (read-file st))
	      (read-file *standard-input*))))
  (bye 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_top.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_debug.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;;Copyright William F. Schelter 1990, All Rights Reserved 


(In-package :SYSTEM)
(import 'sloop::sloop)

(defmacro f (op &rest args)
    `(the fixnum (,op ,@ (mapcar #'(lambda (x) `(the fixnum ,x)) args) )))

(defmacro fb (op &rest args)
    `(,op ,@ (mapcar #'(lambda (x) `(the fixnum ,x)) args) ))


;;; Some debugging features:
;;; Search-stack :
;;; (:s "cal") or (:s 'cal) searches the stack for a frame whose function or 
;;; special form has a name containing "cal", moves there to display the local
;;; data.
;;;
;;; Break-locals :
;;; :bl displays the args and locals of the current function.
;;; (:bl 4) does this for 4 functions.
;;;
;;; (si:loc i)  accesses the local(i): slot.
;;; the *print-level* and *print-depth* are bound to *debug-print-level*

;;; Note you must have space < 3  in your optimize proclamation, in order for
;;; the local variable names to be saved by the compiler.

;;; With BSD You may also use the function write-debug-symbols to
;;; obtain an object file with the correct symbol information for using a
;;; c debugger, on translated lisp code.  You should have used the :debug
;;; t keyword when compiling the file.

;;; To Do: add setf method for si:loc.
;;; add restart capability from various spots on the stack.

(defun show-break-variables (&optional (n 1))
  (loop
					;(break-current)
   (dolist (v (reverse(car *break-env*)))
     (format *debug-io* "~%~9a: ~s" (car v) (second v)))
   (or (fb >  (incf  n -1) 0) (return (values)))
   (break-previous)
   ))

(defun show-environment (ihs)
  (let ((lis  (vs (ihs-vs ihs))))
    (if (listp lis)
	(dolist (v (reverse (vs (ihs-vs ihs))))
	  (format *debug-io* "~%~9a: ~s" (car v) (second v))))))

(putprop :a 'show-break-variables 'break-command)

;;make hack in compiler to remember the local variable names for the 
;;vs variables and associate it with the function name

(defun search-stack (sym &aux string);FIXME
  (setq string (cond((symbolp sym)(symbol-name sym))
		    (t sym)))
  (sloop
     for ihs downfrom (ihs-top) above 2
     for fun = (ihs-fun ihs) with name
     do 
     (cond ((compiled-function-p fun)
	    (setq name (compiled-function-name fun)))
	   ((symbolp fun ) (setq name fun))
;	   ((when (interpreted-function-p fun) (setq fun (interpreted-function-lambda fun)) nil))
	   ((and (listp fun)
		 (member (car fun) '(lambda lambda-block)))
	    (setq name (second fun)))
	   (t (setq name '||)))
     when (search string (symbol-name name) :test 'equal)
     do (return (progn (break-go ihs)(terpri) (break-locals)))
     finally (format *debug-io* "~%Search for ~a failed" string)
     ))

(defvar *debug-print-level* 3)

(defun break-locals (&optional (n 1) ;FIXME
			       &aux (ihs *current-ihs*)
			       (base  (ihs-vs ihs))
			       (*print-level* *debug-print-level*)
			       (*print-circle* t)
			       (*print-length* *debug-print-level*)
			       (current-ihs *current-ihs*)
			       (fun (ihs-fun ihs)) name args)
  (cond ((fb > n 1)
	 (sloop for i below n
	    for ihs downfrom current-ihs above 2
	    do (let ((*current-ihs* ihs))
		 (break-locals) (terpri)(terpri)
		 )))
	(t
	 (cond ((compiled-function-p fun)
		(setq name (compiled-function-name fun)))
;	       ((when (interpreted-function-p fun) (setq fun (interpreted-function-lambda fun)) nil))
	       (t (setq name fun)))
         (if (symbolp name)(setq args (get name 'debugger)))
	 (let ((next (ihs-vs (f + 1 *current-ihs*))))
	   (cond (next
		  (format *debug-io* ">> ~a():" name)
		  (cond ((symbolp name)     
			 (sloop for i from base below next for j from 0
			    for u = nil
			    do 
			    (cond ((member 0 args);;old debug info.
				   (setf u (getf  args j)))
				  (t (setf u (nth j args))))
			    (cond (u
				   (format t
					   "~%Local~a(~a): ~a" j u  (vs i)))
				  (t
				   (format *debug-io* "~%Local(~d): ~a"
					   j (vs i))))))
			((listp name)
			 (show-environment  ihs))
			(t (format *debug-io* "~%Which case is this??")))))))))

(defun loc (&optional (n 0))
  (let ((base (ihs-vs *current-ihs*)))
    (unless  (and (fb >= n 0)
		  (fb < n (f - (ihs-vs
				(min (ihs-top) (f + 1 *current-ihs*)))
			     base)))
	     (error "Not in current function"))
    (vs (f + n base))))

(putprop :bl 'break-locals 'break-command)
(putprop :s 'search-stack 'break-command)

(defvar *record-line-info* (make-hash-table :test 'eq))

(defvar *at-newline* nil)

(defvar *standard-readtable* *readtable*)

(defvar *line-info-readtable* (copy-readtable))

(defvar *left-parenthesis-reader* (get-macro-character #\( ))

(defvar *quotation-reader* (get-macro-character #\" ))

(defvar *stream-alist* nil)

(defvar *break-point-vector* (make-array 10 :fill-pointer 0 :adjustable t))

(defvar *step-next* nil)

(defvar *last-dbl-break* nil)

#-gcl
(eval-when (compile eval load)

(defvar *places* '(|*mv0*| |*mv1*| |*mv2*| |*mv3*| |*mv4*| |*mv5*| |*mv6*| |*mv7*|
		     |*mv8*| |*mv9*|))

(defmacro set-mv (i val) `(setf ,(nth i *places*) ,val))

(defmacro mv-ref (i) (nth i *places*))
  )

(defmacro mv-setq (lis form)
  `(prog1 (setf ,(car lis) ,form)
     ,@ (do ((v (cdr lis) (cdr v))
	     (i 0 (1+ i))
	     (res))
	    ((null v)(reverse res))
	  (push `(setf ,(car v) (mv-ref ,i)) res))))

(defmacro mv-values (&rest lis)
  `(prog1 ,(car lis)
     ,@ (do ((v (cdr lis) (cdr v))
	     (i 0 (1+ i))
	     (res))
	    ((null v)(reverse res))
	  (push `(set-mv ,i ,(car v)) res))))

;;start a lisp debugger loop.   Exit it by using :step

(defun dbl ()
  (break-level nil nil))

(defun stream-name (str) (namestring (pathname str)))

(defstruct instream stream (line 0 :type fixnum) stream-name)


(eval-when (eval compile)

(defstruct (bkpt (:type list)) form file file-line function)
  )

(defun cleanup ()
  (dolist (v *stream-alist*)
    (unless (open-stream-p (instream-stream v))
      (setq *stream-alist* (delete v *stream-alist*)))))

(defun get-instream (str)
  (or (dolist (v *stream-alist*)
	(cond ((eq str (instream-stream v))
	       (return v))))
      (car (setq *stream-alist*
		 (cons  (make-instream :stream str
                                     :stream-name (if (streamp str)
                                               (stream-name str))
   ) *stream-alist*)))))

(defun newline (str ch)
  (declare (ignore ch))
  (let ((in (get-instream str)))
    (setf (instream-line in) (the fixnum (f + 1 (instream-line in)))))
  ;; if the next line begins with '(', then record all cons's eg arglist )
  (setq *at-newline*  (if (eql (peek-char nil str nil) #\() :all t))
  (values))

(defun quotation-reader (str ch)
  (let ((tem (funcall *quotation-reader* str ch))
	(instr (get-instream str)))
    (incf (instream-line instr) (count #\newline tem))
    tem))

(defvar *old-semicolon-reader* (get-macro-character #\;))

(defun new-semi-colon-reader (str ch)
  (let ((in (get-instream str))
	(next (peek-char nil str nil nil)))
    (setf (instream-line in) (the fixnum (f + 1 (instream-line in))))
    (cond ((eql next #\!)
	   (read-char str)
	   (let* ((*readtable* *standard-readtable*)
		  (command (read-from-string (read-line str nil nil))))
	     (cond ((and (consp command)
			 (eq (car command) :line)
			 (stringp (second command))
			 (typep (third command) 'fixnum))
		    (setf (instream-stream-name in) (second command))
		    (setf (instream-line in) (third command))))
	     ))
	  (t    (funcall *old-semicolon-reader* str ch)))
    (setq *at-newline*  (if (eql (peek-char nil str nil) #\() :all t))
    (values)))

(defun setup-lineinfo ()
  (set-macro-character #\newline #'newline nil *line-info-readtable*)
  (set-macro-character #\; #'new-semi-colon-reader nil *line-info-readtable*)
  (set-macro-character #\( 'left-parenthesis-reader nil *line-info-readtable*)
  (set-macro-character #\" 'quotation-reader nil *line-info-readtable*)
  
  )

(defun nload (file &rest args )
  (clrhash *record-line-info*)
  (cleanup)
  (setq file (truename file))
  (setup-lineinfo)
  (let ((*readtable* *line-info-readtable*))
    (apply 'load file args)))

(eval-when (compile eval)

(defmacro break-data (name line) `(cons ,name ,line))
  )

(defun left-parenthesis-reader (str ch &aux line(flag *at-newline*))
  (if (eq *at-newline* t) (setq *at-newline* nil))
  (when flag
    (setq flag (get-instream str))
    (setq line (instream-line flag))
    )
  (let ((tem (funcall *left-parenthesis-reader* str ch)))
    (when flag
      (setf (gethash tem *record-line-info*)
	    (break-data (instream-name flag)
			line)))
    tem))

(defvar *fun-array* (make-array 50 :fill-pointer 0 :adjustable t))

(defun walk-through (body &aux tem)
  (tagbody
   top
   (cond ((consp body)
	  (when (setq tem (gethash body *record-line-info*))
	    ;; lines beginning with ((< u v)..)
	    ;; aren't eval'd but are part of a special form
	    (cond ((and (consp (car body))
			(not (eq (caar body) 'lambda)))
		   (remhash body *record-line-info*)
		   (setf (gethash (car body) *record-line-info*)
			 tem))
		  (t (vector-push-extend (cons tem body) *fun-array*))))
	  (walk-through (car body))
	  (setq body (cdr body))
	  (go top))
	 (t nil))))

;; (defun compiler::compiler-def-hook (name body &aux (ar *fun-array*)
;; 					 (min most-positive-fixnum)
;; 					 (max -1))
;;   (declare (fixnum min max))
;;   ;;  (cond ((and (boundp '*do-it*)
;;   ;;	      (eq (car body) 'lambda-block))
;;   ;;	 (setf (cdr body) (cdr  (walk-top body)))))
	 
;;   (cond ((atom body)
;; 	 (remprop name 'line-info))
;; 	((eq *readtable* *line-info-readtable*) 
;; 	 (setf (fill-pointer *fun-array*) 0)
;; 	 (walk-through body)
;; 	 (dotimes (i (length ar))
;; 		  (declare (fixnum i))
;; 		  (let ((n (cdar (aref ar i))))
;; 		    (declare (fixnum n))
;; 		    (if (fb > n max) (setf max n))
;; 		    (if (fb < n min) (setf min n))))
;; 	 (cond ((fb > (length *fun-array*) 0)
;; 	        (let ((new (make-array (f + (f - max min) 2)
;; 				       :initial-element :blank-line))
;; 		      (old-info (get name 'line-info)))
;; 		  (setf (aref new 0)
;; 			(cons (caar (aref ar 0)) min))
;; 		  (setq min (f - min 1))
;; 		  (dotimes (i (length ar))
;; 			   (let ((y (aref ar i)))
;; 			     (setf (aref new (f - (cdar y) min))
;; 				   (cdr y))))
;; 		  (setf (get name 'line-info) new)
;; 		  (when
;; 		      old-info
;; 		    (let ((tem (get name 'break-points))
;; 			  (old-begin (cdr (aref old-info 0))))
;; 		      (dolist (bptno tem)
;; 			(let* ((bpt (aref *break-points* bptno))
;; 			       (fun (bkpt-function bpt))
;; 			       (li (f - (bkpt-file-line bpt) old-begin)))
;; 			  (setf (aref *break-points* bptno)
;; 				(make-break-point fun  new li))))))))
;; 	       (t (let ((tem (get name 'break-points)))
;; 		    (iterate-over-bkpts tem :delete)))))))

(defun instream-name (instr)
  (or (instream-stream-name instr)
      (stream-name (instream-stream instr))))

(defun find-line-in-fun (form env fun  counter &aux tem)
  (setq tem (get fun 'line-info))
  (if tem
      (let ((ar tem))
	(declare ((array t) ar))
	(when ar
	  (dotimes
	   (i (length ar))
	   (cond ((eq form (aref ar i))
		  (when counter
		    (decf (car counter))
		    (cond ((fb > (car counter) 0)
					;silent
			   (return-from find-line-in-fun :break))))
		  (break-level
		   (setq *last-dbl-break* (make-break-point fun  ar i)) env
		   )
		  (return-from find-line-in-fun :break))))))))

;; get the most recent function on the stack with step info.

(defun current-step-fun ( &optional (ihs (ihs-top)) )
  (do ((i (1- ihs) (f - i 1)))
      ((fb <=  i 0))
    (let ((na (ihs-fname i)))
      (if (get na 'line-info) (return na)))))

(defun init-break-points ()
  (setf (fill-pointer *break-point-vector*) 0)
  (setf *break-points* *break-point-vector*))

(defun step-into (&optional (n 1))
;(defun step-into ()
  (declare (ignore n))
  ;;FORM is the next form about to be evaluated.
  (or *break-points* (init-break-points))
  (setq *break-step* 'break-step-into)
  :resume)

(defun step-next ( &optional (n 1))
  (let ((fun (current-step-fun)))
    (setq *step-next* (cons n fun))
    (or *break-points* (init-break-points))
    (setq *break-step* 'break-step-next)
    :resume))

(defun maybe-break (form line-info fun env &aux pos)
  (cond ((setq pos (position form line-info))
	 (setq *break-step* nil)
	 (or (> (length *break-points*) 0)
	     (setf *break-points* nil))
	 (break-level (make-break-point fun line-info pos) env)
	 t)))

;; These following functions, when they are the value of *break-step*
;; are invoked by an inner hook in eval.   They may choose to stop
;; things.

(defun break-step-into (form env)
  (let ((fun (current-step-fun)))
    (let ((line-info (get fun 'line-info)))
      (maybe-break form line-info fun env))))

(defun break-step-next (form env)
  (let ((fun (current-step-fun)))
    (cond ((eql (cdr *step-next*) fun)
	   (let ((line-info (get fun 'line-info)))
	     (maybe-break form line-info fun env))))))

(setf (get :next 'break-command) 'step-next)
(setf (get :step 'break-command) 'step-into)
(setf (get :loc 'break-command) 'loc)


(defun *break-points* (form  env) 
  (let ((pos(position form *break-points* :key 'car)))
    (format t "Bkpt ~a:" pos)
    (break-level  (aref *break-points* pos) env)))


(defun dwim (fun)
  (dolist (v (list-all-packages))
    (multiple-value-bind
     (sym there)
     (intern (symbol-name fun) v)
     (cond ((get sym 'line-info)
	    (return-from dwim sym))
	   (t (or there (unintern sym))))))
  (format t "~a has no line information" fun))

(defun break-function (fun &optional (li 1)  absolute  &aux fun1)
  (let ((ar (get fun 'line-info)))
    (when (null ar) (setq fun1 (dwim fun))
	  (if fun1 (return-from break-function
				(break-function fun1 li absolute))))
    (or (arrayp ar)(progn (format t "~%No line info for ~a" fun)
			  (return-from break-function nil)))
    (let ((beg (cdr (aref ar 0))))
      (if absolute (setq li (f - li beg)))
      (or (and (fb >= li 1) (fb < li (length ar)))
	  (progn (format t "~%line out of bounds for ~a" fun))
	  (return-from break-function nil))
      (if (eql li 1)
	  (let ((tem (symbol-function fun)))
	    (cond ((and (consp tem)
			(eq (car tem) 'lambda-block)
			(third tem))
		   (setq li 2)))))
      (dotimes (i (f - (length ar) li))
	       (when (not (eq (aref ar i) :blank-line))
		 (show-break-point (insert-break-point
				    (make-break-point fun ar (f + li i))))
		 (return-from break-function (values))))
      (format t "~%Beyond code for ~a "))))

(defun insert-break-point (bpt &aux at)
  (or *break-points* (init-break-points))
  (setq at (or (position nil *break-points*)
	       (prog1 (length *break-points*)
		 (vector-push-extend  nil *break-points*)
		 )))
  (let ((fun (bkpt-function bpt)))
    (push at (get fun 'break-points)))
  (setf (aref *break-points* at) bpt)
  at)

(defun short-name (name)
  (let ((Pos (position #\/ name :from-end t)))
    (if pos (subseq name (f + 1 pos)) name)))

(defun show-break-point (n &aux disabled)
  (let ((bpt (aref *break-points* n)))
    (when bpt
      (when (eq (car bpt) nil)
	(setq disabled t)
	(setq bpt (cdr bpt)))
      (format t "Bkpt ~a:(~a line ~a)~@[(disabled)~]"
	      n (short-name (second bpt))
	      (third bpt) disabled)
      (let ((fun (fourth bpt)))
	(format t "(line ~a of ~a)"  (relative-line fun (nth 2 bpt))
		fun
		)))))

(defun iterate-over-bkpts (l action)
  (dotimes (i (length *break-points*))
	   (if (or (member i l)
		   (null l))
	       (let ((tem (aref *break-points* i)))
		 (setf (aref *break-points* i)
		       (case action
			 (:delete
			  (if tem (setf (get (bkpt-function tem) 'break-points)
					(delete i (get (bkpt-function tem) 'break-points))))
			  nil)
			 (:enable
			  (if (eq (car tem) nil) (cdr tem) nil))
			 (:disable
			  (if (and tem (not (eq (car tem) nil)))
			      (cons nil tem)
			    tem))
			 (:show
			  (when tem (show-break-point i)
				(terpri))
			  tem
			  )))))))

(setf (get :info 'break-command)
      '(lambda (type)
	 (case type
	   (:bkpt  (iterate-over-bkpts nil :show))
	   (otherwise
	    (format t "usage: :info :bkpt -- show breakpoints")
	    ))))

(defun complete-prop (sym package prop &optional return-list)
  (cond ((and (symbolp sym)(get sym prop)(equal (symbol-package sym)
						 (find-package package)))
	 (return-from complete-prop sym)))
  (sloop for v in-package package 
	 when (and (get v prop)
		   (eql (string-match sym v) 0))
	 collect v into all
	 finally
       
         (cond (return-list (return-from complete-prop all))
               ((> (length all) 1)
	                (format t "~&Not unique with property ~(~a: ~{~s~^, ~}~)."
			prop all))

		       ((null all)
			(format t "~& ~a is not break command" sym))
		       (t (return-from complete-prop
				       (car all))))))

(setf (get :delete 'break-command)
      '(lambda (&rest l) (iterate-over-bkpts l :delete)(values)))
(setf (get :disable 'break-command)
      '(lambda (&rest l) (iterate-over-bkpts l :disable)(values)))
(setf (get :enable 'break-command)
      '(lambda (&rest l) (iterate-over-bkpts l :enable)(values)))
(setf (get :break 'break-command)
      '(lambda (&rest l)
	 (print l)
	 (cond (l
		(apply 'si::break-function l))
	       (*last-dbl-break*
		(let ((fun  (nth 3 *last-dbl-break*)))
		  (si::break-function fun (nth 2 *last-dbl-break*) t))))))

(setf (get :fr 'break-command)
      '(lambda (&rest l )
	 (dbl-up (or (car l) 0) *ihs-top*)
	 (values)))

(setf (get :up 'break-command)
      '(lambda (&rest l )
	 (dbl-up (or (car l) 1) *current-ihs*)
	 (values)))

(setf (get :down 'break-command)
      '(lambda (&rest l )
	 (dbl-up ( - (or (car l) 1)) *current-ihs*)
	 (values)))

;; in other common lisps this should be a string output stream.

(defvar *display-string*
  (make-array 100 :element-type 'character :fill-pointer 0 :adjustable t))

(defun display-env (n env)
  (do ((v (reverse env) (cdr v)))
      ((or (not (consp v)) (fb > (fill-pointer *display-string*) n)))
    (or (and (consp (car v))
	     (listp (cdar v)))
	(return))
    (format *display-string* "~s=~s~@[,~]" (caar v) (cadar v) (cdr v))))

(defun apply-display-fun (display-fun  n lis)  
  (let ((*print-length* *debug-print-level*)
	(*print-level* *debug-print-level*)
	(*print-pretty* nil)
	(*PRINT-CASE* :downcase)
	(*print-circle* t)
	)
    (setf (fill-pointer *display-string*) 0)
    (format *display-string* "{")
    (funcall display-fun n lis)
    (when (fb > (fill-pointer *display-string*) n)
      (setf (fill-pointer *display-string*) n)
      (format *display-string* "..."))

    (format *display-string* "}")
    )
  *display-string*
  )

(setf (get :bt 'break-command) 'dbl-backtrace)
(setf (get '*break-points* 'dbl-invisible) t)

(defun get-line-of-form (form line-info)
  (let ((pos (position form line-info)))
    (if pos (f + pos (cdr (aref line-info 0))))))

(defun get-next-visible-fun (ihs)
  (do ((j  ihs (f - j 1)))
      ((fb < j *ihs-base*)
       (mv-values nil j))
    (let
	((na  (ihs-fname j)))
      (cond ((special-operator-p na))
	    ((get na 'dbl-invisible))
	    ((fboundp na)(return (mv-values na j)))))))

(defun dbl-what-frame (ihs &aux (j *ihs-top*) (i 0) na)
  (declare (fixnum ihs j i) (ignorable na))
  (loop
   (mv-setq (na j)   (get-next-visible-fun j))
   (cond ((fb <= j ihs) (return i)))
   (setq i (f + i 1))
   (setq j (f -  j 1))))

(defun dbl-up (n ihs &aux m fun line file env )
  (setq m (dbl-what-frame ihs))
  (cond ((fb >= n 0)
	 (mv-setq (*current-ihs*  n  fun line file env)
		  (nth-stack-frame n ihs))
	 (set-env)
	 (print-stack-frame (f + m n) t *current-ihs* fun line file env))
	(t (setq n (f + m n))
	   (or (fb >= n 0) (setq n 0))
	   (dbl-up n *ihs-top*))))
	
(dolist (v '( break-level universal-error-handler terminal-interrupt
			  break-level   evalhook find-line-in-fun))
  (setf (get v 'dbl-invisible) t))

(defun next-stack-frame (ihs &aux line-info li i k na)
  (cond
   ((fb < ihs *ihs-base*) (mv-values nil nil nil nil nil))
   (t (let (fun)
	;; next lower visible ihs
	(mv-setq (fun i) (get-next-visible-fun  ihs))
	(setq na fun)
	(cond
	 ((and
	   (setq line-info (get fun 'line-info))
	   (do ((j (f + ihs 1) (f - j 1)))
;		(form ))
	       ((<= j i) nil)
;	     (setq form (ihs-fun j))
	     (cond ((setq li (get-line-of-form (ihs-fun j) line-info))
		    (return-from next-stack-frame 
				 (mv-values
				  i fun li
				  ;; filename
				  (car (aref line-info 0))
				  ;;environment
				  (list (vs (setq k (ihs-vs j)))
					(vs (1+ k))
					(vs (+ k 2)))
				  )))))))
	 ((special-operator-p na) nil)
	 ((get na 'dbl-invisible))
	 ((fboundp na)
	  (mv-values i na nil nil
		     (if (ihs-not-interpreted-env i)
			 nil
		       (let ((i (ihs-vs i)))
			 (list (vs i) (vs (1+ i)) (vs (f + i 2)))))))
	 ((mv-values nil nil nil nil nil)))))))

(defun nth-stack-frame (n &optional (ihs *ihs-top*)
			  &aux  name line file env next)
  (or (fb >= n 0) (setq n 0))
  (dotimes (i (f + n 1))
	   (setq next (next-stack-frame ihs))
	   (cond (next
		  (mv-setq (ihs name line file env) next)
		  (setq ihs (f - next 1)))
		 (t (return (setq n (f - i 1))))))
  
  (setq ihs (f + ihs 1) name (ihs-fname ihs))
  (mv-values ihs n name line file env ))

(defun dbl-backtrace (&optional (m 1000) (ihs *ihs-top*) &aux fun  file
				line env (i 0))
  (loop
   (mv-setq  (ihs fun line file  env)  (next-stack-frame ihs))
   (or fun (return nil))
   (print-stack-frame i nil ihs fun line file env)
   (incf i)
   (cond ((fb >= i m) (return (values))))
   (setq ihs (f - ihs 1))
   )
  (values))

(defun display-compiled-env ( plength ihs &aux
				      (base (ihs-vs ihs))
				      (end (min (ihs-vs (1+ ihs)) (vs-top))))
  (format *display-string* "")
  (do ((i base)
       (v (get (ihs-fname ihs) 'debugger) (cdr v)))
      ((or (fb >= i end)(fb > (fill-pointer *display-string*) plength)(= 0 (address (vs i)))));FIXME
    (format *display-string* "~a~@[~d~]=~s~@[,~]"
	    (or (car v)  'loc) (if (not (car v)) (f - i base)) (vs i)
	    (fb < (setq i (f + i 1)) end))))

(defun computing-args-p (ihs)
  ;; When running interpreted we want a line like
  ;; (list joe jane) to get recorded in the invocation
  ;; history while joe and jane are being evaluated,
  ;; even though list has not yet been invoked.   We put
  ;; it in the history, but with the previous lexical environment.
  (and (consp (ihs-fun ihs))
       (> ihs 3)
       (not (member (car (ihs-fun ihs)) '(lambda-block lambda)))
       ;(<= (ihs-vs ihs) (ihs-vs (- ihs 1)))
       )
  )


(defun print-stack-frame (i auto-display ihs fun &optional line file env)
  (declare (ignore env))
  (when (and auto-display line)
    (format *debug-io* "~a:~a:0:beg~%" file line))
  (let  ((computing-args (computing-args-p ihs)))
    (format *debug-io* "~&#~d  ~@[~a~] ~a ~@[~a~] " i
	    (and computing-args "Computing args for ")
	    fun
	    (if (not (ihs-not-interpreted-env ihs))
		(apply-display-fun 'display-env  80
				   (car (vs (ihs-vs ihs))))
	      (apply-display-fun 'display-compiled-env 80 ihs)))
    (if file (format *debug-io* "(~a line ~a)" file line))
    (format *debug-io* "[ihs=~a]"  ihs)
    ))

(defun make-break-point (fun ar i)
  (list					;make-bkpt	;:form
   (aref ar i)
					;:file
   (car (aref ar 0))
					;:file-line
   (f + (cdr (aref  ar 0)) i)
					;:function
   fun)
  )

(defun relative-line (fun l)
  (let ((info (get fun 'line-info)))
    (if info (f - l (cdr (aref info 0)))
      0)))

(defvar *step-display* nil)

(defvar *null-io* (make-broadcast-stream))
;; should really use serror to evaluate this inside.
;; rather than just quietening it.   It prints a long stack
;; which is time consuming.

(defun safe-eval (form env &aux *break-enable*)
  (let ((*error-output* *null-io*)
	(*debug-io* *null-io*))
    (cond ((symbolp form)
	   (unless (or (boundp form)
		       (assoc form (car env)))
		   (return-from safe-eval :<error>))))
    (multiple-value-bind (er val)
			 (si::error-set
			  `(evalhook ',form nil nil ',env))
			 (if er :<error> val))))

(defvar *no-prompt* nil)

(defun set-back (at env &aux (i *current-ihs*))
  (setq *no-prompt* nil)
  (setq *current-ihs* i)
  (cond (env   (setq *break-env* env))
	(t (list   (vs (ihs-vs i)))))
  
  (when (consp at)
    (format *debug-io* "~a:~a:0:beg~%" (second at) (third at))
    (format *debug-io* "(~a line ~a) "
	    (second at)  (third at))
    )
  (dolist (v *step-display*)
    (let ((res (safe-eval v env)))
      (or (eq res :<error>)
	  (format t "(~s=~s)" v res)))))


(eval-when (load eval)
  (pushnew :sdebug *features* )
					;(use-fast-links nil)
  )









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_debug.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_setf.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;        setf.lsp
;;;;
;;;;                                setf routines


;; (in-package 'lisp)


;; (export '(setf psetf shiftf rotatef
;;           define-modify-macro defsetf
;;           getf remf incf decf push pushnew pop
;; ;         define-setf-method
;; 	  define-setf-expander
;; ;	  get-setf-method
;; 	  get-setf-expansion
;; ;	  get-setf-method-multiple-value
;; 	  ))


(in-package :system)


;(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))
;(eval-when (eval compile) (defun si:clear-compiler-properties (symbol code)))
(eval-when (eval compile) (setq si:*inhibit-macro-special* nil))

(defun join (l1 l2)
  (do (r rp (l1 l1 (cdr l1)) (l2 l2 (cdr l2)))
      ((or (endp l1) (endp l2)) r)
      (let ((tmp (cons (list (car l1) (car l2)) nil)))
	(setq rp (cond (rp (rplacd rp tmp) tmp) ((setq r tmp)))))))

(defun to-gensyms (l1)
  (do (r rp (l1 l1 (cdr l1)))
      ((endp l1) r)
      (let ((tmp (cons (gensym) nil)))
	(setq rp (cond (rp (rplacd rp tmp) tmp) ((setq r tmp)))))))

(defun to-quote-cadadr (l1)
  (do (r rp (l1 l1 (cdr l1)))
      ((endp l1) r)
      (let ((tmp (cons (list 'quote (cadadr (car l1))) nil)))
	(setq rp (cond (rp (rplacd rp tmp) tmp) ((setq r tmp)))))))

;;; DEFSETF macro.
(defmacro defsetf (access-fn &rest rest)
  (declare (optimize (safety 2)))
  (cond ((and (car rest) (or (symbolp (car rest)) (functionp (car rest))))
         `(eval-when(compile eval load)
                 (si:putprop ',access-fn ',(car rest) 'setf-update-fn)
                 (remprop ',access-fn 'setf-lambda)
                 (remprop ',access-fn 'setf-method)
                 (si:putprop ',access-fn
                             ,(when (not (endp (cdr rest)))
                                    (unless (stringp (cadr rest))
                                            (error "A doc-string expected."))
                                    (unless (endp (cddr rest))
                                            (error "Extra arguments."))
                                    (cadr rest))
                             'setf-documentation)
                 ',access-fn))
	(t
	 (unless (= (list-length (cadr rest)) 1)
		 (error "(store-variable) expected."))
	 (multiple-value-bind
	  (doc decls ctps body)
	  (parse-body-header (cddr rest))
	  (declare (ignore ctps))
	  `(eval-when (compile eval load)
		      (si:putprop 
		       ',access-fn 
		       (lambda ,(car rest) ,@decls (lambda ,(cadr rest) (block ,access-fn ,@body))) 'setf-lambda)
		      (remprop ',access-fn 'setf-update-fn)
		      (remprop ',access-fn 'setf-method)
		      (si:putprop ',access-fn
				  ,doc
				  'setf-documentation)
		      ',access-fn)))))
  
  
;;; DEFINE-SETF-METHOD macro.
(defmacro define-setf-method (access-fn &rest rest &aux body)
  (multiple-value-bind
   (args env) 
   (get-&environment (car rest))
   (setq body (cdr rest))
   (cond (env (setq args (cons env args)))
	 ((setq args (cons (gensym) args))
	  (push `(declare (ignore ,(car args))) body)))
   `(eval-when (compile eval load)
	       (si:putprop ',access-fn
			   #'(lambda ,args (block ,access-fn ,@ body)) 'setf-method)
	       (remprop ',access-fn 'setf-lambda)
	       (remprop ',access-fn 'setf-update-fn)
	       (si:putprop ',access-fn
			   ,(find-documentation (cdr rest))
			   'setf-documentation)
	       ',access-fn)))

(defmacro define-setf-expander (access-fn &rest rest)
  (declare (optimize (safety 2)))
  `(define-setf-method ,access-fn ,@rest))

;;; GET-SETF-METHOD.
;;; It just calls GET-SETF-METHOD-MULTIPLE-VALUE
;;;  and checks the number of the store variable.
(defun get-setf-method (form &optional env)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method-multiple-value form env)
    (unless (= (list-length stores) 1)
	    (error "Multiple store-variables are not allowed."))
    (values vars vals stores store-form access-form)))

(defun get-setf-expansion (form &optional env)
  (declare (optimize (safety 2)))
  (get-setf-method form env))

;;;; GET-SETF-METHOD-MULTIPLE-VALUE.

;; FIXME  when all is well, remove this and the setf tests in the pcl directory
(push :setf *features*)

(defun get-setf-method-multiple-value (form &optional env &aux tem)
  (cond ((symbolp form)
	 (let ((store (gensym)))
	   (values nil nil (list store) `(setq ,form ,store) form)))
	((or (not (consp form)) (not (symbolp (car form))))
	 (error "Cannot get the setf-method of ~S." form))
	((multiple-value-bind 
	  (t1 exp) (macroexpand form env)
	  (when exp (setq tem t1)))
	 (get-setf-method-multiple-value tem env))
;	((and env (setq tem (assoc (car form) (second env))))
;	 (setq tem (macroexpand form env))
;	 (if (eq form tem) (error "Cannot get setf-method for ~a" form))
;	 (return-from get-setf-method-multiple-value
;		      (get-setf-method-multiple-value tem  env)))
	((get (car form) 'setf-method)
	 (apply (get (car form) 'setf-method) env (cdr form)))
	((or (get (car form) 'setf-update-fn)
	     (setq tem (get (car form) 'si::structure-access)))
	 (let ((vars (to-gensyms (cdr form)))
	       (store (gensym)))
	   (values vars (cdr form) (list store)
	           (cond (tem (setf-structure-access (car vars) (car tem) (cdr tem) store))
			 ((let ((f (get (car form) 'setf-update-fn)))
			    `(,f ,@vars ,store))))
		   (cons (car form) vars))))
	((get (car form) 'setf-lambda)
	 (let* ((vars (to-gensyms (cdr form)))
		(store (gensym))
		(f (get (car form) 'setf-lambda)))
		;; this looks bogus to me.  What if l is compiled?--wfs
;		(f `(lambda ,(car l) #'(lambda ,(cadr l) ,@(cddr l)))))
	   (values vars (cdr form) (list store)
		   (funcall (apply f vars) store)
		   (cons (car form) vars))))
	((macro-function (car form))
	 (get-setf-method-multiple-value (macroexpand form env)))
	(t 
	 (let ((vars (to-gensyms (cdr form)))
	       (store (gensym)))
	   (values vars (cdr form) (list store)
	           `(funcall
		     #'(setf ,(car form))
		     ,store ,@vars )
		   (cons (car form) vars))))))


;;;; SETF definitions.

(defsetf car (x) (y) `(progn (rplaca ,x ,y) ,y))
(defsetf cdr (x) (y) `(progn (rplacd ,x ,y), y))
(defsetf caar (x) (y) `(progn (rplaca (car ,x) ,y) ,y))
(defsetf cdar (x) (y) `(progn (rplacd (car ,x) ,y) ,y))
(defsetf cadr (x) (y) `(progn (rplaca (cdr ,x) ,y) ,y))
(defsetf cddr (x) (y) `(progn (rplacd (cdr ,x) ,y) ,y))
(defsetf caaar (x) (y) `(progn (rplaca (caar ,x) ,y) ,y))
(defsetf cdaar (x) (y) `(progn (rplacd (caar ,x) ,y) ,y))
(defsetf cadar (x) (y) `(progn (rplaca (cdar ,x) ,y) ,y))
(defsetf cddar (x) (y) `(progn (rplacd (cdar ,x) ,y) ,y))
(defsetf caadr (x) (y) `(progn (rplaca (cadr ,x) ,y) ,y))
(defsetf cdadr (x) (y) `(progn (rplacd (cadr ,x) ,y) ,y))
(defsetf caddr (x) (y) `(progn (rplaca (cddr ,x) ,y) ,y))
(defsetf cdddr (x) (y) `(progn (rplacd (cddr ,x) ,y) ,y))
(defsetf caaaar (x) (y) `(progn (rplaca (caaar ,x) ,y) ,y))
(defsetf cdaaar (x) (y) `(progn (rplacd (caaar ,x) ,y) ,y))
(defsetf cadaar (x) (y) `(progn (rplaca (cdaar ,x) ,y) ,y))
(defsetf cddaar (x) (y) `(progn (rplacd (cdaar ,x) ,y) ,y))
(defsetf caadar (x) (y) `(progn (rplaca (cadar ,x) ,y) ,y))
(defsetf cdadar (x) (y) `(progn (rplacd (cadar ,x) ,y) ,y))
(defsetf caddar (x) (y) `(progn (rplaca (cddar ,x) ,y) ,y))
(defsetf cdddar (x) (y) `(progn (rplacd (cddar ,x) ,y) ,y))
(defsetf caaadr (x) (y) `(progn (rplaca (caadr ,x) ,y) ,y))
(defsetf cdaadr (x) (y) `(progn (rplacd (caadr ,x) ,y) ,y))
(defsetf cadadr (x) (y) `(progn (rplaca (cdadr ,x) ,y) ,y))
(defsetf cddadr (x) (y) `(progn (rplacd (cdadr ,x) ,y) ,y))
(defsetf caaddr (x) (y) `(progn (rplaca (caddr ,x) ,y) ,y))
(defsetf cdaddr (x) (y) `(progn (rplacd (caddr ,x) ,y) ,y))
(defsetf cadddr (x) (y) `(progn (rplaca (cdddr ,x) ,y) ,y))
(defsetf cddddr (x) (y) `(progn (rplacd (cdddr ,x) ,y) ,y))
(defsetf first (x) (y) `(progn (rplaca ,x ,y) ,y))
(defsetf second (x) (y) `(progn (rplaca (cdr ,x) ,y) ,y))
(defsetf third (x) (y) `(progn (rplaca (cddr ,x) ,y) ,y))
(defsetf fourth (x) (y) `(progn (rplaca (cdddr ,x) ,y) ,y))
(defsetf fifth (x) (y) `(progn (rplaca (cddddr ,x) ,y) ,y))
(defsetf sixth (x) (y) `(progn (rplaca (nthcdr 5 ,x) ,y) ,y))
(defsetf seventh (x) (y) `(progn (rplaca (nthcdr 6 ,x) ,y) ,y))
(defsetf eighth (x) (y) `(progn (rplaca (nthcdr 7 ,x) ,y) ,y))
(defsetf ninth (x) (y) `(progn (rplaca (nthcdr 8 ,x) ,y) ,y))
(defsetf tenth (x) (y) `(progn (rplaca (nthcdr 9 ,x) ,y) ,y))
(defsetf rest (x) (y) `(progn (rplacd ,x ,y) ,y))
(defsetf svref si:svset)
(defsetf elt si:elt-set)
(defsetf symbol-value set)
(defsetf symbol-function si::fset)
(defsetf macro-function (s &optional env) (v) `(let ((env ,env)) (declare (ignorable env)) (si:fset ,s (cons 'macro ,v)) ,v))
;; (defun aset-wrap (x &rest r &aux v)
;;   (declare (:dynamic-extent r)) 
;;   (setq r (nreverse r) v (pop r) r (nreverse r)) 
;;   (apply 'si:aset v x r))
(defsetf aref (x &rest r) (v) `(si::aset ,v ,x ,@r))
;(defsetf aref aset-wrap)
(defsetf get put-aux)
(defmacro put-aux (a b &rest l)
  `(si::sputprop ,a ,b (progn ,@l)))
;  `(si::sputprop ,a ,b ,(car (last l))))
(defsetf nth (n l) (v) `(progn (rplaca (nthcdr ,n ,l) ,v) ,v))
(defsetf char si:char-set)
(defsetf schar si:schar-set)
;(defsetf bit aset-wrap)
;(defsetf sbit aset-wrap)
(defsetf bit (x &rest r) (v) `(si::aset ,v ,x ,@r))
(defsetf sbit (x &rest r) (v) `(si::aset ,v ,x ,@r))
;(defsetf fill-pointer si:fill-pointer-set)
(defsetf fill-pointer c-set-vector-fillp)
;(defsetf symbol-plist si:set-symbol-plist)
(defsetf symbol-plist (x) (y) `(c-set-symbol-plist ,x ,y))
(defsetf gethash (k h &optional d) (v) `(si:hash-set ,k ,h ,v))
(defsetf row-major-aref si::aset1)
(defsetf readtable-case si::set-readtable-case)

(defun set-documentation (s d v)
  (let ((x (typecase s
		      (function (function-name s))
		      (package (find-symbol (package-name s) :keyword))
		      ((cons (member setf) (cons symbol nil)) (setf-sym s))
		      (symbol s)))
	(p (ecase d
	       (variable 'variable-documentation)
	       (function 'function-documentation)
	       (structure 'structure-documentation)
	       (type 'type-documentation)
	       (setf 'setf-documentation)
	       (compiler-macro 'compiler-macro-documentation)
	       (method-combination 'method-combination-documentation)
	       ((t) 'package-documentation))))
    (if x (putprop x v p) v)))

(defsetf documentation (s d) (v)
  `(set-documentation ,s ,d ,v))

(define-setf-method getf (&environment env place indicator &optional default)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method place env)
    (let ((itemp (gensym))
	  (store (gensym))
	  (def-temp (if default (gensym))))
      (values `(,@vars ,itemp ,@(if default `(,def-temp)))
	      `(,@vals ,indicator ,@(if default `(,default)))
	      (list store)
	      `(let ((,(car stores) (si:put-f ,access-form ,store ,itemp)))
		 ,store-form
		 ,store)
	      `(getf ,access-form ,itemp ,@(if default `(,def-temp)))))))

(defsetf subseq (sequence1 start1 &optional end1)
                (sequence2)
  `(progn (replace ,sequence1 ,sequence2 :start1 ,start1 :end1 ,end1)
	  ,sequence2))

(define-setf-method the (&environment env type form)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method form env)
    (let ((store (gensym)))
      (values vars vals (list store)
	      `(let ((,(car stores) (the ,type ,store))) ,store-form)
	      `(the ,type ,access-form)))))

#|
(define-setf-method apply (&environment env fn &rest rest)
  (unless (and (consp fn) (eq (car fn) 'function) (symbolp (cadr fn))
	       (null (cddr fn)))
	  (error "Can't get the setf-method of ~S." fn))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method (cons (cadr fn) rest) env)
    (unless (eq (car (last store-form)) (car (last vars)))
            (error "Can't get the setf-method of ~S." fn))
    (values vars vals stores
	    `(apply #',(car store-form) ,@(cdr store-form))
	    `(apply #',(cadr fn) ,@(cdr access-form)))))
|#

(define-setf-method apply (&environment env fn &rest rest)
  (unless (and (consp fn)
               (or (eq (car fn) 'function) (eq (car fn) 'quote))
               (symbolp (cadr fn))
               (null (cddr fn)))
    (error "Can't get the setf-method of ~S." fn))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method (cons (cadr fn) rest) env)
    (cond ((eq (car (last store-form)) (car (last vars)))
           (values vars vals stores
                   `(apply #',(car store-form) ,@(cdr store-form))
                   `(apply #',(cadr fn) ,@(cdr access-form))))
          ((eq (car (last (butlast store-form))) (car (last vars)))
           (values vars vals stores
                   `(apply #',(car store-form)
                           ,@(cdr (butlast store-form 2))
                           (append ,(car (last (butlast store-form)))
                                   (list ,(car (last store-form)))))
                   `(apply #',(cadr fn) ,@(cdr access-form))))
          (t (error "Can't get the setf-method of ~S." fn)))))

(define-setf-method char-bit (&environment env char name)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method char env)
    (let ((ntemp (gensym))
	  (store (gensym))
	  (stemp (first stores)))
      (values `(,ntemp ,@temps)
	      `(,name ,@vals)
	      (list store)
	      `(let ((,stemp (set-char-bit ,access-form ,ntemp ,store)))
	         ,store-form ,store)
	      `(char-bit ,access-form ,ntemp)))))

(define-setf-method ldb (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int env)
    (let ((btemp (gensym))
	  (store (gensym))
	  (stemp (first stores)))
      (values `(,btemp ,@temps)
	      `(,bytespec ,@vals)
	      (list store)
	      `(let ((,stemp (dpb ,store ,btemp ,access-form)))
	         ,store-form ,store)
	      `(ldb ,btemp ,access-form)))))

(define-setf-method mask-field (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int env)
    (let ((btemp (gensym))
	  (store (gensym))
	  (stemp (first stores)))
      (values `(,btemp ,@temps)
	      `(,bytespec ,@vals)
	      (list store)
	      `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
	         ,store-form ,store)
	      `(mask-field ,btemp ,access-form)))))


;;; The expansion function for SETF.
(defun setf-expand-1 (place newvalue env &aux g)
  (when (and (consp place) (eq (car place) 'the))
        (return-from setf-expand-1
          (setf-expand-1 (caddr place) `(the ,(cadr place) ,newvalue) env)))
  (when (and (consp place) (eq (car place) 'values))
    (do ((vl (cdr place) (cdr vl))
	 (sym (gensym))
	 (forms nil)
	 (n 0 (1+ n)))
	((endp vl) (return-from setf-expand-1 
				`(let ((,sym (multiple-value-list ,newvalue))) 
				   (values ,@(nreverse forms)))))
	 (declare (fixnum n) (object vl))
	 (let ((method (if (symbolp (car vl)) 'setq 'setf)))
	   (push `(,method ,(car vl) (nth ,n ,sym)) forms))))
  (when (symbolp place)
        (return-from setf-expand-1 `(setq ,place ,newvalue)))
  (when (and (consp place)
	       (not (or (get (car place) 'setf-lambda)
			(get (car place) 'setf-update-fn))))
	  (multiple-value-setq (place g) (macroexpand place env))
	  (if g (return-from setf-expand-1 (setf-expand-1 place newvalue env))))
  (when (and (symbolp (car place)) (setq g (get (car place) 'setf-update-fn)))
    (return-from setf-expand-1 `(,g ,@(cdr place) ,newvalue)))
  (cond ((and (symbolp (car place))
	      (setq g (get (car place) 'structure-access)))
	 (return-from setf-expand-1
	   (setf-structure-access (cadr place) (car g) (cdr g) newvalue))))
	     
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method place env)
    (declare (ignore access-form))
    `(let* ,(join (append vars stores) (append vals (list newvalue)))
       (declare (ignorable ,@vars))
       ,store-form)))

(defun setf-structure-access (struct type index newvalue)
  (case type
    (list `(setf (nth ,index ,struct) ,newvalue))
;    (list `(si:rplaca-nthcdr ,struct ,index ,newvalue))
    (vector `(si:elt-set ,struct ,index ,newvalue))
    (t `(si::structure-set ,struct ',type ,index ,newvalue))))

(defun setf-expand (l env)
  (cond ((endp l) nil)
        ((endp (cdr l)) (error "~S is an illegal SETF form." l))
        (t
         (cons (setf-expand-1 (car l) (cadr l) env)
               (setf-expand (cddr l) env)))))


;;; SETF macro.

;; (defun setf-helper (rest env)
;;   (setq rest (cdr rest))
;;   (cond ((endp rest) nil)
;; ;        ((endp (cdr rest)) (error "~S is an illegal SETF form." rest))
;;         ((endp (cddr rest)) (setf-expand-1 (car rest) (cadr rest) env))
;;         (t (cons 'progn (setf-expand rest env)))))

;; ;(setf (macro-function 'setf) 'setf-help)
;; (si::fset 'setf (cons 'macro (symbol-function 'setf-helper)))

(defmacro setf (&environment env &rest rest)
  (cond ((endp rest) nil)
;        ((endp (cdr rest)) (error "~S is an illegal SETF form." rest))
        ((endp (cddr rest)) (setf-expand-1 (car rest) (cadr rest) env))
        ((cons 'progn (setf-expand rest env)))))

;;; PSETF macro.

(defmacro psetf (&environment env &rest rest)
  (declare (optimize (safety 2)))
  (cond ((endp rest) nil)
        ((endp (cdr rest)) (error "~S is an illegal PSETF form." rest))
        ((endp (cddr rest))
         `(progn ,(setf-expand-1 (car rest) (cadr rest) env)
                 nil))
        (t
	 (do ((r rest (cddr r))
	      (pairs nil)
	      (store-forms nil))
	     ((endp r)
	      `(let* ,pairs
		 ,@(nreverse store-forms);FIXME put in ignorable decl here
		 nil))
	   (when (endp (cdr r)) (error "~S is an illegal PSETF form." rest))
	   (multiple-value-bind (vars vals stores store-form access-form)
	       (get-setf-method (car r) env)
             (declare (ignore access-form))
	     (setq store-forms (cons store-form store-forms))
	     (setq pairs
		   (nconc pairs
			  (join (append vars stores) (append vals (list (cadr r)))))))))))


;;; SHIFTF macro.
(defmacro shiftf (&environment env &rest rest )
  (declare (optimize (safety 2)))
  (do ((r rest (cdr r))
       (pairs nil)
       (stores nil)
       (store-forms nil)
       (g (gensym))
       (access-forms nil))
      ((endp (cdr r))
       (setq stores (nreverse stores))
       (setq store-forms (nreverse store-forms))
       (setq access-forms (nreverse access-forms))
       `(let* ,(nconc pairs
		      (list (list g (car access-forms)))
		      (join stores (cdr access-forms))
		      (list (list (car (last stores)) (car r))))
	    ,@store-forms
	    ,g))
    (multiple-value-bind (vars vals stores1 store-form access-form)
	(get-setf-method (car r) env)
      (setq pairs (nconc pairs (join vars vals)))
      (setq stores (cons (car stores1) stores))
      (setq store-forms (cons store-form store-forms))
      (setq access-forms (cons access-form access-forms)))))


;;; ROTATEF macro.
(defmacro rotatef (&environment env &rest rest )
  (declare (optimize (safety 2)))
  (do ((r rest (cdr r))
       (pairs nil)
       (stores nil)
       (store-forms nil)
       (access-forms nil))
      ((endp r)
       (setq stores (nreverse stores))
       (setq store-forms (nreverse store-forms))
       (setq access-forms (nreverse access-forms))
       (when store-forms
	 `(let* ,(nconc pairs
		      (join stores (cdr access-forms))
		      (list (list (car (last stores)) (car access-forms))))
	    ,@store-forms
	    nil
	    )))
    (multiple-value-bind (vars vals stores1 store-form access-form)
	(get-setf-method (car r) env)
      (setq pairs (nconc pairs (join vars vals)))
      (setq stores (cons (car stores1) stores))
      (setq store-forms (cons store-form store-forms))
      (setq access-forms (cons access-form access-forms)))))


(defun make-update-form (function access-form others lets)
  (if others
      (make-update-form function access-form (cdr others)
			(cons (list 'list (list 'quote (gensym)) (car others)) lets))
    `(list 'let* (list ,@lets)
	   (list ',function access-form ,@(to-quote-cadadr lets)))))

;;; DEFINE-MODIFY-MACRO macro.
;;FIXME -- this is really ugly and error prone.  CM 20041214
(defmacro define-modify-macro (name lambda-list function &optional doc-string)
  (declare (optimize (safety 2)))
  (let ((update-form
	 (do ((l lambda-list (cdr l))
	      (vs nil))
	     ((null l) (make-update-form function 'access-form (nreverse vs) nil))
	   (unless (eq (car l) '&optional)
		   (if (eq (car l) '&rest)
		       (return `(list* ',function
				       access-form
				       ,@(nreverse vs)
				       ,(cadr l))))
		   (if (symbolp (car l))
		       (setq vs (cons (car l) vs))
		       (setq vs (cons (caar l) vs)))))))
    `(defmacro ,name (&environment env reference . ,lambda-list)
       ,@(if doc-string (list doc-string))
       (when (symbolp reference)
             (return-from ,name
               (let ((access-form reference))
                 (list 'setq reference ,update-form))))
       (multiple-value-bind (vars vals stores store-form access-form)
	   (get-setf-method reference env)
         (list 'let*
	       (join (append vars stores) (append vals (list ,update-form)))
	       (list 'declare (cons 'ignorable vars))
	       store-form)))))


;;; Some macro definitions.

;;; (defmacro remf (&environment env place indicator)
;;;  (multiple-value-bind (vars vals stores store-form access-form)
;;;      (get-setf-method place env)
;;;    `(let* ,(mapcar #'list vars vals)
;;;       (multiple-value-bind (,(car stores) flag)
;;;           (si:rem-f ,access-form ,indicator)
;;;         ,store-form
;;;         flag))))

;;; This definition was obtained from SBCL
(defmacro remf (&environment env place indicator)
  (declare (optimize (safety 2)))
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (do* ((d dummies (cdr d))
          (v vals (cdr v))
          (let-list nil)
          (ind-temp (gensym))
          (local1 (gensym))
          (local2 (gensym)))
         ((null d)
          ;; See ANSI 5.1.3 for why we do out-of-order evaluation
          (push (list ind-temp indicator) let-list)
          (push (list (car newval) getter) let-list)
          `(let* ,(nreverse let-list)
             (do ((,local1 ,(car newval) (cddr ,local1))
                  (,local2 nil ,local1))
                 ((atom ,local1) nil)
               (cond ((atom (cdr ,local1))
                      (error "Odd-length property list in REMF."))
                     ((eq (car ,local1) ,ind-temp)
                      (cond (,local2
                             (rplacd (cdr ,local2) (cddr ,local1))
                             (return t))
                            (t (setq ,(car newval) (cddr ,(car newval)))
                               ,setter
                               (return t))))))))
      (push (list (car d) (car v)) let-list))))

(define-modify-macro incf (&optional (delta 1)) +)
(define-modify-macro decf (&optional (delta 1)) -)

(defmacro push (&environment env item place)
  (declare (optimize (safety 2)))
  (let ((myitem (gensym)))
    (when (symbolp place)
      (return-from push `(let* ((,myitem ,item))
			   (setq ,place (cons ,myitem ,place)))))
    (multiple-value-bind (vars vals stores store-form access-form)
			 (get-setf-method place env)
			 `(let* ,(join (append (list myitem) vars stores) (append (list   item) vals (list (list 'cons myitem access-form))))
			    (declare (ignorable ,@vars))
			    ,store-form))))

(defmacro pushnew (&environment env item place &rest rest)
  (declare (optimize (safety 2)))
  (let ((myitem (gensym)))
    (cond ((symbolp place)
	   (return-from pushnew `(let* ((,myitem ,item))
				   (setq ,place (adjoin ,myitem ,place ,@rest))))))
    (multiple-value-bind (vars vals stores store-form access-form)
			 (get-setf-method place env)
			 `(let* ,(join (append (list myitem) vars stores) 
				       (append (list item) vals  (list (list* 'adjoin myitem access-form rest))))
			    (declare (ignorable ,@vars))
			    ,store-form))))

(defmacro pop (&environment env place)
  (declare (optimize (safety 2)))
  (when (symbolp place)
        (return-from pop
          (let ((temp (gensym)))
            `(let ((,temp (car ,place)))
                (setq ,place (cdr ,place))
                ,temp))))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method place env)
    `(let* ,(join (append vars stores) (append vals (list (list 'cdr access-form))))
       (declare (ignorable ,@vars))
       (prog1 (car ,access-form)
              ,store-form))))

(defun fdefinition (n)
  (declare (optimize (safety 2)))
  (let ((n (funid-sym n)))
    (if (fboundp n)
	(symbol-function n)
      (error 'undefined-function :name n))))

(defun (setf fdefinition) (def n)
  (declare (optimize (safety 2)))
  (check-type def function)
  (let ((n (funid-sym n)))
    (assert (not (special-operator-p n)))
    (setf (symbol-function n) def)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_setf.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_defstruct.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;    DEFSTRUCT.LSP
;;;;
;;;;        The structure routines.


;; (in-package 'lisp)
;; (export 'defstruct)


(in-package :system)


(defvar *accessors* (make-array 10 :adjustable t))
(defvar *list-accessors* (make-array 2 :adjustable t))
(defvar *vector-accessors* (make-array 2 :adjustable t))

(defun record-fn (&rest l) (declare (ignore l)) nil)

(defun make-access-function (name conc-name no-conc type named include no-fun
				  ;; from apply
				  slot-name default-init slot-type read-only
				  offset &optional predicate) 
  (declare (ignore named default-init predicate no-fun))
  (let ((access-function (if no-conc slot-name
			   (intern (si:string-concatenate conc-name slot-name)))))
    (record-fn access-function 'defun '(t) slot-type)
    (cond (read-only
	   (remprop access-function 'structure-access)
	   (setf (get access-function 'struct-read-only) t))
	  (t (remprop access-function 'setf-update-fn)
	     (remprop access-function 'setf-lambda)
	     (remprop access-function 'setf-documentation)
	     (let ((tem (get access-function 'structure-access)))
	       (unless (and (consp tem) include
			    (subtypep include (car tem))
			    (eql (cdr tem) offset))
		 (setf (get access-function 'structure-access) (cons (if type type name) offset)))))))
  nil)

(defmacro key-name (key prior-keyword)
  `(cond
   ((not (consp ,key))
    ,key)
   (t 
    (unless (endp (cdddr ,key))
      (error "Bad key ~S~%" ,key))
    (cond 
     ((not (consp (car ,key)))
      (car ,key))
     ((and (eq ,prior-keyword '&key) (not (consp (caar ,key))))
      (unless (endp (cddar ,key))
	(error "Bad key ~S~%" ,key))
      (cadar ,key))
     (t
      (error "Bad key ~S~%" ,key))))))

(defmacro maybe-add-keydef (key keydefs prior-keyword)
  `(let ((def (cadar 
	       (member (key-name ,key ,prior-keyword) ,keydefs
		       :key (lambda (k)
			      (declare (optimize (safety 2)))
			      (when (consp k) (car k)))))))
     (if def
	 (cond ((not (consp ,key))
		(list ,key def))
	       (t
		(if (cdr ,key) ,key (list (car ,key) def))))
       ,key)))

(defun parse-boa-lambda-list (lambda-list keydefs)
  (let ((keywords '(none &optional &rest &key &allow-other-keys &aux))
	vs res tk restvar seen-keys)
    (do ((ll lambda-list (cdr ll))) ((endp ll))
      (let ((key (car ll)))
	(cond ((setq tk (member key keywords))
	       (setq keywords tk)
	       (push key res)
	       (push key seen-keys))
	      ((member key lambda-list-keywords)
	       (error "Keyword ~S appeared in a bad place in BOA lambda list" key))
	      (t
	       (let ((prior-keyword (car keywords)))
		 (case prior-keyword
		   ((none &rest)
		    (unless (symbolp key)
		      (error "non-symbol appeared in bad place in BOA lambda list" key))
		    (push key res)
		    (push key vs)
		    (when (eq prior-keyword '&rest)
		      (when restvar
			(error "Multiple variables after &rest in BOA lambda list"))
		      (setq restvar t)))
		   ((&optional &key)
		    (push (maybe-add-keydef key keydefs prior-keyword) res)
		    (push (key-name key prior-keyword) vs))
		   (&allow-other-keys
		    (error "Variable ~S appeared after &allow-other-keys in BOA list" key))
		   (&aux
		    (push key res)
		    (push (key-name key prior-keyword) vs))))))))
    (when (and (member '&rest seen-keys) (not restvar))
      (error "Missing &rest variable in BOA list"))
    (unless (member '&aux seen-keys)
      (push '&aux res))
    (do ((ll keydefs (cdr ll))) ((endp ll))
      (let* ((keydef (car ll))
	     (keydef-name (if (atom keydef) keydef (car keydef))))
	(unless (member keydef-name vs)
	  (push keydef res))))
    (nreverse res)))

(defun maybe-cons-keyname (x &optional y)
  (unless (consp x)
    (error 'program-error :format-control "x ~S is not a list~%" :format-arguments (list x)))
  (let ((sn (sixth x)))
    (if sn
	(if y
	    (list (list (car x) sn) y)
	  (list (list (car x) sn)))
      (if y (list (car x) y) (car x)))))

(defun make-constructor (name constructor type named
                         slot-descriptions)
  (declare (ignore named))
  (let ((slot-names
         ;; Collect the slot-names.
         (mapcar (lambda (x)
                     (cond ((null x)
                            ;; If the slot-description is NIL,
                            ;;  it is in the padding of initial-offset.
                            nil)
                           ((null (car x))
                            ;; If the slot name is NIL,
                            ;;  it is the structure name.
                            ;;  This is for typed structures with names.
                            (list 'quote (cadr x)))
                           (t (let ((sn (sixth x))) (if sn sn (car x))))))
                 slot-descriptions))
        (keys
         ;; Make the keyword parameters.
         (mapcan (lambda (x)
                     (cond ((null x) nil)
                           ((null (car x)) nil)
                           ((null (cadr x)) (list (maybe-cons-keyname x)))
                           (t (list (maybe-cons-keyname x (cadr x))))))
                 slot-descriptions)))
    (cond ((consp constructor)
	   (setq keys (parse-boa-lambda-list (cadr constructor) keys))
           (setq constructor (car constructor)))
          (t
           ;; If not a BOA constructor, just cons &KEY.
           (setq keys (cons '&key keys))))
     (cond ((null type)
	    `(defun ,constructor ,keys
	       (the ,name (si:make-structure ',name ,@slot-names))))
	   ((eq type 'vector)
	    `(defun ,constructor ,keys
	       (vector ,@slot-names)))
	   ((and (consp type) (eq (car type) 'vector))
	    (if (endp (cdr type))
		`(defun ,constructor ,keys
		   (vector ,@slot-names)))
	      `(defun ,constructor ,keys
		 (make-array ,(length slot-names)
			     :element-type ',(cadr type)
			     :initial-contents (list ,@slot-names))))
	   ((eq type 'list)
	    `(defun ,constructor ,keys
	       (list ,@slot-names)))
	   ((error "~S is an illegal structure type" type)))))
  

;;; PARSE-SLOT-DESCRIPTION parses the given slot-description
;;;  and returns a list of the form:
;;;        (slot-name default-init slot-type read-only offset)

(defun parse-slot-description (slot-description offset)
  (let (slot-name default-init slot-type read-only)
    (cond ((atom slot-description)
           (setq slot-name slot-description))
          ((endp (cdr slot-description))
           (setq slot-name (car slot-description)))
          (t
           (setq slot-name (car slot-description))
           (setq default-init (cadr slot-description))
           (do ((os (cddr slot-description) (cddr os)) (o) (v))
               ((endp os))
             (setq o (car os))
             (when (endp (cdr os))
                   (error "~S is an illegal structure slot option."
                          os))
             (setq v (cadr os))
             (case o
               (:type (setq slot-type v))
               (:read-only (setq read-only v))
               (t
                (error "~S is an illegal structure slot option."
                         os))))))
    (list slot-name default-init slot-type read-only offset)))


;;; OVERWRITE-SLOT-DESCRIPTIONS overwrites the old slot-descriptions
;;;  with the new descriptions which are specified in the
;;;  :include defstruct option.

(defun overwrite-slot-descriptions (news olds)
  (if (null olds)
      nil
      (let ((sds (member (caar olds) news :key #'car)))
        (cond (sds
               (when (and (null (cadddr (car sds)))
                          (cadddr (car olds)))
                     ;; If read-only is true in the old
                     ;;  and false in the new, signal an error.
                     (error "~S is an illegal include slot-description."
                            sds))
	       ;; If
	       (setf (caddr (car sds))
		     (upgraded-array-element-type (caddr (car sds))))
	       (when (not  (equal (normalize-type (or (caddr (car sds)) t))
				 (normalize-type (or (caddr (car olds)) t))))
		     (error "Type mismmatch for included slot ~a" (car sds)))
		     (cons (list (caar sds)
                           (cadar sds)
                           (caddar sds)
                           (cadddr (car sds))
                           ;; The offset if from the old.
                           (car (cddddr (car olds))))
                     (overwrite-slot-descriptions news (cdr olds))))
              (t
               (cons (car olds)
                     (overwrite-slot-descriptions news (cdr olds))))))))

(defconstant +aet-type-object+ (aet-type nil))
(defconstant +all-t-s-type+ 
  (make-array 50 :element-type 'unsigned-char :static t :initial-element +aet-type-object+))
(defconstant +alignment-t+ (alignment t))

(defun make-t-type (n include slot-descriptions &aux i)
  (let ((res  (make-array n :element-type 'unsigned-char :static t)))
    (when include
	  (let ((tem (get include 's-data)) raw)
	    (or tem (error "Included structure undefined ~a" include))
	    (setq raw (s-data-raw tem))
	  (dotimes (i (min n (length raw)))
		   (setf (aref res i) (aref raw i)))))
    (dolist (v slot-descriptions)
	    (setq i (nth 4 v))
	    (let ((type (third v)))
	      (cond ((<= (the fixnum (alignment type)) +alignment-t+)
		     (setf (aref res i) (aet-type type))))))
    (cond ((< n (length +all-t-s-type+))
	   (let ((def +aet-type-object+))
	     (dotimes (i n)
	       (cond ((not (= (the fixnum (aref res i)) def))
		      (return-from make-t-type res)))))
	   +all-t-s-type+)
	  (t res))))

(defvar *standard-slot-positions*
  (let ((ar (make-array 50 :element-type 'unsigned-short :static t))) 
    (dotimes (i 50)
	     (declare (fixnum i))
	     (setf (aref ar i)(*  (size-of t) i)))
    ar))

(defun round-up (a b)
  (declare (fixnum a b))
  (setq a (ceiling a b))
  (the fixnum (* a b)))


(defun get-slot-pos (leng include slot-descriptions &aux type small-types
			  has-holes) 
  (declare (ignore include) (special *standard-slot-positions*)) 
  (dolist (v slot-descriptions)
	  (when (and v (car v))
		(setf type 
		      (upgraded-array-element-type (or (caddr v) t))
		      (caddr v) type)
		(let ((val (second v)))
		  (unless (typep val type)
			  (if (and (symbolp val)
				   (constantp val))
			      (setf val (symbol-value val)))
			  (and (constantp val)
			       (setf (cadr v) (coerce val type)))))
		(cond ((member type '(signed-char unsigned-char
						short unsigned-short
					 long-float
					 bit))
		       (setq small-types t)))))
  (cond ((and (null small-types)
	      (< leng (length *standard-slot-positions*))
	      (list  *standard-slot-positions* (* leng  (size-of t)) nil)))
	(t (let ((ar (make-array leng :element-type 'unsigned-short
				 :static t))
		 (pos 0)(i 0)(align 0)type (next-pos 0))
	     (declare (fixnum pos i align next-pos))
	     ;; A default array.
		   
	     (dolist
	       (v slot-descriptions)
	       (setq type (caddr v))
	       (setq align (alignment type))
	       (unless (<= align +alignment-t+)
		       (setq type t)
		       (setf (caddr v) t)
		       (setq align +alignment-t+)
		       (setq v (nconc v '(t))))
	       (setq next-pos (round-up pos align))	
	       (or (eql pos next-pos) (setq has-holes t))
	       (setq pos next-pos)
	       (setf (aref ar i) pos)
	       (incf pos (size-of type))
	       (incf i))
	     (list ar (round-up pos (size-of t)) has-holes)
	     ))))

;FIXME function-src for all functions, sigs for constructor and copier
(defun define-structure (name conc-name no-conc type named slot-descriptions copier
			      static include print-function constructors
			      offset predicate &optional documentation no-funs
			      &aux def leng)
  (declare (ignore copier))
  (and (consp type) (eq (car type) 'vector)(setq type 'vector))
  (setq leng (length slot-descriptions))
  (dolist (x slot-descriptions)
    (and x (car x)
	 (apply 'make-access-function
		name conc-name no-conc type named include no-funs x)))

  (cond ((and (null type)
	      (eq name 's-data))
	 ;bootstrapping code!
	 (setq def (make-s-data-structure
		     (make-array (* leng (size-of t))
				 :element-type 'unsigned-char :static t :initial-element +aet-type-object+)
		     (make-t-type leng nil slot-descriptions)
		     *standard-slot-positions*
		     slot-descriptions
		     t
		     ))
	 )
	(t
	  (let (slot-position
		 (size 0) has-holes
		 (include-str (and include
				   (get include 's-data))))
	    (when include-str
		  (cond ((and (s-data-frozen include-str)
			      (or (not (s-data-included include-str))
				  (not (let ((te (get name 's-data)))
					 (and te
					      (eq (s-data-includes 
						    te)
						  include-str))))))
			 (warn " ~a was frozen but now included"
			       include)))
		  (pushnew name (s-data-included include-str)))
	    (when (null type)
		 (setf slot-position
		       (get-slot-pos leng include slot-descriptions))
		 (setf size (cadr slot-position)
		       has-holes (caddr slot-position)
		       slot-position (car slot-position)
		       ))
	  (setf def (make-s-data
		       :name name
		       :length leng
		       :raw
		       (and (null type)
			    (make-t-type leng include slot-descriptions))
		       :slot-position slot-position
		       :size size
		       :has-holes has-holes
		       :staticp static
		       :includes include-str
		       :print-function print-function
		       :slot-descriptions slot-descriptions
		       :constructors constructors
		       :offset offset
		       :type type
		       :named named
		       :documentation documentation
		       :conc-name conc-name)))))
  (let ((tem (get name 's-data)))
    (cond ((eq name 's-data)
	   (if tem (warn "not replacing s-data property"))
	   (or tem (setf (get name 's-data) def)))
	  (tem 
	   (check-s-data tem def name))
	  (t (setf (get name 's-data) def)));(null type)
    (when documentation
	  (setf (get name 'structure-documentation) documentation))
    (when (and (null type) predicate)
	  (record-fn predicate 'defun '(t) t)
	  (setf (get predicate 'compiler::co1)'compiler::co1structure-predicate)
	  (setf (get predicate 'struct-predicate) name)))
  nil)

		  
(defun str-ref (x y z) (declare (ignore y)) (structure-ref1 x z))
(export 'str-ref)

(defmacro defstruct (name &rest slots)
  (declare (optimize (safety 2)))
  (let ((slot-descriptions slots)
        options
        conc-name
        constructors default-constructor no-constructor
        copier
        predicate predicate-specified
        include
        print-function print-object  type named initial-offset
        offset name-offset
        documentation
	static
	(no-conc nil))

    (when (consp name)
	  ;; The defstruct options are supplied.
          (setq options (cdr name))
          (setq name (car name)))
    
    ;; The default conc-name.
    (setq conc-name (si:string-concatenate (string name) "-"))

    ;; The default constructor.
    (setq default-constructor
          (intern (si:string-concatenate "MAKE-" (string name))))

    ;; The default copier and predicate.
    (setq copier
          (intern (si:string-concatenate "COPY-" (string name)))
          predicate
          (intern (si:string-concatenate (string name) "-P")))

    ;; Parse the defstruct options.
    (do ((os options (cdr os)) (o) (v))
        ((endp os))
	(cond ((and (consp (car os)) (not (endp (cdar os))))
	       (setq o (caar os) v (cadar os))
	       (case o
		 (:conc-name
		   (if (null v) 
		       (progn
			 (setq conc-name "")
			 (setq no-conc t))
		     (setq conc-name v)))
		 (:constructor
		   (if (null v)
		       (setq no-constructor t)
		     (if (endp (cddar os))
			 (setq constructors (cons v constructors))
		       (setq constructors (cons (cdar os) constructors)))))
		 (:copier (setq copier v))
		 (:static (setq static v))
		 (:predicate
		   (setq predicate (or v (gensym)))
		   (setq predicate-specified t))
		 (:include
		   (setq include (cdar os))
		   (unless (get v 's-data)
			   (error "~S is an illegal included structure." v)))
		 (:print-object
		  (and (consp v) (eq (car v) 'function)
		       (setq v (second v)))
		  (setq print-object v))
		 (:print-function
		  (and (consp v) (eq (car v) 'function)
		       (setq v (second v)))
		  (setq print-function v))
		 (:type (setq type v))
		 (:initial-offset (setq initial-offset v))
		 (t (error "~S is an illegal defstruct option." o))))
	      (t
		(if (consp (car os))
		    (setq o (caar os))
		  (setq o (car os)))
		(case o
		  (:constructor
		    (setq constructors
			  (cons default-constructor constructors)))
		  ((:copier :predicate :print-function))
		  (:conc-name
		   (progn
		     (setq conc-name "")
		     (setq no-conc t)))
		  (:named (setq named t))
		  (t (error "~S is an illegal defstruct option." o))))))

    (setq conc-name (intern (string conc-name)))

    (when (and print-function print-object)
      (error "Cannot specify both :print-function and :print-object."))
    (when print-object
      (setq print-function (lambda (x y z) 
			     (declare (optimize (safety 2)))
			     (declare (ignore z)) (funcall print-object x y))))

    (and include (not print-function)
	 (setq print-function (s-data-print-function (get (car include)  's-data))))

    ;; Skip the documentation string.
    (when (and (not (endp slot-descriptions))
               (stringp (car slot-descriptions)))
          (setq documentation (car slot-descriptions))
          (setq slot-descriptions (cdr slot-descriptions)))
    
    ;; Check the include option.
    (when include
          (unless (equal type
			 (s-data-type (get  (car include) 's-data)))
                  (error "~S is an illegal structure include."
                         (car include))))

    ;; Set OFFSET.
    (cond ((null include)
           (setq offset 0))
          (t 
	    (setq offset (s-data-offset (get (car include) 's-data)))))

    ;; Increment OFFSET.
    (when (and type initial-offset)
          (setq offset (+ offset initial-offset)))
    (when (and type named)
          (setq name-offset offset)
          (setq offset (1+ offset)))

    ;; Parse slot-descriptions, incrementing OFFSET for each one.
    (do ((ds slot-descriptions (cdr ds))
         (sds nil))
        ((endp ds)
         (setq slot-descriptions (nreverse sds)))
	(setq sds (cons (parse-slot-description (car ds) offset) sds))
	(setq offset (1+ offset)))

    ;; If TYPE is non-NIL and structure is named,
    ;;  add the slot for the structure-name to the slot-descriptions.
    (when (and type named)
          (setq slot-descriptions
                (cons (list nil name) slot-descriptions)))

    ;; Pad the slot-descriptions with the initial-offset number of NILs.
    (when (and type initial-offset)
          (setq slot-descriptions
                (append (make-list initial-offset) slot-descriptions)))

    ;; Append the slot-descriptions of the included structure.
    ;; The slot-descriptions in the include option are also counted.
    (cond ((null include))
          ((endp (cdr include))
           (setq slot-descriptions
                 (append (s-data-slot-descriptions
			   (get (car include) 's-data))
                         slot-descriptions)))
          (t
	    (setq slot-descriptions
		  (append (overwrite-slot-descriptions
			    (mapcar (lambda (sd)
					(parse-slot-description sd 0))
				    (cdr include))
			    (s-data-slot-descriptions
			      (get (car include) 's-data)
                              ))
			  slot-descriptions))))

    (cond (no-constructor
	    ;; If a constructor option is NIL,
	    ;;  no constructor should have been specified.
	    (when constructors
		  (error "Contradictory constructor options.")))
          ((null constructors)
	   ;; If no constructor is specified,
	   ;;  the default-constructor is made.
           (setq constructors (list default-constructor))))

    ;; We need a default constructor for the sharp-s-reader
    (or (member t (mapcar 'symbolp  constructors))
	(push (intern (string-concatenate "__si::" default-constructor))
		      constructors))

    ;; Check the named option and set the predicate.
    (when (and type (not named))
          (when predicate-specified
                (error "~S is an illegal structure predicate."
                       predicate))
          (setq predicate nil))

    (when include (setq include (car include)))

    ;; Check the print-function.
    (when (and print-function type)
          (error "A print function is supplied to a typed structure."))

    (let* ((tp (cond ((not type) nil) 
		     ((subtypep type 'list) 'list)
		     ((subtypep type 'vector) 'vector)))
	   (ctp (cond ((or (not type) named) name) (tp)))
	   new-slot-descriptions
	   (new-slot-descriptions ;(copy-list slot-descriptions)))
	    (dolist (sd slot-descriptions (nreverse new-slot-descriptions))
	      (if (and (consp sd) (eql (length sd) 5))
		(let* ((csd (car sd))
		       (sym (when (or (constantp csd) (keywordp csd) (si::specialp csd)) 
			      (make-symbol (symbol-name csd))))
		       (nsd (if (or (constantp csd) (si::specialp csd))
				(cons (intern (symbol-name csd) 'keyword) (cdr sd))
			      sd)))
		  (push (append nsd (list sym)) new-slot-descriptions)
		  (when sym
		    (setf (car sd) sym)))
		(push sd new-slot-descriptions)))))
      `(progn
	 (define-structure ',name  ',conc-name ',no-conc ',type
	   ',named ',slot-descriptions ',copier ',static ',include 
	   ',print-function ',constructors 
	   ',offset ',predicate ',documentation)
	 ,@(mapcar (lambda (constructor)
		       (make-constructor name constructor type named new-slot-descriptions))
		   constructors)
	 ,@(when copier
	     `((defun ,copier (x) 
		 (declare (optimize (safety 1)))
		 (check-type x ,ctp)
		 (the ,ctp 
		      ,(ecase tp
			      ((nil) `(copy-structure x))
			      (list `(copy-list x))
			      (vector `(copy-seq x)))))))
	 ,@(mapcar (lambda (y) 
		     (let* ((sn (pop y))
			   (nm (if no-conc sn
				 (intern (si:string-concatenate (string conc-name) (string sn)))))
			   (di (pop y))
			   (st (pop y))
			   (ro (pop y))
			   (offset (pop y)))
		       `(defun ,nm (x)
			   (declare (optimize (safety 2)))
			   (check-type x ,ctp)
			   (the ,(or (not st) st)
				,(ecase tp
					((nil) `(str-ref x ',name ,offset))
					(list `(let ((c (nthcdr ,offset x))) (check-type c cons) (car c)));(list-nth ,offset x))
					(vector `(aref x ,offset)))))))
		   slot-descriptions)
	 ,@(mapcar (lambda (y) 
		     (let* ((sn (car y))
			    (y (if no-conc sn
				 (intern (si:string-concatenate (string conc-name) (string sn))))))
		       `(si::putprop ',y t 'compiler::cmp-inline))) slot-descriptions);FIXME
	 ,@(when predicate
	     `((defun ,predicate (x) 
		 (declare (optimize (safety 2)))
		 (the boolean 
		      ,(ecase tp
			      ((nil) `(structure-subtype-p x ',name))
			      (list
			       (unless named (error "The structure should be named."))
			       `(let ((x (when (listp x) (nthcdr ,name-offset x)))) (when x (eq (car x) ',name))))
			      (vector
			       (unless named (error "The structure should be named."))
			       `(and (typep x '(vector t))
				     (> (length x) ,name-offset)
				     (eq (aref x ,name-offset) ',name))))))))
	 ,@(when predicate ;(and predicate named);predicate;
	     `((deftype ,name nil (list 'satisfies ',predicate))))
	 ',name))))

;; First several fields of this must coincide with the C structure
;; s_data (see object.h).


(defstruct s-data (name nil :type symbol)
		 (length 0 :type fixnum)
		 raw
		 included
		 includes
		 staticp
		 print-function
		 slot-descriptions
		 slot-position 
		 (size 0 :type fixnum)
		 has-holes
		 frozen
		 documentation
		 constructors
		 offset
		 named
		 type
		 conc-name
		 )


(defun check-s-data (tem def name)
  (cond ((s-data-included tem)
	 (setf (s-data-included def)(s-data-included tem))))
  (cond ((s-data-frozen tem)
	 (setf (s-data-frozen def) t)))
  (unless (equalp def tem)
	  (warn "structure ~a is changing" name)
	  (setf (get name 's-data) def)))
(defun freeze-defstruct (name)
  (let ((tem (and (symbolp name) (get name 's-data))))
    (if tem (setf (s-data-frozen tem) t))))


;;; The #S reader.

(defun sharp-s-reader (stream subchar arg)
  (declare (ignore subchar))
  (when (and arg (null *read-suppress*))
        (error "An extra argument was supplied for the #S readmacro."))
  (let* ((l (prog1 (read stream t nil t)
	      (if *read-suppress*
		  (return-from sharp-s-reader nil))))
	 (sd
	   (or (get (car l) 's-data)
	       
	       (error "~S is not a structure." (car l)))))
    
    ;; Intern keywords in the keyword package.
    (do ((ll (cdr l) (cddr ll)))
        ((endp ll)
         ;; Find an appropriate construtor.
         (do ((cs (s-data-constructors sd) (cdr cs)))
             ((endp cs)
              (error "The structure ~S has no structure constructor."
                     (car l)))
           (when (symbolp (car cs))
                 (return (apply (car cs) (cdr l))))))
      (rplaca ll (intern (string (car ll)) 'keyword)))))


;; Set the dispatch macro.
(set-dispatch-macro-character #\# #\s 'sharp-s-reader)
(set-dispatch-macro-character #\# #\S 'sharp-s-reader)

;; Examples from Common Lisp Reference Manual.

#|
(defstruct ship
  x-position
  y-position
  x-velocity
  y-velocity
  mass)

(defstruct person name (age 20 :type signed-char) (eyes 2 :type signed-char)
							sex)
(defstruct person name (age 20 :type signed-char) (eyes 2 :type signed-char)
							sex)
(defstruct person1 name (age 20 :type fixnum)
							sex)

(defstruct joe a (a1 0 :type (mod  30)) (a2 0 :type (mod  30))
  (a3 0 :type (mod  30)) (a4 0 :type (mod 30)) )

;(defstruct person name age sex)

(defstruct (astronaut (:include person (age 45 :type fixnum))
                      (:conc-name astro-))
  helmet-size
  (favorite-beverage 'tang))

(defstruct (foo (:constructor create-foo (a
                                          &optional b (c 'sea)
                                          &rest d
                                          &aux e (f 'eff))))
  a (b 'bee) c d e f)

(defstruct (binop (:type list) :named (:initial-offset 2))
  (operator '?)
  operand-1
  operand-2)

(defstruct (annotated-binop (:type list)
                            (:initial-offset 3)
                            (:include binop))
  commutative
  associative
  identity)

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_defstruct.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_seqlib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;   seqlib.lsp
;;;;
;;;;                           sequence routines


;; (in-package 'lisp)


;; (export '(copy-seq reduce fill replace length elt every some notevery notany
;;           remove remove-if remove-if-not
;;           delete delete-if delete-if-not
;;           count count-if count-if-not
;;           substitute substitute-if substitute-if-not
;;           nsubstitute nsubstitute-if nsubstitute-if-not
;;           find find-if find-if-not
;;           position position-if position-if-not
;;           remove-duplicates delete-duplicates
;;           mismatch search
;; 	  with-hash-table-iterator
;;           sort stable-sort merge))


(in-package :system)


(eval-when
 (compile eval)

 (defmacro comp-key (key) 
   `(if (eq ,key #'identity) 0 1))
 
 (defmacro do-key (key n x) 
   (let ((xx (sgen)))
     `(let ((,xx ,x)) (case ,n (0 ,xx) (otherwise (funcall ,key ,xx))))))
 
 (defmacro comp-red (f) 
   `(let ((,f ,f))
      (if (eq ,f #'+) 0
	(if (eq ,f #'*) 1 
	  (if (eq ,f #'min) 2
	    (if (eq ,f #'max) 3
	      (if (eq ,f #'logior) 4 
		(if (eq ,f #'logxor) 5
		  (if (eq ,f #'logand) 6
		    (if (eq ,f #'cons) 7
		      (if (eq ,f #'list) 8 9)))))))))))
 
 (defmacro comp-test (test test-not) 
   `(+ (if ,test-not 1 0)
       (let ((,test ,test))
	 (if (eq ,test #'eq) 0
	   (if (eq ,test #'eql) 2
	     (if (eq ,test #'equal) 4
	       (if (eq ,test #'equalp) 6 
		 (if (eq ,test #'funcall) 8 10))))))))
 
 (defmacro do-test (test nn x y) 
   (let ((r (sgen))(n (sgen))(nx (sgen))(ny (sgen)))
     `(let* ((,n ,nn)(,nx ,x)(,ny ,y)
	     (,r (case ,n 
		       ((0 1) (eq ,nx ,ny))
		       ((2 3) (eql ,nx ,ny))
		       ((4 5) (equal ,nx ,ny))
		       ((6 7) (equalp ,nx ,ny))
		       ((8 9) (funcall ,nx ,ny))
		       (otherwise (funcall ,test ,nx ,ny)))))
	(if (= (logand ,n 1) 1) (not ,r) ,r))))
 
 (defmacro do-red (f nn x y) 
   (let ((n (sgen))(nx (sgen))(ny (sgen)))
     `(let* ((,n ,nn)(,nx ,x)(,ny ,y))
	(case ,n 
	      (0 (+ ,nx ,ny))
	      (1 (* ,nx ,ny))
	      (2 (min ,nx ,ny))
	      (3 (max ,nx ,ny))
	      (4 (logior ,nx ,ny))
	      (5 (logxor ,nx ,ny))
	      (6 (logand ,nx ,ny))
	      (7 (cons ,nx ,ny))
	      (8 (list ,nx ,ny))
	      (otherwise (funcall f ,nx ,ny))))))
 
 (defmacro bump-test (nn i) 
   (let ((n (sgen)))
     `(let ((,n ,nn))
	(case ,n 
	      ((0 1) ,n) 
	      ((2 3) (- ,n (if (eql-is-eq ,i) 2 0)))
	      ((4 5) (- ,n (if (equal-is-eq ,i) 4 0)))
	      ((6 7) (- ,n (if (equalp-is-eq ,i) 6 0)))
	      (otherwise ,n)))))
 
 
 (defmacro mrotatef (a b &aux (s (sgen "MRF-S"))) `(let ((,s ,a)) (setf ,a ,b ,b ,s)))
 
 (defmacro raref (a seq i j l) 
   `(if ,l 
	(mrotatef (car (aref ,a ,i)) (car (aref ,a ,j)))
      (set-array ,seq ,i ,seq ,j t)))
 
 (defmacro garef (a seq i l) `(if ,l (car (aref ,a ,i)) (aref ,seq ,i)))
 
 (defconstant +seq-ll+ '(from-end key start end))
 
 (defmacro defnseq (n (il seq countp testp ifp &optional ivp) &body body)
   `(progn
      (defun ,n ,(append `(,@il ,seq &key) 
			 (when ivp (list (list 'initial-value nil 'ivsp)))
			 (when testp (list 'test 'test-not))
			 (when countp (list 'count)) +seq-ll+)
	(declare (optimize (safety 1)))
	(check-type ,seq proper-sequence)
	(check-type start (or null seqind))
	(check-type end (or null seqind))
	,@(when countp `((check-type count (or null integer))))
	,@(when testp `((and test test-not (error "both test and test not supplied"))))
	
	(let* ,(when countp `((count (if count count array-dimension-limit))
			      (count (min array-dimension-limit (max 0 count)))))
	  (let* ((startp (when start t))(start (if start start 0))
		 (endp (when end t))(end (if end end array-dimension-limit))
		 ,@(when countp `((count count)))
		 ,@(when testp `((test (or test test-not #'eql))
				 (test (if (functionp test) test (funcallable-symbol-function test)))
				 (test-comp (comp-test test test-not))))
		 (key (or key #'identity))(key (if (functionp key) key (funcallable-symbol-function key)))
		 (key-comp  (comp-key key)))
	    (let* ((l (listp s))
		   (hls (or (and from-end (or endp startp)) (not l)))
		   (ls  (if hls (length s) array-dimension-limit))
		   (end (if (< ls end) ls end)))
	      ,@body))))
      ,@(when ifp
	  `((defun ,(intern (string-concatenate (string n) "-IF")) 
	      ,(append `(,@il ,seq &key) (when countp (list 'count)) +seq-ll+)
	      (declare (optimize (safety 1)))
	      (check-type ,seq proper-sequence)
	      (,n ,@il ,seq :test 'funcall :key key :start start :end end :from-end from-end 
		  ,@(when countp `(:count count))))
	    (defun ,(intern (string-concatenate (string n) "-IF-NOT")) 
	      ,(append `(,@il ,seq &key) (when countp (list 'count)) +seq-ll+)
	      (declare (optimize (safety 1)))
	      (check-type ,seq proper-sequence)
	      (,n ,@il ,seq :test-not 'funcall :key key :start start :end end :from-end from-end 
		  ,@(when countp `(:count count)))))))))


(defun length (x)
  (declare (optimize (safety 1)))
  (check-type x sequence)
  (labels ((ll (x i) (declare (seqind i)) (if x (ll (cdr x) (1+ i)) i)))
	  (if (listp x) (ll x 0) (if (array-has-fill-pointer-p x) (c-vector-fillp x) (array-dimension x 0)))))

;; (defun length (x)
;;   (declare (optimize (safety 2)))
;;   (check-type x sequence)
;;   (labels ((ll (x &optional (i 0)) 
;; 	       (declare (seqind i))
;; 	       (if (endp x) i (ll (cdr x) (1+ i)))))
;; 	  (if (listp x)
;; 	      (ll x 0)
;; 	      (if (array-has-fill-pointer-p x) (fill-pointer x) (array-dimension x 0)))))


(defun elt (seq n)
  (declare (optimize (safety 1)))
  (check-type seq sequence)
  (check-type n seqind)
  (labels ((err nil (error 'type-error :datum n :expected-type `(integer 0 (,(length seq))))))
	  (if (listp seq)
	      (let ((r (nthcdr n seq)))
		(if r (car r) (err)))
	    (if (< n (length seq)) (aref seq n) (err)))))

(defun nreverse (s)
  (declare (optimize (safety 1)))
  (check-type s proper-sequence)
  (labels ((lr (tl &optional hd) (if tl (lr (cdr tl) (rplacd tl hd)) hd))
	   (la (&optional (i 0) (j (1- (length s))))
	       (cond ((< i j) (set-array s i s j t) (la (1+ i) (1- j))) (s))))
	  (if (listp s) (lr s) (la))))

(defun reverse (s)
  (declare (optimize (safety 1)))
  (check-type s sequence);FIXME
  (labels ((lr (tl &optional hd) (if tl (lr (cdr tl) (cons (car tl) hd)) hd))
	   (la (&optional (ls (length s)) (r (make-array ls :element-type (array-element-type s))) (i 0) (j (1- ls)))
	       (cond ((and (< i ls) (>= j 0)) (set-array r i s j) (la ls r (1+ i) (1- j))) (r))))
	  (if (listp s) (lr s) (la))))


(defun subseq (s start &optional end)
  (declare (optimize (safety 1)))
  (check-type s sequence)
  (check-type start seqind)
  (check-type end (or null seqind))

  (let ((s s)(start start)(end (or end array-dimension-limit)))
    (declare (sequence s) (seqind start end))
    (cond ((listp s)
	   (do ((i start (1+ i))(r)(rp)(p (nthcdr start s) (cdr p))) ((or (>= i end) (endp p)) r)
	       (let ((tmp (cons (car p) nil))) (setq rp (if rp (cdr (rplacd rp tmp)) (setq r tmp))))))
	  ((let* ((ls (length s))
		  (end (if (> end ls) ls end))
		  (r (make-array (- end start) :element-type (array-element-type s)))
		  (lr (length r)))
	     (do ((j 0 (1+ j))(i start (1+ i))) ((or (>= i end) (>= j lr)) r)
		  (set-array r j s i)))))))

(defnseq delete-duplicates (nil s nil t nil)

  (when from-end 
    (setq s (nreverse s))
    (let ((tmp start))
      (setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
  (let* ((r (unless (and l (= start 0)) s))(rp (when (and l (> start 0)) (nthcdr (1- start) r))))
  (do ((i start (1+ i))
       (p (when l (if rp (cdr rp) s)) (cdr p))
       (ri start))
      ((or (>= i end) (when l (endp p)))
       (let ((r (cond (l (when rp (rplacd rp p)) (or r p))
		      ((do ((m i (1+ m))) ((>= m ls) 
					   (cond ((array-has-fill-pointer-p r) 
						  (setf (fill-pointer r) ri) r)
						 ((subseq r 0 ri))))
			       (setf (aref r ri) (aref r m) ri (1+ ri)))))))
	 (if from-end (nreverse r) r)))
    (declare (seqind ri))
    (let ((el1 (do-key key key-comp (if l (car p) (aref s i)))))
      (unless
	  (do ((j (1+ i) (1+ j))
	       (q (cdr p) (cdr q))
	       (test-comp (bump-test test-comp el1)))
	      ((or (>= j end) (when l (endp q))))
	    (let ((el2 (do-key key key-comp (if l (car q) (aref s j)))))
	      (when (do-test test test-comp el1 el2)
		(return t))))
	(cond (l (setq rp (if rp (cdr (rplacd rp p)) p) r (or r p)))
	      ((setf (aref r ri) (aref r i) ri (1+ ri)))))))))

(defnseq remove-duplicates (nil s nil t nil)

  (when from-end 
    (setq s (reverse s))
    (let ((tmp start))
      (setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
  (do ((i start (1+ i))
       (k 0)
       (h s)
       (p (when l (nthcdr start s)) (cdr p))
       (r)(rp)(ri 0))
      ((or (>= i end) (when l (endp p)))
       (let ((r (cond (l (nconc r h))
		      ((not r) s)
		      ((do ((m k (1+ m))) ((>= m ls) (setf (fill-pointer r) ri) r) 
			   (set-array r ri s m)(setf ri (1+ ri)))))))
	 (if from-end (nreverse r) r)))
      (declare (seqind ri))
      (let* ((el1 (if l (car p) (aref s i)))
	     (el1 (do-key key key-comp el1)))
	(when
	    (do ((j (1+ i) (1+ j))
		 (q (cdr p) (cdr q))
		 (test-comp (bump-test test-comp el1)))
		((or (>= j end) (when l (endp q))))
		(let* ((el2 (if l (car q) (aref s j)))
		       (el2 (do-key key key-comp el2)))
		  (when (do-test test test-comp el1 el2)
		    (return t))))
	  (unless (or l r) (setq r (make-array ls :element-type (array-element-type s) :fill-pointer 0) ri 0))
	  (do ((m k (1+ m))(hp (when l h) (cdr hp))) ((>= m i) (setq k (1+ i) h (cdr hp)))
	      (if l 
		  (let ((tmp (cons (car hp) nil))) (setq rp (if rp (cdr (rplacd rp tmp)) (setq r tmp))))
		(progn (set-array r ri s m)(setf ri (1+ ri)))))))))


(defnseq substitute ((newitem item) s t t t)
  (let* ((test-comp (bump-test test-comp item)))
    (when from-end 
      (setq s (reverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (do ((i start (1+ i))(k 0)(j 0)(h s)(r)(rp)
	 (p (when l (nthcdr start s)) (cdr p)))
	((or (>= i end) (>= j count) (when l (endp p)))
	 (let ((r (cond (l (nconc r h))
			((not r) s)
			((do ((m k (1+ m))) ((>= m ls) (setf (fill-pointer r) m) r) 
			     (setf (aref r m) (aref s m)))))))
	   (if from-end (nreverse r) r)))
	(let* ((el2 (do-key key key-comp (if l (car p) (aref s i)))))
	  (when (do-test test test-comp item el2)
	    (unless (or l r) (setq r (make-array ls :element-type (array-element-type s) :fill-pointer t)))
	    (do ((m k (1+ m))(hp (when l h) (cdr hp))) ((> m i) (setq k (1+ i) j (1+ j) h hp))
		(cond (l (let ((tmp (cons (if (eq p hp) newitem (car hp)) nil))) (setq rp (if rp (cdr (rplacd rp tmp)) (setq r tmp)))))
		      ((if (= i m) (setf (aref r m) newitem) (set-array r m s m))))))))))

(defnseq nsubstitute ((newitem item) s t t t)
  (let* ((test-comp (bump-test test-comp item)))
    (when from-end 
      (setq s (nreverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (let* ()
      (do ((i start (1+ i))(j 0)
	   (p (when l (nthcdr start s)) (cdr p)))
	  ((or (>= i end) (>= j count) (when l (endp p))) (if from-end (nreverse s) s))
	  (let ((el2 (do-key key key-comp (if l (car p) (aref s i)))))
	    (when (do-test test test-comp item el2)
	      (incf j) 
	      (cond (l (rplaca p newitem))
		    ((setf (aref s i) newitem)))))))))

(defnseq count ((item) s nil t t)
  (let* ((test-comp (bump-test test-comp item)))
    (when from-end 
      (setq s (reverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (do ((i start (1+ i))(j 0)
	 (p (when l (nthcdr start s)) (cdr p)))
	((or (>= i end) (when l (endp p))) j)
	(declare (seqind j));FIXME iteration counting
	(let ((el2 (do-key key key-comp (if l (car p) (aref s i)))))
	  (when (do-test test test-comp item el2)
	    (incf j))))))

(defnseq position ((item) s nil t t)
  (let* ((test-comp (bump-test test-comp item))
	 (ls (if (and (not hls) from-end) (length s) ls))
	 (hls (or from-end hls)))
    (when from-end 
      (setq s (reverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (do ((i start (1+ i))
	 (p (when l (nthcdr start s)) (cdr p)))
	((or (>= i end) (when l (endp p))))
	(let ((el2 (do-key key key-comp (if l (car p) (aref s i)))))
	  (when (do-test test test-comp item el2)
	    (return-from position (if from-end (the seqind (- ls (1+ i))) i)))))));FIXME

(defnseq find ((item) s nil t t)
  (let* ((test-comp (bump-test test-comp item))
	 (ls (if (and (not hls) from-end) (length s) ls))
	 (hls (or from-end hls)))
    (when from-end 
      (setq s (reverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (do ((i start (1+ i))
	 (p (when l (nthcdr start s)) (cdr p)))
	((or (>= i end) (when l (endp p))))
	(let* ((el (if l (car p) (aref s i)))
	       (el2 (do-key key key-comp el)))
	  (when (do-test test test-comp item el2)
	    (return-from find el))))))


(defnseq remove ((item) s t t t)
  (let* ((test-comp (bump-test test-comp item)))
    (when from-end 
      (setq s (reverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (let ((end1   (if l end (min (1+ end) array-dimension-limit)))
	  (count1 (if l count (min (1+ count) array-dimension-limit))))
      (do ((i start (1+ i))(k 0)(j 0)(h s)(r)(rp)(ri 0)
	   (p (when l (nthcdr start s)) (cdr p)))
	  ((or (>= i end1) (>= j count1) (when l (endp p)))
	   (let ((r (cond (l (when rp (rplacd rp h)) (or r h))
			  ((not r) s)
			  (t (setf (fill-pointer r) ri) r))))
	     (if from-end (nreverse r) r)))
	  (declare (seqind ri k));FIXME
	  (let* ((e (or (= i end) (= j count)))
		 (i (if e (if r ls k) i)))
	    (when (or e (do-test test test-comp item (do-key key key-comp (if l (car p) (aref s i)))))
	      (unless (or l e r) (setq r (make-array ls :element-type (array-element-type s) :fill-pointer 0)))
	      (do ((m k (1+ m))(hp (when l h) (cdr hp))) ((>= m i) (setq k (1+ i) j (1+ j) h (cdr hp)))
		  (cond (l (let ((tmp (cons (car hp) nil))) (setq rp (if rp (cdr (rplacd rp tmp)) (setq r tmp)))))
			(t (set-array r ri s m)(setf ri (1+ ri)))))))))))

(defnseq delete ((item) s t t t)
  (let* ((test-comp (bump-test test-comp item)))
    (when from-end 
      (setq s (nreverse s))
      (let ((tmp start))
	(setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
    (let* ((r (unless (and (listp s) (= start 0)) s))(rp (when (and l (> start 0)) (nthcdr (1- start) r))));FIXME compiler aid
      (do ((i start (1+ i))(j 0)(ri start)
	   (p (when (listp s) (if rp (cdr rp) s)) (cdr p)));FIXME compiler aid
	  ((or (>= i end) (>= j count) (when l (endp p)))
	   (let ((r (cond (l (when rp (rplacd rp p)) (or r p))
			  ((do ((m i (1+ m))) 
			       ((>= m ls) 
				(cond ((array-has-fill-pointer-p r) (setf (fill-pointer r) ri) r) 
				      ((subseq r 0 ri))))
			       (set-array r ri r m)(setf ri (1+ ri)))))))
	     (if from-end (nreverse r) r)))
	  (cond ((do-test test test-comp item (do-key key key-comp (if l (car p) (aref s i)))) (incf j))
		((cond (l (setq rp (if rp (cdr (rplacd rp p)) p) r (or r p)))
		       (t (set-array r ri r i)(setf ri (1+ ri))))))))))


(defun tofn (o);FIXME coerce function, elsewhere
  (etypecase 
   o
   (function o) 
   (otherwise (the function (c-symbol-gfdef o)))))
(putprop 'tofn t 'compiler::cmp-inline)

(deftype fn nil `(satisfies fnp))
(defun fnp (x)
  (typecase
   x
   (function t)
   ((and symbol (not boolean))
    (and (= 0 (c-symbol-mflag x))
	 (/= 0 (address (c-symbol-gfdef x)))))))
(putprop 'fnp t 'compiler::cmp-inline)

(defun reduce (fd s &key key from-end (start 0) end (initial-value nil ivp) 
		 &aux (kf (when key (coerce key 'function)))(f (coerce fd 'function))
		 (l (listp s))(e (or end (if l array-dimension-limit (length s)))))
  (declare (optimize (safety 1)))
  (check-type fd function-designator)
  (check-type s sequence)
  (check-type key (or null function-designator))
  (check-type start seqind)
  (check-type end (or null seqind))
  (labels ((k (s i &aux (z (if l (car s) (aref s i)))) (if kf (funcall kf z) z))
	   (fc (r k) (prog1 (if ivp (funcall f (if from-end k r) (if from-end r k)) k) (setq ivp t)))
	   (rl (s i res)
	       (cond ((or (>= i e) (when l (endp s))) (if (or ivp res) res (values (funcall f))))
		     (from-end (fc (rl (if l (cdr s) s) (1+ i) (if ivp res t)) (k s i)))
		     ((rl (if l (cdr s) s) (1+ i) (fc res (k s i)))))))
    (rl (if l (nthcdr start s) s) start initial-value)))

;; (defun reduce (fd s &key key from-end (start 0) end (initial-value nil ivp) 
;; 		 &aux (key (if key (coerce key 'function) #'identity))(f (coerce fd 'function))
;; 		 (l (listp s))(lim (if l array-dimension-limit (length s)))(ftt ivp)(e (or end lim)))
;;   (declare (optimize (safety 2)))
;;   (check-type fd function-designator)
;;   (check-type s sequence)
;;   (check-type key (or null function-designator))
;;   (check-type start seqind)
;;   (check-type end (or null seqind))
;;   (labels ((rl (s &optional (i 0) (res initial-value) (ft ivp))
;; 	       (if (or (>= i e) (when l (endp s)))
;; 		   (if ft res (values (funcall f)))
;; 	       (let ((k (funcall key (if l (car s) (aref s i)))))
;; 		 (if from-end 
;; 		     (let ((r (rl (if l (cdr s) s) (1+ i) (if ftt res k) t)))
;; 		       (cond (ftt (funcall f k r)) ((setq ftt t) r)))
;; 		   (rl (if l (cdr s) s) (1+ i) (if ft (funcall f res k) k) t))))))
;; 	    (rl (if l (nthcdr start s) s) start)))

;; (defun reduce (f s &key key from-end (start 0) end (initial-value nil ivp) 
;; 		 &aux (key (if key (tofn key) #'identity))(f (tofn f))
;; 		 (l (listp s))(lim (if l array-dimension-limit (length s)))(ftt ivp)(e (or end lim)))
;;   (declare (optimize (safety 2)))
;;   (check-type f fn)
;;   (check-type s sequence)
;;   (check-type key (or null fn))
;;   (check-type start seqind)
;;   (check-type end (or null seqind))
;;   (labels ((rl (s &optional (i 0) (res initial-value) (ft ivp))
;; 	       (if (or (>= i e) (when l (endp s)))
;; 		   (if ft res (funcall f))
;; 	       (let ((k (funcall key (if l (car s) (aref s i)))))
;; 		 (if from-end 
;; 		     (let ((r (rl (if l (cdr s) s) (1+ i) (if ftt res k) t)))
;; 		       (cond (ftt (funcall f k r)) ((setq ftt t) r)))
;; 		   (rl (if l (cdr s) s) (1+ i) (if ft (funcall f res k) k) t))))))
;; 	    (rl (if l (nthcdr start s) s) start)))

;; (defnseq reduce ((f) s nil nil nil t)
;;   (when from-end 
;;     (setq s (reverse s))
;;     (let ((tmp start))
;;       (setq start (if endp (- ls end) 0) end (if hls (- ls tmp) array-dimension-limit))))
;;   (do ((p (when l (nthcdr start s)) (cdr p))
;;        (i start (1+ i))
;;        (f (if (functionp f) f (funcallable-symbol-function f)))
;;        (red-comp (comp-red f))
;;        (rx initial-value (let* ((el (do-key key key-comp (if l (car p) (aref s i))))
;; 				(ry (if from-end rx el))
;; 				(rx (if from-end el rx)))
;; 			  (cond (ivsp (do-red f red-comp rx ry))
;; 				((setq ivsp t) el)))))
;;       ((or (>= i end) (when l (endp p))) (if ivsp rx (values (funcall f))))))


(eval-when 
 (compile eval)

 (defmacro locsym (f s) `(sgen (string-concatenate (string ,f) ,s)))

 (defmacro dyncpl (x &aux (l (locsym 'dyncpl "-LOOP")));FIXME this can't cons in a labels as it might be a separate fn.  Get do to unroll too.
   `(labels ((,l (x y) (when x (setf (car x) (car y)) (,l (cdr x) (cdr y)))))
	    (declare (notinline make-list))
	    (let ((tmp (make-list (length ,x))))
	      (declare (:dynamic-extent tmp))
	      (,l tmp ,x);Can't be mapl, used by
	     tmp)))

 (defmacro seqend (seq seqs &aux (l (locsym 'seqend "-LOOP")))
   `(labels ((,l (s &aux (x (car s)) (y (length x))) (if s (min y (,l (cdr s))) (length ,seq))))
	    (,l ,seqs)))
 
 (defmacro seqval (seq place i)
   `(if (listp ,seq) (pop ,place) (aref ,seq ,i)))

 (defmacro seqvals (vals ns i)
   `(mapl (lambda (x y &aux (yc (car y))) (setf (car x) (seqval yc (car y) ,i))) ,vals ,ns))
 
 ;; (defmacro defevsm (f o w &aux (l (locsym f "-LOOP")))
 ;;   `(defun ,f (pred seq &rest seqs &aux (end (seqend seq seqs)) (ns (dyncpl seqs)) (vals (dyncpl seqs)))
 ;;      (declare (optimize (safety 2))(:dynamic-extent seqs))
 ;;      (check-type seq proper-sequence)
 ;;      (labels ((,l (i) (,o (>= i end)
 ;; 			     (,w (apply pred (seqval seq seq i) (seqvals vals ns i))
 ;; 				 (,l (1+ i))))))
 ;; 	      (,l 0))))
)


;; (defevsm every or   when)
;; (defevsm some  unless or)

(defun every (pred seq &rest seqs &aux (pred (coerce pred 'function)))
  (declare (optimize (safety 1))(dynamic-extent seqs))
  (check-type pred function-designator)
  (check-type seq sequence)
  (apply 'map nil (lambda (x &rest r) (unless (apply pred x r) (return-from every nil))) seq seqs)
  t)

(defun some (pred seq &rest seqs &aux (pred (coerce pred 'function)))
  (declare (optimize (safety 1))(dynamic-extent seqs))
  (check-type pred function-designator)
  (check-type seq sequence)
  (apply 'map nil (lambda (x &rest r &aux (v (apply pred x r))) (when v (return-from some v))) seq seqs))

(defun notevery (pred seq &rest seqs)
  (declare (optimize (safety 1))(:dynamic-extent seqs))
  (check-type seq proper-sequence)
  (not (apply 'every pred seq seqs)))

(defun notany (pred seq &rest seqs)
  (declare (optimize (safety 1))(:dynamic-extent seqs))
  (check-type seq proper-sequence)
  (not (apply 'some pred seq seqs)))


(defun seqtype (sequence)
  (cond ((listp sequence) 'list)
        ((stringp sequence) 'string)
        ((bit-vector-p sequence) 'bit-vector)
        ((vectorp sequence) (list 'array (array-element-type sequence)))
        (t (error "~S is not a sequence." sequence))))

(defmacro call-test (test test-not item keyx)
  (let ((j1 (sgen)) (j2 (sgen))(tst (sgen))(tstn (sgen)))
    `(let ((,j1 ,item)(,j2 ,keyx)(,tst ,test)(,tstn ,test-not))
       (cond (,tst (funcall ,tst ,j1 ,j2))
	     (,tstn (not (funcall ,tstn ,j1 ,j2)))
	     ((eql ,j1 ,j2))))))


(defun bad-seq-limit (x y)
  (declare (seqind x y))
  (error 'type-error :datum x  :expected-type (if (= y 0) '(integer 0) '(integer 0 y))))


(eval-when (compile eval)
(defmacro f+ (x y) `(the fixnum (+ (the fixnum ,x) (the fixnum ,y))))
(defmacro f- (x y) `(the fixnum (- (the fixnum ,x) (the fixnum ,y))))

(defmacro with-start-end (start end seq &body body)
  `(let ((,start (the-start ,start)))
     (check-type ,seq sequence)
     (let ((,seq ,seq));;FIXME
       (declare (sequence ,seq))
       (let ((,end (the-end ,end (length ,seq))))
	 (or (<= ,start ,end) (bad-seq-limit  ,start ,end))
	 ,@body))))

(defmacro with-start-end-length (start end length seq &body body)
  `(let ((,start (the-start ,start)))
     (check-type ,seq sequence)
     (let ((,seq ,seq));;FIXME
       (declare (sequence ,seq))
       (let* ((,length (length ,seq))(,end (the-end ,end ,length)))
	 (or (<= ,start ,end) (bad-seq-limit  ,start ,end))
	 ,@body)))))

(defun the-end (x y)
  (declare (seqind y))
  (cond ((seqindp x)
	 (unless (<= x y)
	   (bad-seq-limit x y))
	 x)
	((null x) y)
	(t (error 'type-error :datum x :expected-type '(or null seqind)) y)))
	
(defun the-start (x)
  (cond ((seqindp x)
	 (unless (>= x 0)
	     (bad-seq-limit x 0))
	 x)
	((null x) 0)
	(t (error 'type-error :datum x :expected-type '(or null seqind)))))
  

(defun fill (sequence item &key start end );FIXME
  (declare (optimize (safety 1)))
  (with-start-end start end sequence
		  (do ((i start (f+ 1 i)))
		      ((>= i end) sequence)
		     (declare (fixnum i))
		     (setf (elt sequence i) item))))


(defun replace (s1 s2 &key (start1 0) end1 (start2 0) end2 &aux (os1 s1) s3)
  (declare (optimize (safety 1))(notinline make-list)(dynamic-extent s3))
  (check-type s1 sequence)
  (check-type s2 sequence)
  (check-type start1 seqind)
  (check-type start2 seqind)
  (check-type end1 (or null seqind))
  (check-type end2 (or null seqind))
  (when (and (eq s1 s2) (> start1 start2))
    (setq s3 (make-list (length s2)) s2 (replace s3 s2)))
  (let* ((lp1 (listp s1)) (lp2 (listp s2))
	 (e1 (or end1 (if lp1 array-dimension-limit (length s1))))
	 (e2 (or end2 (if lp2 array-dimension-limit (length s2)))))
    (do ((i1 start1 (1+ i1))(i2 start2 (1+ i2))
	 (s1 (if lp1 (nthcdr start1 s1) s1) (if lp1 (cdr s1) s1))
	 (s2 (if lp2 (nthcdr start2 s2) s2) (if lp2 (cdr s2) s2)))
	((or (not s1) (>= i1 e1) (not s2) (>= i2 e2)) os1)
	(let ((e2 (if lp2 (car s2) (aref s2 i2))))
	  (if lp1 (setf (car s1) e2) (setf (aref s1 i1) e2))))))

       

(defun mismatch (sequence1 sequence2
		 &key from-end test test-not
		      (key #'identity)
		      start1 start2
		      end1 end2)
  (declare (optimize (safety 1)))
  (and test test-not (error "both test and test not supplied"))
  (with-start-end start1 end1 sequence1
   (with-start-end start2 end2 sequence2
    (if (not from-end)
        (do ((i1 start1 (f+ 1  i1))
             (i2 start2  (f+ 1  i2)))
            ((or (>= i1 end1) (>= i2 end2))
             (if (and (>= i1 end1) (>= i2 end2)) nil i1))
          (declare (fixnum i1 i2))
          (unless (call-test test test-not
                             (funcall key (elt sequence1 i1))
                             (funcall key (elt sequence2 i2)))
                  (return i1)))
        (do ((i1 (f+ -1  end1) (f+ -1  i1))
             (i2 (f+ -1  end2)  (f+ -1  i2)))
            ((or (< i1 start1) (< i2 start2))
             (if (and (< i1 start1) (< i2 start2)) nil (f+ 1 i1)))
          (declare (fixnum i1 i2))
          (unless (call-test test test-not
                             (funcall key (elt sequence1 i1))
                             (funcall key (elt sequence2 i2)))
                  (return (f+ 1 i1))))))))


(defun search (sequence1 sequence2
               &key from-end test test-not
                    key
		    (start1 0) (start2 0)
		    end1 end2)
  (declare (optimize (safety 1)))
  (and test test-not (error "both test and test not supplied"))
  (check-type sequence1 sequence)
  (check-type sequence2 sequence)
  (check-type start1 seqind)
  (check-type start2 seqind)
  (when end1 (check-type end1 seqind))
  (when end2 (check-type end2 seqind))

  (let ((s1 sequence1)(s2 sequence2)(i1 start1)(i2 start2)(e1 end1)(e2 end2)(st (or test test-not)))
    (declare (sequence s1 s2)(seqind i1 i2)((or null seqind) e1 e2))
    (let* ((eq (unless st (every (lambda (x) (eql-is-eq (if key (funcall key x) x))) s1))) m (mv 0) x1
	   (l1 (listp s1))(l2 (listp s2))
	   (e1 (or e1 (unless l1 (length s1))))(e2 (or e2 (unless l2 (length s2)))))
      (do ((is2 i2 (1+ is2))
	   (ps2 s2 p2)
	   (p2 (when l2 (nthcdr i2 s2)) (cdr p2))
	   (p1 (when l1 (nthcdr i1 s1))))
	  ((if e2 (> is2 e2) (endp ps2)) (when m mv))
	  (declare (seqind is2))
	  (do ((p1 p1 (cdr p1))
	       (p2 p2 (cdr p2))
	       (i1 i1  (1+ i1))
	       (i2 is2 (1+ i2)))
	      ((or (setq x1 (if e1 (>= i1 e1) (endp p1)))
		   (if e2 (>= i2 e2) (endp p2)))
	       (when x1 (if from-end (setq m t mv is2) (return-from search is2))))
	      (declare (seqind i1 i2))
	      (let ((el1 (if l1 (car p1) (aref s1 i1)))
		    (el2 (if l2 (car p2) (aref s2 i2))))
		(when key (setq el1 (funcall key el1) el2 (funcall key el2)))
		(unless
		    (cond (eq (eq el1 el2))
			  ((not st) (eql el1 el2))
			  (test (funcall test el1 el2))
			  ((not (funcall test-not el1 el2))))
		  (return nil))))))))

(defun sort (seq pred &key (key 'identity))
  (declare (optimize (safety 1)))
  (check-type seq sequence)
  (let* ((k (comp-key key))
	 (ll (length seq))
	 (list (listp seq))
	 (a (when list (make-array ll))))
    (when list
      (do ((fi 0 (1+ fi)) (l seq (cdr l))) ((>= fi ll)) (setf (aref a fi) l)))
    (do ((ii (list ll 0))) ((not ii) seq)
	(declare (:dynamic-extent ii))
	(let* ((ls (pop ii)) (fi (pop ii)))
	  (declare (seqind ls fi))
	  (do nil ((>= fi (1- ls)))
	    (let* ((spi (+ fi (random (- ls fi))))
		   (sp (do-key key k (garef a seq spi list))))
	      (raref a seq fi spi list)
	      (do ((lf fi) (rt ls)) ((>= lf rt))
		(declare (seqind lf rt));FIXME
		(do ((q t)) 
		    ((or (>= (if q (incf lf) lf) (if q rt (decf rt)))
			 (let ((f (do-key key k (garef a seq (if q lf rt) list))))
			   (and (not (funcall pred (if q f sp) (if q sp f)))
				(setq q (not q)))))))
		(let* ((r (< lf rt))
		       (f (if r lf fi))
		       (s (if r rt (setq spi (1- lf)))))
		  (raref a seq f s list)))
	      (let* ((ospi (1+ spi))
		     (b   (< (- ls ospi) (- spi fi)))
		     (lf  (if b ospi 0))
		     (rt  (if b 0 spi))
		     (b1  (if b (> (- ls lf) 1) (> (- rt fi) 1)))
		     (ns  (if b lf fi))
		     (ns1 (if b ls rt))
		     (nls (if b spi ls))
		     (nfi (if b fi ospi)))
		(when b1
		  (push ns ii) (push ns1 ii))
		(setq ls nls fi nfi))))))))

(defun list-merge-sort (l pred key k)

  (let* ((ll (length l)))
    (if (< ll 2) l
      (let* ((i (ash ll -1))
	     (lf l)
	     (l1 (nthcdr (1- i) l))
	     (rt (prog1 (cdr l1) (rplacd l1 nil)))
	     (lf (list-merge-sort lf pred key k))
	     (rt (list-merge-sort rt pred key k)))
	(do (l0 l1) ((not (and lf rt)) l0)
	  (cond ((funcall pred (do-key key k (car rt)) (do-key key k (car lf)))
		 (setq l1 (if l1 (cdr (rplacd l1 rt)) (setq l0 rt)) rt (cdr rt))
		 (unless rt (rplacd l1 lf)))
		(t (setq l1 (if l1 (cdr (rplacd l1 lf)) (setq l0 lf)) lf (cdr lf))
		   (unless lf (rplacd l1 rt)))))))))



(defun stable-sort (sequence predicate &key (key #'identity))
  (declare (optimize (safety 1)))
  (check-type sequence sequence)
  (typecase 
   sequence
   (list (list-merge-sort sequence predicate key (comp-key key)))
   (string (sort sequence predicate :key key))
   (bit-vector (sort sequence predicate :key key))
   (otherwise 
    (coerce (list-merge-sort (coerce sequence 'list) predicate key (comp-key key))
	    (seqtype sequence)))))

(defun merge (result-type sequence1 sequence2 predicate
	      &key (key #'identity)
	      &aux (l1 (length sequence1)) (l2 (length sequence2)))
  (declare (optimize (safety 1)))
  (declare (fixnum l1 l2))
  (when (equal key 'nil) (setq key #'identity))
  (do ((newseq (make-sequence result-type (the fixnum (f+ l1 l2))))
       (j 0 (f+ 1  j))
       (i1 0)
       (i2 0))
      ((and (= i1 l1) (= i2 l2)) newseq)
    (declare (fixnum j i1 i2))
    (cond ((and (< i1 l1) (< i2 l2))
	   (cond ((funcall predicate
			   (funcall key (elt sequence1 i1))
			   (funcall key (elt sequence2 i2)))
		  (setf (elt newseq j) (elt sequence1 i1))
		  (setf  i1 (f+ 1  i1)))
		 ((funcall predicate
			   (funcall key (elt sequence2 i2))
			   (funcall key (elt sequence1 i1)))
		  (setf (elt newseq j) (elt sequence2 i2))
		  (setf  i2 (f+ 1  i2)))
		 (t
		  (setf (elt newseq j) (elt sequence1 i1))
		  (setf  i1 (f+ 1  i1)))))
          ((< i1 l1)
	   (setf (elt newseq j) (elt sequence1 i1))
	   (setf  i1 (f+ 1  i1)))
	  (t
	   (setf (elt newseq j) (elt sequence2 i2))
	   (setf  i2 (f+ 1  i2))))))

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  (declare (optimize (safety 1)))
  (let ((table (sgen))
	(ind (sgen)))
    `(let ((,table ,hash-table)
	   (,ind 0))
       (macrolet ((,name ()
			 `(multiple-value-bind
			   (more key val)
			   (si::next-hash-table-entry ,',table ,',ind)
			   (cond ((>= (the fixnum more) 0)
				  (setq ,',ind more)
				  (values t key val))))))
		 ,@body))))
		 

(defun copy-seq (s) 
  (declare (optimize (safety 1)))
  (check-type s sequence)
  (if (listp s)
      (copy-list s)
    (let* ((n (length s))
	   (o (make-array n :element-type (array-element-type s))))
      (do ((i 0 (1+ i))) ((>= i n) o) 
	  (set-array o i s i)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_seqlib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_export.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;    export.lsp
;;;;
;;;;                    Exporting external symbols of LISP package

(in-package :cl)

(export '(
       &allow-other-keys            *print-miser-width*          
       &aux                         *print-pprint-dispatch*      
       &body                        *print-pretty*               
       &environment                 *print-radix*                
       &key                         *print-readably*             
       &optional                    *print-right-margin*         
       &rest                        *query-io*                   
       &whole                       *random-state*               
       *                            *read-base*                  
       **                           *read-default-float-format*  
       ***                          *read-eval*                  
       *break-on-signals*           *read-suppress*              
       *compile-file-pathname*      *readtable*                  
       *compile-file-truename*      *standard-input*             
       *compile-print*              *standard-output*            
       *compile-verbose*            *terminal-io*                
       *debug-io*                   *trace-output*               
       *debugger-hook*              +                            
       *default-pathname-defaults*  ++                           
       *error-output*               +++                          
       *features*                   -                            
       *gensym-counter*             /                            
       *load-pathname*              //                           
       *load-print*                 ///                          
       *load-truename*              /=                           
       *load-verbose*               1+                           
       *macroexpand-hook*           1-                           
       *modules*                    <                            
       *package*                    <=                           
       *print-array*                =                            
       *print-base*                 >                            
       *print-case*                 >=                           
       *print-circle*               abort                        
       *print-escape*               abs                          
       *print-gensym*               acons                        
       *print-length*               acos                         
       *print-level*                acosh                        
       *print-lines*                add-method                   
       
       adjoin                      atom          boundp                    
       adjust-array                base-char     break                     
       adjustable-array-p          base-string   broadcast-stream          
       allocate-instance           bignum        broadcast-stream-streams  
       alpha-char-p                bit           built-in-class            
       alphanumericp               bit-and       butlast                   
       and                         bit-andc1     byte                      
       append                      bit-andc2     byte-position             
       apply                       bit-eqv       byte-size                 
       apropos                     bit-ior       caaaar                    
       apropos-list                bit-nand      caaadr                    
       aref                        bit-nor       caaar                     
       arithmetic-error            bit-not       caadar                    
       arithmetic-error-operands   bit-orc1      caaddr                    
       arithmetic-error-operation  bit-orc2      caadr                     
       array                       bit-vector    caar                      
       array-dimension             bit-vector-p  cadaar                    
       array-dimension-limit       bit-xor       cadadr                    
       array-dimensions            block         cadar                     
       array-displacement          boole         caddar                    
       array-element-type          boole-1       cadddr                    
       array-has-fill-pointer-p    boole-2       caddr                     
       array-in-bounds-p           boole-and     cadr                      
       array-rank                  boole-andc1   call-arguments-limit      
       array-rank-limit            boole-andc2   call-method               
       array-row-major-index       boole-c1      call-next-method          
       array-total-size            boole-c2      car                       
       array-total-size-limit      boole-clr     case                      
       arrayp                      boole-eqv     catch                     
       ash                         boole-ior     ccase                     
       asin                        boole-nand    cdaaar                    
       asinh                       boole-nor     cdaadr                    
       assert                      boole-orc1    cdaar                     
       assoc                       boole-orc2    cdadar                    
       assoc-if                    boole-set     cdaddr                    
       assoc-if-not                boole-xor     cdadr                     
       atan                        boolean       cdar                      
       atanh                       both-case-p   cddaar                    
       
       cddadr             clear-input                  copy-tree                  
       cddar              clear-output                 cos                        
       cdddar             close                        cosh                       
       cddddr             clrhash                      count                      
       cdddr              code-char                    count-if                   
       cddr               coerce                       count-if-not               
       cdr                compilation-speed            ctypecase                  
       ceiling            compile                      debug                      
       cell-error         compile-file                 decf                       
       cell-error-name    compile-file-pathname        declaim                    
       cerror             compiled-function            declaration                
       change-class       compiled-function-p          declare                    
       char               compiler-macro               decode-float               
       char-code          compiler-macro-function      decode-universal-time      
       char-code-limit    complement                   defclass                   
       char-downcase      complex                      defconstant                
       char-equal         complexp                     defgeneric                 
       char-greaterp      compute-applicable-methods   define-compiler-macro      
       char-int           compute-restarts             define-condition           
       char-lessp         concatenate                  define-method-combination  
       char-name          concatenated-stream          define-modify-macro        
       char-not-equal     concatenated-stream-streams  define-setf-expander       
       char-not-greaterp  cond                         define-symbol-macro        
       char-not-lessp     condition                    defmacro                   
       char-upcase        conjugate                    defmethod                  
       char/=             cons                         defpackage                 
       char<              consp                        defparameter               
       char<=             constantly                   defsetf                    
       char=              constantp                    defstruct                  
       char>              continue                     deftype                    
       char>=             control-error                defun                      
       character          copy-alist                   defvar                     
       characterp         copy-list                    delete                     
       check-type         copy-pprint-dispatch         delete-duplicates          
       cis                copy-readtable               delete-file                
       class              copy-seq                     delete-if                  
       class-name         copy-structure               delete-if-not              
       class-of           copy-symbol                  delete-package             
       
       denominator                    eq                   
       deposit-field                  eql                  
       describe                       equal                
       describe-object                equalp               
       destructuring-bind             error                
       digit-char                     etypecase            
       digit-char-p                   eval                 
       directory                      eval-when            
       directory-namestring           evenp                
       disassemble                    every                
       division-by-zero               exp                  
       do                             export               
       do*                            expt                 
       do-all-symbols                 extended-char        
       do-external-symbols            fboundp              
       do-symbols                     fceiling             
       documentation                  fdefinition          
       dolist                         ffloor               
       dotimes                        fifth                
       double-float                   file-author          
       double-float-epsilon           file-error           
       double-float-negative-epsilon  file-error-pathname  
       dpb                            file-length          
       dribble                        file-namestring      
       dynamic-extent                 file-position        
       ecase                          file-stream          
       echo-stream                    file-string-length   
       echo-stream-input-stream       file-write-date      
       echo-stream-output-stream      fill                 
       ed                             fill-pointer         
       eighth                         find                 
       elt                            find-all-symbols     
       encode-universal-time          find-class           
       end-of-file                    find-if              
       endp                           find-if-not          
       enough-namestring              find-method          
       ensure-directories-exist       find-package         
       ensure-generic-function        find-restart         
       
       find-symbol                       get-internal-run-time        
       finish-output                     get-macro-character          
       first                             get-output-stream-string     
       fixnum                            get-properties               
       flet                              get-setf-expansion           
       float                             get-universal-time           
       float-digits                      getf                         
       float-precision                   gethash                      
       float-radix                       go                           
       float-sign                        graphic-char-p               
       floating-point-inexact            handler-bind                 
       floating-point-invalid-operation  handler-case                 
       floating-point-overflow           hash-table                   
       floating-point-underflow          hash-table-count             
       floatp                            hash-table-p                 
       floor                             hash-table-rehash-size       
       fmakunbound                       hash-table-rehash-threshold  
       force-output                      hash-table-size              
       format                            hash-table-test              
       formatter                         host-namestring              
       fourth                            identity                     
       fresh-line                        if                           
       fround                            ignorable                    
       ftruncate                         ignore                       
       ftype                             ignore-errors                
       funcall                           imagpart                     
       function                          import                       
       function-keywords                 in-package                   
       function-lambda-expression        incf                         
       functionp                         initialize-instance          
       gcd                               inline                       
       generic-function                  input-stream-p               
       gensym                            inspect                      
       gentemp                           integer                      
       get                               integer-decode-float         
       get-decoded-time                  integer-length               
       get-dispatch-macro-character      integerp                     
       get-internal-real-time            interactive-stream-p         
       
       intern                                  lisp-implementation-type            
       internal-time-units-per-second          lisp-implementation-version         
       intersection                            list                                
       invalid-method-error                    list*                               
       invoke-debugger                         list-all-packages                   
       invoke-restart                          list-length                         
       invoke-restart-interactively            listen                              
       isqrt                                   listp                               
       keyword                                 load                                
       keywordp                                load-logical-pathname-translations  
       labels                                  load-time-value                     
       lambda                                  locally                             
       lambda-list-keywords                    log                                 
       lambda-parameters-limit                 logand                              
       last                                    logandc1                            
       lcm                                     logandc2                            
       ldb                                     logbitp                             
       ldb-test                                logcount                            
       ldiff                                   logeqv                              
       least-negative-double-float             logical-pathname                    
       least-negative-long-float               logical-pathname-translations       
       least-negative-normalized-double-float  logior                              
       least-negative-normalized-long-float    lognand                             
       least-negative-normalized-short-float   lognor                              
       least-negative-normalized-single-float  lognot                              
       least-negative-short-float              logorc1                             
       least-negative-single-float             logorc2                             
       least-positive-double-float             logtest                             
       least-positive-long-float               logxor                              
       least-positive-normalized-double-float  long-float                          
       least-positive-normalized-long-float    long-float-epsilon                  
       least-positive-normalized-short-float   long-float-negative-epsilon         
       least-positive-normalized-single-float  long-site-name                      
       least-positive-short-float              loop                                
       least-positive-single-float             loop-finish                         
       length                                  lower-case-p                        
       let                                     machine-instance                    
       let*                                    machine-type                        
       
       machine-version                mask-field                  
       macro-function                 max                         
       macroexpand                    member                      
       macroexpand-1                  member-if                   
       macrolet                       member-if-not               
       make-array                     merge                       
       make-broadcast-stream          merge-pathnames             
       make-concatenated-stream       method                      
       make-condition                 method-combination          
       make-dispatch-macro-character  method-combination-error    
       make-echo-stream               method-qualifiers           
       make-hash-table                min                         
       make-instance                  minusp                      
       make-instances-obsolete        mismatch                    
       make-list                      mod                         
       make-load-form                 most-negative-double-float  
       make-load-form-saving-slots    most-negative-fixnum        
       make-method                    most-negative-long-float    
       make-package                   most-negative-short-float   
       make-pathname                  most-negative-single-float  
       make-random-state              most-positive-double-float  
       make-sequence                  most-positive-fixnum        
       make-string                    most-positive-long-float    
       make-string-input-stream       most-positive-short-float   
       make-string-output-stream      most-positive-single-float  
       make-symbol                    muffle-warning              
       make-synonym-stream            multiple-value-bind         
       make-two-way-stream            multiple-value-call         
       makunbound                     multiple-value-list         
       map                            multiple-value-prog1        
       map-into                       multiple-value-setq         
       mapc                           multiple-values-limit       
       mapcan                         name-char                   
       mapcar                         namestring                  
       mapcon                         nbutlast                    
       maphash                        nconc                       
       mapl                           next-method-p               
       maplist                        nil                         
       
       nintersection         package-error                  
       ninth                 package-error-package          
       no-applicable-method  package-name                   
       no-next-method        package-nicknames              
       not                   package-shadowing-symbols      
       notany                package-use-list               
       notevery              package-used-by-list           
       notinline             packagep                       
       nreconc               pairlis                        
       nreverse              parse-error                    
       nset-difference       parse-integer                  
       nset-exclusive-or     parse-namestring               
       nstring-capitalize    pathname                       
       nstring-downcase      pathname-device                
       nstring-upcase        pathname-directory             
       nsublis               pathname-host                  
       nsubst                pathname-match-p               
       nsubst-if             pathname-name                  
       nsubst-if-not         pathname-type                  
       nsubstitute           pathname-version               
       nsubstitute-if        pathnamep                      
       nsubstitute-if-not    peek-char                      
       nth                   phase                          
       nth-value             pi                             
       nthcdr                plusp                          
       null                  pop                            
       number                position                       
       numberp               position-if                    
       numerator             position-if-not                
       nunion                pprint                         
       oddp                  pprint-dispatch                
       open                  pprint-exit-if-list-exhausted  
       open-stream-p         pprint-fill                    
       optimize              pprint-indent                  
       or                    pprint-linear                  
       otherwise             pprint-logical-block           
       output-stream-p       pprint-newline                 
       package               pprint-pop                     
       
       pprint-tab                 read-char                   
       pprint-tabular             read-char-no-hang           
       prin1                      read-delimited-list         
       prin1-to-string            read-from-string            
       princ                      read-line                   
       princ-to-string            read-preserving-whitespace  
       print                      read-sequence               
       print-not-readable         reader-error                
       print-not-readable-object  readtable                   
       print-object               readtable-case              
       print-unreadable-object    readtablep                  
       probe-file                 real                        
       proclaim                   realp                       
       prog                       realpart                    
       prog*                      reduce                      
       prog1                      reinitialize-instance       
       prog2                      rem                         
       progn                      remf                        
       program-error              remhash                     
       progv                      remove                      
       provide                    remove-duplicates           
       psetf                      remove-if                   
       psetq                      remove-if-not               
       push                       remove-method               
       pushnew                    remprop                     
       quote                      rename-file                 
       random                     rename-package              
       random-state               replace                     
       random-state-p             require                     
       rassoc                     rest                        
       rassoc-if                  restart                     
       rassoc-if-not              restart-bind                
       ratio                      restart-case                
       rational                   restart-name                
       rationalize                return                      
       rationalp                  return-from                 
       read                       revappend                   
       read-byte                  reverse                     
       
       room                          simple-bit-vector                  
       rotatef                       simple-bit-vector-p                
       round                         simple-condition                   
       row-major-aref                simple-condition-format-arguments  
       rplaca                        simple-condition-format-control    
       rplacd                        simple-error                       
       safety                        simple-string                      
       satisfies                     simple-string-p                    
       sbit                          simple-type-error                  
       scale-float                   simple-vector                      
       schar                         simple-vector-p                    
       search                        simple-warning                     
       second                        sin                                
       sequence                      single-float                       
       serious-condition             single-float-epsilon               
       set                           single-float-negative-epsilon      
       set-difference                sinh                               
       set-dispatch-macro-character  sixth                              
       set-exclusive-or              sleep                              
       set-macro-character           slot-boundp                        
       set-pprint-dispatch           slot-exists-p                      
       set-syntax-from-char          slot-makunbound                    
       setf                          slot-missing                       
       setq                          slot-unbound                       
       seventh                       slot-value                         
       shadow                        software-type                      
       shadowing-import              software-version                   
       shared-initialize             some                               
       shiftf                        sort                               
       short-float                   space                              
       short-float-epsilon           special                            
       short-float-negative-epsilon  special-operator-p                 
       short-site-name               speed                              
       signal                        sqrt                               
       signed-byte                   stable-sort                        
       signum                        standard                           
       simple-array                  standard-char                      
       simple-base-string            standard-char-p                    
       
       standard-class             sublis                      
       standard-generic-function  subseq                      
       standard-method            subsetp                     
       standard-object            subst                       
       step                       subst-if                    
       storage-condition          subst-if-not                
       store-value                substitute                  
       stream                     substitute-if               
       stream-element-type        substitute-if-not           
       stream-error               subtypep                    
       stream-error-stream        svref                       
       stream-external-format     sxhash                      
       streamp                    symbol                      
       string                     symbol-function             
       string-capitalize          symbol-macrolet             
       string-downcase            symbol-name                 
       string-equal               symbol-package              
       string-greaterp            symbol-plist                
       string-left-trim           symbol-value                
       string-lessp               symbolp                     
       string-not-equal           synonym-stream              
       string-not-greaterp        synonym-stream-symbol       
       string-not-lessp           t                           
       string-right-trim          tagbody                     
       string-stream              tailp                       
       string-trim                tan                         
       string-upcase              tanh                        
       string/=                   tenth                       
       string<                    terpri                      
       string<=                   the                         
       string=                    third                       
       string>                    throw                       
       string>=                   time                        
       stringp                    trace                       
       structure                  translate-logical-pathname  
       structure-class            translate-pathname          
       structure-object           tree-equal                  
       style-warning              truename                    
       
       truncate                             values-list               
       two-way-stream                       variable                  
       two-way-stream-input-stream          vector                    
       two-way-stream-output-stream         vector-pop                
       type                                 vector-push               
       type-error                           vector-push-extend        
       type-error-datum                     vectorp                   
       type-error-expected-type             warn                      
       type-of                              warning                   
       typecase                             when                      
       typep                                wild-pathname-p           
       unbound-slot                         with-accessors            
       unbound-slot-instance                with-compilation-unit     
       unbound-variable                     with-condition-restarts   
       undefined-function                   with-hash-table-iterator  
       unexport                             with-input-from-string    
       unintern                             with-open-file            
       union                                with-open-stream          
       unless                               with-output-to-string     
       unread-char                          with-package-iterator     
       unsigned-byte                        with-simple-restart       
       untrace                              with-slots                
       unuse-package                        with-standard-io-syntax   
       unwind-protect                       write                     
       update-instance-for-different-class  write-byte                
       update-instance-for-redefined-class  write-char                
       upgraded-array-element-type          write-line                
       upgraded-complex-part-type           write-sequence            
       upper-case-p                         write-string              
       use-package                          write-to-string           
       use-value                            y-or-n-p                  
       user-homedir-pathname                yes-or-no-p               
       values                               zerop))

(in-package :si)


;FIXME bootstrap code

(fset 'intersection #'intersection-eq)
(fset 'union #'union-eq)
(fset 'set-difference #'set-difference-eq)
(fset 'nunion #'nunion-eq)

(*make-constant '+array-types+ (si::aelttype-list))
(*make-constant '+sfix+ (eql (truncate fixnum-length char-length) 4))


(defun num-comp (x y tp) 
  (if (c-fixnum-== tp 1) (c-fixnum-== x y)
    (if (c-fixnum-== tp 2) (eql 0 (gmp::mpz_cmp x y))
      (if (c-fixnum-== tp 3) (and (eql (numerator x) (numerator y))
				  (eql (denominator x) (denominator y)))
	(if (c-fixnum-== tp 4) (c-float-== x y)
	  (if (c-fixnum-== tp 5) (c-double-== x y)
	    (if (c-fixnum-== tp 6) (and (eql (realpart x) (realpart y)) (eql (imagpart x) (imagpart y)))
	      (if (c-fixnum-== tp 7) (c-fcomplex-== x y)
		(if (c-fixnum-== tp 8) (c-dcomplex-== x y))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_export.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_sloop.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;;; -*- Mode:LISP; Package:(SLOOP LISP);Syntax:COMMON-LISP;Base:10 -*- ;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1985,86 by William Schelter,                     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Report bugs to wfs@carl.ma.utexas.edu
;;; It comes with ABSOLUTELY NO WARRANTY but we hope it is useful.

 
;;; The following code is meant to run in COMMON LISP and to provide
;;; extensive iteration facilities, with very high backwards compatibility
;;; with the traditional loop macro. It is meant to be publicly available!
;;; Anyone is hereby given permission to copy it provided he does not make
;;; ANY changes to the file unless he is William Schelter.  He may change
;;; the behavior after loading it by resetting the global variables such
;;; as like *Use-locatives*, *automatic-declarations*,..  listed at the
;;; beginning of this file.
 
;;; The original of this file is on
;;; rascal.ics.utexas.edu:/usr2/ftp/pub/sloop.lisp.   I am happy to accept
;;; suggestions for different defaults for various implementations, or for
;;; improvements.


;;If you want to redefine the common lisp loop you may include in your code:
;;; (defmacro loop (&body body) (parse-loop body))

;;         Principal New Features

;;; Sloop is extremely user extensible so that you may easily redefine
;;; most behavior, or add additional collections, and paths.  There are a
;;; number of such examples defined in this file, including such
;;; constructs as

;;; .. FOR v IN-FRINGE x ..         (iterate through the fringe of a tree x)
;;; .. SUM v ..                     (add the v)
;;; .. AVERAGING v ..      
;;; .. FOR sym IN-PACKAGE y         (iterate through symbols in a package y)
;;; .. COLLATE v ..                 (for collecting X into an ordered list),
;;; .. FOR (elt i) IN-ARRAY ar      (iterate through array ar, with index i)
;;; .. FOR (key elt) IN-TABLE foo.. (if foo is a hash table)

;;; you can combine any collection method with any path.
;;; Also there is iteration over products so that you may write
;;; (SLOOP FOR i BELOW k
;;;       SLOOP (FOR j BELOW i
;;;          	    COLLECTING (foo i j)))

;;; Declare is fully supported.  The syntax would be
;;; (sloop for u in l with v = 0
;;;       declare (fixnum u v)
;;;       do ....

;;; This extensibility is gained by the ability to define a "loop-macro",
;;; which plays a role analagous to an ordiary lisp macro.  See eg.
;;; definitions near that of "averaging".  Essentially a "loop-macro"
;;; takes some arguments (supplied from the body of the loop following its
;;; occurrence, and returns a new form to be stuffed onto the front of the
;;; loop form, in place of it and its arguments).
 
;;; Compile notes: For dec-20 clisp load the lisp file before compiling.


;;; there seems to be no unanimity about what in-package etc. does on
;;; loading and compiling a file.  The following is as close to the
;;; examples in the Common Lisp manual, as we could make it.  The user
;;; should put (require "SLOOP") and then (use-package "SLOOP") early in
;;; his init file.  Note use of the string to avoid interning 'sloop in
;;; some other package.


(in-package :SLOOP  :use '(LISP))  
(eval-when (compile eval load)

(export '(loop-return sloop def-loop-collect def-loop-map
		      def-loop-for def-loop-macro local-finish
		      sloop-finish) (find-package "SLOOP"))

)

;;; some variables that may be changed to suit different implementations:

(eval-when (compile load eval)

(defvar *use-locatives* nil "See sloop.lisp")   ;#+lispm t #-lispm nil 
;;; If t should have locf, such that if we do
;;;   (setf b nil) (setq a (locf b))
;;;    then the command
;;;   (setf (cdr a) (cons 3 nil)) means that b==>(3).
;;; This is useful for building lists starting with a variable pointing to
;;; nil, since otherwise we must check each time if the list has really
;;; been started, before we do a (setf (cdr b) ..)

(defvar *Automatic-declarations*  #+lispm nil  #-lispm
  '(:from fixnum) "See sloop.lisp")

;;; some other reasonable ones would be :count fixnum :max fixnum
;;; Automatic declarations for variables in the stepping and collecting,
;;; so for i below n, gives i and n a :from declaration (here fixnum)


;;valid keys in *automatic-declarations*
(defvar *auto-type* '(:from :in :collect))
;;give automatic register declaration to these variables 
(defvar *auto-register* '(:from :in :collect))
(eval-when (compile eval load)
(proclaim '(declaration :register))
)


(defvar *type-check* t "If t adds a type check on bounds of from loop
if there is and automatic declare")

(defvar *macroexpand-hook-for-no-copy* #-(or lmi ti) 'funcall #+(or lmi ti) t)
;;; some lisps remember a macro so that (loop-return) will expand eq forms
;;; always in the same manner, even if the form is in a macrolet! To
;;; defeat this feature we copy all macro expansions unless
;;; *macro-expand-hook* = *macroexpand-hook-for-no-copy*
)


;;; *****ONLY CONDITIONALIZATIONS BELOW HERE SHOULD BE FOR BUG FIXES******
;;; eg. some kcls don't return nil from a prog by default!

;;; all macros here in here.
(eval-when (compile eval load)

(defparameter *sloop-translations* '((appending . append)
			 ((collecting collect) . collect)
			 ((maximizing maximize) . maximize)
			 ((minimizing minimize) . minimize)
			 (nconcing . nconc)
			 ((count counting) . count)
			 (summing . sum)
			 (if . when)
			 (as . for)
			 (in-fringe . in-fringe)
			 (collate . collate)
			 (in-table . in-table)
			 (in-carefully . in-carefully)
			 (averaging . averaging)
			 (repeat . repeat)
			 (first-use . first-use)
			 (in-array . in-array))
  "A list of cons's where the translation is the cdr, and the car
is a list of names or name to be translated.  Essentially allows 'globalizing'
a symbol for the purposes of being a keyword in a sloop")


(defparameter *additional-collections* nil)

(defmacro lcase (item &body body)
  (let (bod last-case tem)
    (do ((rest body (cdr rest)) (v))
	((or last-case (null rest)))
      (setq  v (car rest))
      (push
	(cond ((eql (car v) t) (setq last-case t) v)
	      ((eql (car v) :collect)
	       `((loop-collect-keyword-p .item.) ,@ (cdr v)))
	      ((eql (car v) :no-body)
	       `((parse-no-body  .item.) ,@ (cdr v)))
	      ((setq tem
		     (member (car v) '(sloop-macro sloop-for sloop-map)))
	       `((and (symbolp .item.)(get .item. ',(car tem))) ,@ (cdr v)))
	      (t
	       `((l-equal .item. ',(car v)) ,@ (cdr v))))
	bod))
     (or last-case (push `(t (error "lcase fell off end ~a  " .item.)) bod))
    `(let ((.item. (translate-name ,item)))
       (cond ,@ (nreverse bod)))))

(defun desetq1 (form val)
  (cond ((symbolp form)
	 (and form `(setf ,form ,val)))
	((consp form)
	 `(progn ,(desetq1 (car form) `(car ,val))
		 ,@ (if (consp (cdr form))
			(list(desetq1 (cdr form) `(cdr ,val)))
		      (and (cdr form) `((setf ,(cdr form) (cdr ,val)))))))
	(t (error ""))))

(defmacro desetq (form val)
  (cond ((atom val) (desetq1 form val))
	(t (let ((value (gensym)))
	     `(let ((,value ,val)) , (desetq1 form value))))))

(defmacro loop-return (&rest vals)
  (cond ((<=  (length vals) 1)
	 `(return ,@ vals))
	(t`(return (values  ,@ vals)))))

(defmacro sloop-finish ()
  `(go finish-loop))

(defmacro local-finish ()
  `(go finish-loop))

(defmacro sloop (&body body)
  (parse-loop body))
  
(defmacro def-loop-map (name args &body body)
  (def-loop-internal name args body 'map))
(defmacro def-loop-for (name args &body body )
  (def-loop-internal name args body 'for nil 1))
(defmacro def-loop-macro (name args &body body)
  (def-loop-internal name args body 'macro))
(defmacro def-loop-collect (name arglist &body body )
       "Define function of 2 args arglist= (collect-var value-to-collect)"
  (def-loop-internal name arglist body 'collect '*additional-collections* 2 2))

(defmacro sloop-swap ()
 `(progn (rotatef a *loop-bindings*)
  (rotatef b  *loop-prologue*)
  (rotatef c *loop-epilogue*)
  (rotatef e *loop-end-test*)
  (rotatef f *loop-increment*)
  (setf *inner-sloop* (not *inner-sloop*))
  ))

) ;;end of macros

(defun l-equal (a b)
  (and (symbolp a)
       (cond ((symbolp b)
	      (equal (symbol-name a) (symbol-name b)))
	     ((listp b)
	      (member  a b :test 'l-equal)))))

(defun loop-collect-keyword-p (command)
  (or (member command '(collect append nconc sum count) :test 'l-equal)
      (find command *additional-collections* :test 'l-equal)))
 			 
(defun translate-name (name)
  (cond ((and (symbolp name)
	      (cdar (member name *sloop-translations*
			    :test 'l-equal :key 'car))))
	(t name)))

(defun loop-pop ()
  (declare (special *last-val* *loop-form*))
  (cond (*loop-form*
          (setq *last-val* (pop *loop-form*)))
	(t (setq *last-val* 'empty-form) nil)))

(defun loop-un-pop ()  (declare (special *last-val* *loop-form*))
  (case *last-val*
	(empty-form nil)
	(already-un-popped (error "you are un-popping without popping"))
	(t  (push *last-val* *loop-form*)
	    (setf *last-val* 'alread-un-popped))))

(defun loop-peek () (declare (special *last-val* *loop-form*))
   (car *loop-form*))

(defun loop-let-bindings(binds)
  (do ((v (car binds) (cdr v)))
      ((null v) (nreverse (car binds)))
      (or (cdar v) (setf (car v) (caar v)))))

(defun parse-loop (form &aux inner-body)
  (let ((*loop-form* form)
	(*Automatic-declarations* *Automatic-declarations*)
	*last-val* *loop-map* 
	*loop-body* 
	*loop-name*
	*loop-prologue* *inner-sloop*
	*loop-epilogue* *loop-increment*
	*loop-collect-pointers*  *loop-map-declares*
	*loop-collect-var* 	*no-declare*
	*loop-end-test*
	*loop-bindings*
	*product-for*
	*type-test-limit*
	local-macros
	(finish-loop 'finish-loop)
	)
    (declare (special *loop-form* *last-val* *loop-map* 
		      *loop-collect-pointers*
		      *loop-name* *inner-sloop*
		      *loop-body*
		      *loop-prologue* 
		      *no-declare*
		      *loop-bindings*
		      *loop-collect-var*  *loop-map-declares*
		      *loop-epilogue* *loop-increment*
		      *loop-end-test* *product-for*
		      *type-test-limit*
		      ))
    (unless (and (symbolp (car *loop-form*))  (car *loop-form*))
	    (push 'do  *loop-form*))	;compatible with common lisp loop..
    (parse-loop1)
    (when (or *loop-map* *product-for*)
	  (or *loop-name* (setf *loop-name* (gensym "SLOOP")))
	  (and (eql 'finish-loop finish-loop)
	       (setf finish-loop (gensym "FINISH"))))
;;; some one might use local-finish,local-return or sloop-finish, so they might
;;; be bound at an outer level.  WE have to always include this since
;;; loop-return may be being bound outside.
    (and				; *loop-name*
      (push 
	`(loop-return (&rest vals)
		      `(return-from ,',*loop-name* (values ,@ vals)))
	local-macros))
    (when  t;; (or (> *loop-level* 1) (not (eql finish-loop 'finish-loop)))
	   (push 	 `(sloop-finish () `(go ,',finish-loop)) local-macros)
	   (push 	 `(local-finish () `(go ,',finish-loop)) local-macros))
    (and *loop-collect-var*
	 (push   `(return-from ,*loop-name* , *loop-collect-var*)
		 *loop-epilogue*))
    (setq inner-body (append  *loop-end-test*
			      (nreverse *loop-body*)
			      (nreverse	*loop-increment*)))
    (cond (*loop-map*
	    (setq inner-body (substitute-sloop-body inner-body)))
	  (t (setf inner-body (cons 'next-loop
				    (append inner-body '((go next-loop)))))))
    (let ((bod 
	    `(macrolet ,local-macros
		       (block ,*loop-name*
			      (tagbody
				,@ (append
				     (nreverse *loop-prologue*)
				     inner-body
				     `(,finish-loop)
				     (nreverse *loop-epilogue*)
				     #+kcl '((loop-return  nil))))))
	    
	    ))
;;; temp-fix..should not be necessary but some lisps cache macro
;;; expansions.  and ignore the macrolet!!
      (unless  (eql *macroexpand-hook* *macroexpand-hook-for-no-copy*)
	       (setf bod (copy-tree bod)))
      (dolist (v *loop-bindings*)
	      (setf bod
		    `(let ,(loop-let-bindings v)
		       ,@(and (cdr v) `(,(cons 'declare (cdr v))))
		       ,bod)))
      bod
      ))) 

(defun parse-loop1 ()
  (declare (special *loop-form*
		    *loop-body* *loop-increment*
		    *no-declare* *loop-end-test*
		    *loop-name* ))
  (lcase (loop-peek)
	 (named (loop-pop) (setq *loop-name* (loop-pop)))
	 (t nil))
  (do ((v (loop-pop) (loop-pop)))
      ((and (null v) (null *loop-form*)))
      (lcase v
	     (:no-body)
	     (for (parse-loop-for))
	     (while (push
		      `(or ,(loop-pop) (local-finish))  *loop-body*))
	     (until (push
		      `(and ,(loop-pop) (local-finish))  *loop-body*))
	     (do (setq *loop-body* (append (parse-loop-do) *loop-body*)))
	     ((when unless) (setq *loop-body*
				  (append (parse-loop-when) *loop-body*)))
	     (:collect  (setq *loop-body*
			      (append (parse-loop-collect) *loop-body*)))
	     )))


(defun parse-no-body (com &aux (found t) (first t))
  "Reads successive no-body-contribution type forms, like declare,
initially, etc.  which can occur anywhere. Returns t if it finds some
otherwise nil"
  (declare (special *loop-form*
		    *loop-body*
		    *loop-increment*
		    *no-declare* *loop-end-test*
		    *loop-name* ))
  (do ((v com (loop-pop)))
      ((null (or first *loop-form*)))
      (lcase v
	     ((initially finally)(parse-loop-initially v))
	     (nil nil)
	     (with      (parse-loop-with))
	     (declare   (parse-loop-declare (loop-pop) t))
	     (nodeclare  (setq *no-declare* (loop-pop)))
	     ;take argument to be consistent.
	     (increment (setq *loop-increment*
			      (append (parse-loop-do) *loop-increment*)))
	     (end-test  (setq *loop-end-test*
			      (append (parse-loop-do) *loop-end-test*)))
	     (with-unique (parse-loop-with nil t))
	     (sloop-macro (parse-loop-macro v 'sloop-macro))
	     (t
	       (cond (first
		       (setf found nil))
		     (t (loop-un-pop)))
	       (return 'done)))
      (setf first nil))
  found)

(defun parse-loop-with (&optional and-with only-if-not-there)
  (let ((var  (loop-pop)))
    (lcase (loop-peek)
      (= (loop-pop)
	 (or (symbolp var) (error "Not a variable ~a" var))
	 (loop-add-binding var (loop-pop)
			   (not and-with) nil nil t only-if-not-there))
      (t (loop-add-temps var nil nil (not and-with) only-if-not-there)))
    (lcase (loop-peek)
      (and (loop-pop)
	   (lcase (loop-pop)
	     (with (parse-loop-with t ))
	     (with-unique (parse-loop-with t t))
	     (t (loop-un-pop) (parse-loop-with t))
	     ))
      (t nil))))

(defun parse-loop-do (&aux result)
  (declare (special *loop-form*))
  (do ((v (loop-pop) (loop-pop)) )
      (())
    (cond
      ((listp v)
       (push v result)
       (or *loop-form* (return 'done)))
      (t (loop-un-pop) (return 'done))))
  (or result (error "empty clause"))
  result)
  
(defun parse-loop-initially (command )
  (declare (special *loop-prologue* *loop-epilogue* *loop-bindings*))
  (lcase
    command
    (initially
      (let ((form (parse-loop-do)))
	(dolist (v (nreverse form))
		(cond ((and (listp v)
			    (member (car v) '(setf setq))
			    (eql (length v) 3)
			    (symbolp   (second v))
			    (constantp (third v))
			    (assoc (second v) (caar *loop-bindings*))
			    (loop-add-binding (second v) (third v)
					      nil nil nil t t)
			    ))
		      (t (setf *loop-prologue*
			       (cons v *loop-prologue*)))))))
    (finally
      (setf *loop-epilogue* (append (parse-loop-do) *loop-epilogue*)))))

(defun parse-one-when-clause ( &aux this-case  (want 'body) v)
  (declare (special *loop-form*))
  (prog
    nil
    next-loop
    (and (null *loop-form*) (return 'done))
    (setq v (loop-pop))
    (lcase v
	   (:no-body)
	   (:collect (or (eql 'body want) (go finish))
		     (setq this-case (append  (parse-loop-collect) this-case))
		     (setq want 'and))
	   (when  (or (eql 'body want) (go finish))
		  (setq this-case (append   (parse-loop-when) this-case))
		  (setq want 'and))
	   (do    (or (eql 'body want) (go finish))
		  (setq this-case (append   (parse-loop-do) this-case))
		  (setq want 'and))
	   (and    (or (eql 'and  want) (error "Premature AND"))
		   (setq want 'body))
	   (t  (loop-un-pop)(return 'done)))
    (go next-loop)
    finish
    (loop-un-pop))
  (or this-case (error "Hanging conditional"))
  this-case)


(defun parse-loop-when (&aux initial else else-clause)
  (declare (special *last-val* ))
  (let ((test (cond ((l-equal *last-val* 'unless) `(not , (loop-pop)))
		    (t (loop-pop)))))
    (setq initial (parse-one-when-clause))
    (lcase (loop-peek)
	   (else
	     (loop-pop)
	     (setq else t)
	     (setq else-clause (parse-one-when-clause)))
	   (t nil))
    `((cond (,test ,@ (nreverse initial))
	    ,@ (and else `((t ,@ (nreverse else-clause))))))))

(defun pointer-for-collect (collect-var)
  (declare (special *loop-collect-pointers*))
  (or (cdr (assoc collect-var *loop-collect-pointers*))
      (let ((sym(loop-add-binding (gensym "POIN") nil nil :collect )))
	(push (cons collect-var sym)
	      *loop-collect-pointers*)
	sym)))

(defun parse-loop-collect ( &aux collect-var pointer name-val)
  (declare (special *last-val* *loop-body* *loop-collect-var*
		    *loop-collect-pointers* *inner-sloop*
		    *loop-prologue* ))
  (and *inner-sloop* (throw 'collect nil))
  (let ((command   *last-val*)
	(val (loop-pop)))
    (lcase
      (loop-pop)
      (into (loop-add-binding (setq collect-var (loop-pop)) nil nil t nil t ))
      (t (loop-un-pop)
	 (cond (*loop-collect-var* (setf collect-var *loop-collect-var*))
	       (t  (setf collect-var
			 (setf *loop-collect-var*
			       (loop-add-binding (gensym "COLL") nil )))))))
    (lcase command
	   ((append nconc collect)
	    (setf pointer (pointer-for-collect collect-var))
	    (cond (*use-locatives*
		    (pushnew `(setf ,pointer
				    (locf ,collect-var))
			     *loop-prologue* :test 'equal)))
	    (lcase command
		   ( append
		      (unless (and (listp val) (eql (car val) 'list))
			      (setf val `(copy-list ,val))))
		   (t nil)))
	   (t nil))
    (cond ((and  (listp val) (not *use-locatives*))
	   (setq name-val (loop-add-binding (gensym "VAL") nil nil)))
	  (t (setf name-val val)))
    (let
	((result
	   (lcase
	     command
	     ((nconc append)
	      (let ((set-pointer
		      `(and (setf (cdr ,pointer) ,name-val)
			    (setf ,pointer (last (cdr ,pointer))))))
		(cond (*use-locatives*
			(list set-pointer))
		      (t
			`((cond (,pointer ,set-pointer)
				(t (setf ,pointer
					 (last (setf
						 ,collect-var
						 ,name-val))))))))))
	     (collect
	       (cond (*use-locatives*
		       `((setf (cdr ,pointer)
			       (setf ,pointer (cons ,name-val nil)))))
		     (t `((cond (,pointer
				   (setf (cdr ,pointer)
					 (setf ,pointer (cons ,name-val nil))))
				(t (setf ,collect-var
					 (setf ,pointer
					       (cons ,name-val nil)))))))))
	     (t (setq command (translate-name command))
		(cond ((find command *additional-collections* :test 'l-equal)
		       (loop-parse-additional-collections
			 command collect-var name-val))
		      (t (error "loop fell off end ~a" command)))))))
      (cond ((eql name-val val)
	     result)
	    (t (nconc result `((setf ,name-val ,val) )))))))

(defun loop-parse-additional-collections
  (command collect-var name-val &aux eachtime)
  (declare (special *loop-prologue* *last-val*
		    *loop-collect-var* *loop-epilogue* ))
  (let* ((com  (find command *additional-collections* :test 'l-equal))
	 (helper (get com 'sloop-collect)))
    (let ((form (funcall helper collect-var name-val)))
      (let ((*loop-form* form) *last-val*)
	(declare (special  *loop-form* *last-val*))
	(do ((v (loop-pop) (loop-pop)))
	    ((null *loop-form*))
	    (lcase v
		   (:no-body)
		   (do (setq eachtime (parse-loop-do)))))
	eachtime))))

(defun the-type (symbol type)
  (declare (special *no-declare*))
  (and *no-declare* (setf type nil))
  (and type (setf type (or (getf *Automatic-declarations* type)
			   (and  (not (keywordp type)) type))))
  (and (consp type) (eq (car type) 'type) (setf type (second  type)))
  (cond (type (list 'the type symbol ))
	(t symbol)))

(defun sloop-type-error ()
  (error "While checking a bound of a sloop, I found the wrong type 
for something in sloop::*automatic-declarations*.
    Perhaps your limit is wrong? 
If not either use nodeclare t or set sloop::*automatic-declarations* to nil. 
recompile."))


;;; this puts down code to check that automatic declarations induced by
;;; :from are indeed valid!  It checks both ends of the interval, and so
;;; need not check the numbers in between.

(defun make-value (value type-key &aux type )
  (declare (special *no-declare* *type-test-limit*))
  (cond ((and
	  (not *no-declare*)
	  *type-check*
	  (eq type-key :from)
	  (setq type (getf  *Automatic-declarations* type-key)))
	  (setq type
	       (cond ((and (consp type)
			   (eq (car type) 'type))
		      (second type))
		     (t type)))
	 (cond ((constantp value)
		(let ((test-value
		       (cond (*type-test-limit*
			      (eval (subst value
					   'the-value *type-test-limit*)))
			     (t (eval value)))))
		(or (typep test-value type)
		    (error
		     "~&Sloop found the type of ~a was not type ~a,~%~
                      Maybe you want to insert SLOOP NODECLARE T ..."
		     value
		     type))
		(list value)))
	       (t  (let (chk)
		     `((let ,(cond ((atom value)
				    nil)
				   (t `((,(setq chk(gensym)) ,value))))
			 (or
			  (typep
			   ,(if *type-test-limit*
				(subst (or chk value)
				       'the-value *type-test-limit*)
			      (or chk value))
			   ',type)
			  (sloop-type-error))
			 ,(or chk value)))))))
	(t (list value))))


;;; keep track of the bindings in a list *loop-bindings* each element of
;;; the list will give rise to a different let.  the car will be the
;;; variable bindings, the cdr the declarations.


(defun loop-add-binding
       (variable value &optional (new-level t) type force-type (force-new-value t)
			 only-if-not-there &aux tem)
;;; Add a variable binding to the current or new level.  If FORCE-TYPE,
;;; ignore a *no-declare*.  If ONLY-IF-NOT-THERE, check all levels.
  (declare (special *loop-bindings*))
  (when  (or new-level (null *loop-bindings*))
	 (push (cons nil nil) *loop-bindings*))
  (cond ((setq tem (assoc variable (caar  *loop-bindings*) ))
	 (and force-new-value
	      (setf (cdr tem) (and value (make-value value type)))))
	((and (or only-if-not-there (and (null (symbol-package variable))
					 (constantp value)))
	      (dolist (v (cdr *loop-bindings*))
		(cond ((setq tem (assoc variable (car v)))
		       (and force-new-value
			    (setf (cdr tem)
				  (and value (make-value value type))))
		       (return t))))))
	(t (push (cons variable  (and value (make-value value type)))
		 (caar *loop-bindings*))))
  (and type (loop-declare-binding variable type force-type))
  variable)

;(defmacro nth-level (n) `(nth ,n *loop-bindings*))
;if x = (nth i *loop-bindings*)
;(defmacro binding-declares (x) `(cdr ,x)) ;(cons 'declare (binding-declares x)) to get honest declare statement
;(defmacro binding-values (x) `(car ,x))  ;(let (binding-values x) ) to get let.

(defun loop-declare-binding (var type force-type &optional odd-type
				 &aux found )
  (declare (special *loop-bindings* *automatic-declarations*
		    *no-declare* *loop-map*))
  odd-type ;;ignored
  (and type
       (member type *auto-type*)
       (setf type (getf  *automatic-declarations* type))
       *auto-register*
       (loop-declare-binding var :register force-type))
  (when (and type(or force-type (null *no-declare*)))
    (dolist (v *loop-bindings*)
      (cond ((assoc var (car v)) (setf found t)
	     (pushnew
	       (if (and (consp type)
			(eq (car type) 'type))
		   (list 'type (second type) var)
		   (if odd-type (list 'type type var)
		       
		   (list type var)))
	       (cdr v) :test 'equal)
	     (return 'done)
	     )))
    (or found *loop-map* (error "Could not find variable ~a in bindings" var)))
  var)

(defun parse-loop-declare (&optional (decl-list (loop-pop)) (force t))
  (let ((type (car decl-list)) odd-type)
    (cond ((eq type 'type)
	   (setf decl-list (cdr decl-list) type (car decl-list) odd-type t)))
    (dolist (v (cdr decl-list))
      (loop-declare-binding v (car decl-list) force odd-type))))
	
(defun loop-add-temps (form &optional val type new-level only-if-not-there)
  (cond ((null form))
	((symbolp form)
	 (loop-add-binding form val new-level type nil  t only-if-not-there))
	((listp form)
	 (loop-add-temps (car form))
	 (loop-add-temps (cdr form)))))


(defun add-from-data (data &rest args)
   "rest = var begin end  incr direction or-eql"
   (or data (setq data (copy-list '(nil 0 nil 1 + nil))))
   (do ((l data (cdr l))
        (v args (cdr v)))
      ((null v) l)
     (and (car v) (setf (car l) (car v))))
   data)

(defun parse-loop-for ( &aux  inc  from-data)
  (declare (special *loop-form*  *loop-map-declares*  *loop-map* 
		    *loop-body* *loop-increment* *no-declare*
		    *loop-prologue*
		    *loop-epilogue*
		    *loop-end-test*
		    *loop-bindings*
		    ))
  (let* ((var (loop-pop)) test incr)
    (do ((v (loop-pop) (loop-pop)))
	(())
	(lcase v
	       (in (let ((lis (gensym "LIS")))
		     (loop-add-temps var nil :in t)
		     (loop-add-binding lis (loop-pop) nil)
		     (push  `(desetq ,var (car ,lis)) *loop-body*)
		     (setf incr `(setf ,lis (cdr ,lis)))
		     (setq test   `(null ,lis) )
		     ))
	       (on (let ((lis
			   (cond ((symbolp var) var)
				 (t (gensym "LIS")))))
		     (loop-add-temps var nil :in t)
		     (loop-add-binding lis (loop-pop) nil)
		     (setf incr `(setf ,lis (cdr ,lis)))
		     (unless (eql lis var)
			     (push `(desetq ,var ,lis) *loop-body*))
		     (setf test `(null ,lis))))
        
	       ((upfrom from)
		(setq from-data (add-from-data from-data
					       var (loop-pop) nil  nil '+)))
	       (downfrom
		 (setq from-data  (add-from-data
				    from-data var (loop-pop) nil  nil '-)))
	       (by
		 (setq inc (loop-pop))
		 (cond (from-data
			 (setq from-data (add-from-data
					   from-data nil nil nil inc)))
		       (t (assert (eq (car (third incr)) 'cdr))
			  (setq incr
				`(setf ,(second incr)
				       ,(if (and (consp inc)
						(member (car inc) '(quote function)))
					  `(,(second inc) ,(second incr))
					  `(funcall
					    ,inc ,(second incr))))))))
	       (below
		 (setq from-data (add-from-data from-data
						var nil (loop-pop) nil '+)))
	       (above
		 (setq from-data (add-from-data from-data
						var nil (loop-pop) nil '-)))
	       (to
		 (setq from-data (add-from-data from-data
						var nil (loop-pop) nil nil t)))
	       (sloop-for (parse-loop-macro (translate-name v)
					     'sloop-for var )
			   (return 'done))
	       (sloop-map (parse-loop-map (translate-name v) var )
			   (return nil))
	       (t(or			
		   (loop-un-pop))
		 (return 'done))))
    
    ;;whew finished parsing a for clause..
    
    (cond (from-data
	    (let
		((op (nth 4 from-data))
		 (or-eql (nth 5 from-data))
		 (var (car from-data))
		 (end (third from-data))
		 (inc (fourth from-data))
		 type)
	      (loop-add-binding var (second from-data) t :from)
	      (or (constantp inc) (setq *no-declare* t))
	      (setf incr `(setf ,var ,(the-type `(,op  ,var ,inc) :from)))
	      (cond (end
		      (let ((lim (gensym "LIM"))
			    (*type-test-limit*
			      (cond ((and (eql inc 1)
					  (null (nth 5 from-data)))
				     nil)
				    (t `(,op
					   the-value , inc)))))
			(declare (special *type-test-limit*))
			(loop-add-binding lim end nil :from nil nil)
			(setq test `(,(cond (or-eql
					      (if (eq op '+) '> '<))
					    (t (if (eq op '+) '>= '<=)))
				     ,var ,lim))))
		    ((and (not *no-declare*)
			  *type-check*
			  (setq type (getf *automatic-declarations* :from))
			  (progn (if (and (consp type)(eq (car type) 'type))
				     (setf type      (second type)))
				 (subtypep type 'fixnum)))
		     (or (constantp inc) (error "increment must be constant."))
		     (push
		       `(or
			  ,(cond ((eq op '+)
				  `(< ,var ,(- most-positive-fixnum
					       (or inc 1))))
				 (t `(> ,var  ,(+ most-negative-fixnum
						  (or inc 1)))))
			  (sloop-type-error))
		       *loop-increment*)
		     )))))
    
    (and test (push (copy-tree `(and ,test (local-finish))) *loop-end-test*))
    (and incr (push incr *loop-increment*))
    ))


(defun parse-loop-macro (v type &optional initial &aux result)
  (declare (special *loop-form*))
  (let ((helper (get v type)) args)
    (setq args
	  (ecase type
	    (sloop-for
	     (let ((tem (get v 'sloop-for-args)))
	       (or (cdr tem) (error "sloop-for macro needs at least one arg"))
	       (cdr tem)))
	    (sloop-macro(get v 'sloop-macro-args))))
    (let ((last-helper-apply-arg
	    (cond ((member '&rest args)
		   (prog1 *loop-form* (setf *loop-form* nil)))
		  (t (dotimes (i (length args) (nreverse result))
			     (push (car *loop-form*) result)
			     (setf *loop-form* (cdr *loop-form*)))))))
      (setq *loop-form*
	    (append 
	      (case type
		    (sloop-for (apply helper initial last-helper-apply-arg))
		    (sloop-macro(apply helper  last-helper-apply-arg)))
	      *loop-form*)))))

(defun parse-loop-map (v var)
  (declare (special *loop-map* *loop-map-declares* *loop-form*))
  (and *loop-map* (error "Sorry only one allowed loop-map per sloop"))
  (let ((helper (get v 'sloop-map))
	(args  (get v 'sloop-map-args)))
    (or args (error "map needs one arg before the key word"))
    (cond ((member '&rest args)
	   (error "Build this in two steps if you want &rest")))
    (let* (result
	    (last-helper-apply-arg
	      (dotimes (i (1- (length args)) (nreverse result))
		       (push (car *loop-form*) result)
		       (setf *loop-form* (cdr *loop-form*)))))
      (setq *loop-map-declares*
	    (do ((v (loop-pop)(loop-pop)) (result))
		((null (l-equal v 'declare))
		 (loop-un-pop)
		 (and result (cons 'declare result)))
		(push (loop-pop) result)))
      (setq *loop-map* (apply helper var last-helper-apply-arg))
      nil)))

(defun substitute-sloop-body (inner-body)
  (declare (special *loop-map* *loop-map-declares*))
    (cond (*loop-map*
	   (setf inner-body (list  (subst (cons 'progn inner-body)
					  :sloop-body *loop-map*)))
	   (and *loop-map-declares*
		(setf inner-body(subst *loop-map-declares*
				       :sloop-map-declares inner-body)))))
  inner-body)

;;; **User Extensible Iteration Facility**

(eval-when (compile eval load)
(defun def-loop-internal (name args  body type
			       &optional list min-args max-args
			       &aux (*print-case* :upcase)
			       (helper (intern
				  (format nil "~a-SLOOP-~a" name type))))
  (and min-args (or (>= (length args) min-args)(error "need more args")))
  (and max-args (or (<= (length args) max-args)(error "need less args")))
  `(eval-when (load compile eval)
	      (defun ,helper ,args
		,@ body)
	      ,@ (and list `((pushnew ',name ,list)))
	      (setf (get ',name ',(intern (format nil "SLOOP-~a" type)
					 (find-package 'sloop))) ',helper)
	      (setf (get ',name ',(intern (format nil "SLOOP-~a-ARGS" type)
					 (find-package 'sloop))) ',args)))
)
		

;;; DEF-LOOP-COLLECT lets you get a handle on the collection var.  exactly
;;; two args.  First arg=collection-variable. Second arg=value this time
;;; thru the loop.

(def-loop-collect sum (ans val)
  `(initially (setq ,ans 0)
    do (setq ,ans (+ ,ans ,val))))
(def-loop-collect logxor (ans val)
  `(initially (setf ,ans 0)
  do (setf ,ans (logxor ,ans ,val))
  declare (fixnum ,ans ,val)))
(def-loop-collect maximize (ans val)
  `(initially (setq ,ans nil) 
  do (if ,ans (setf ,ans (max ,ans ,val)) (setf ,ans ,val))))

(def-loop-collect minimize (ans val) 
  `(initially (setq ,ans nil)
  do (if ,ans (setf ,ans (min ,ans ,val)) (setf ,ans ,val))))

(def-loop-collect count (ans val)
  `(initially (setq ,ans 0)
  do (and ,val (setf ,ans (1+ ,ans)))))

(def-loop-collect thereis (ans val)(declare(ignore ans))
  `(do (if ,val (loop-return ,val))))
(def-loop-collect always (ans val)
  `(initially (setq ,ans t) do (and (null ,val)(loop-return nil))))
(def-loop-collect never (ans val)
  `(initially (setq ,ans t) do (and  ,val  (loop-return nil))))
 

;;; DEF-LOOP-MACRO
;;; If we have done
;;;  (def-loop-macro averaging (x)
;;;    `(sum ,x into .tot. and count t into .how-many.
;;;	   finally (loop-return (/ .tot. (float .how-many.)))))

;;; (def-loop-collect average (ans val)
;;;   `(initially (setf ,ans 0.0)
;;;     with-unique .how-many. = 0
;;;     do (setf ,ans (/  (+ (* .how-many. ,ans) ,val) (incf .how-many.)))
;;;     ))

;;; Finally we show how to provide averaging with
;;; current value the acutal average.

(def-loop-macro averaging (x)
  `(with-unique .average. = 0.0
    and with-unique .n-to-average. = 0
    declare (float .average. ) declare (fixnum .n-to-average.)
    do (setf .average. (/
			 (+ (* .n-to-average. .average.) ,x)
			 (incf .n-to-average.)))
    finally (loop-return .average.)))

(def-loop-macro repeat (x)
  (let ((ind (gensym)))
    `(for ,ind below ,x)))

(def-loop-macro return (x)
  `(do (loop-return ,@ (if (and (consp x) (eq (car x) 'values))
			   (cdr x)
			 (list x)))))

;;; then we can write:
;;; (sloop for x in l when (oddp x) averaging x)


;;; DEF-LOOP-FOR def-loop-for and def-loop-macro are almost identical
;;; except that the def-loop-for construct can only occur after a for:

;;; (def-loop-for in-array (vars array)
;;;   (let ((elt (car vars))
;;;	 (ind (second vars)))
;;;   `(for ,ind below (length ,array) do (setf ,elt (aref ,array ,ind)))))

;;; (sloop for (elt ind) in-array ar when (oddp elt) collecting ind)

;;; You are just building something understandable by loop but minus the
;;; for.  Since this is almost like a "macro", and users may want to
;;; customize their own, the comparsion of tokens uses eq, ie. you must
;;; import IN-ARRAY to your package if you define it in another one.
;;; Actually we make a fancier in-array below which understands from, to,
;;; below, downfrom,.. and can have either (elt ind) or elt as the
;;; argument vars.

;;; DEF-LOOP-MAP A rather general iteration construct which allows you to
;;; map over things It can only occur after FOR.  There can only be one
;;; loop-map for a given loop, so you want to only use them for
;;; complicated iterations.

(def-loop-map in-table (var table)
  `(maphash #'(lambda ,var :sloop-map-declares :sloop-body) ,table))

;;; Usage  (sloop for (key elt) in-table table
;;;               declare (fixnum elt)
;;;               when (oddp elt) collecting (cons key elt))


(def-loop-map in-package (var pkg)
  `(do-symbols (,var (find-package ,pkg))  :sloop-body))

;;; Usage:
;;; (defun te()
;;;  (sloop for sym in-package 'sloop when (fboundp sym) count t)) 

;;; IN-ARRAY that understands from,downfrowm,to, below, above,etc.  I used
;;; a do for the macro iteration to be able include it here.

(def-loop-for in-array (vars array &rest args)
  (let (elt ind to)
    (cond ((listp vars) (setf elt (car vars) ind (second vars)))
	  (t (setf elt vars ind (gensym "INDEX" ))))
    (let ((skip (do ((v args (cddr v)) (result))
		    (())
		   (lcase (car v)
		       ((from downfrom) )
		       ((to below above) (setf to t))
		       (by)
		       (t (setq args (copy-list v))
			  (return (nreverse result))))
		   (push (car v) result) (push (second v) result))))
      (or to (setf skip (nconc `(below (length ,array)) skip)))
      `(for ,ind 
	,@ skip
	with ,elt 
	do (setf ,elt (aref ,array ,ind)) ,@ args))))

;;; usage: IN-ARRAY
;;; (sloop for (elt i) in-array ar from 4
;;;       when (oddp i)
;;;       collecting elt)

;;; (sloop for elt in-array ar below 10 by 2
;;;        do (print elt))

(def-loop-for = (var val)
  (lcase (loop-peek)
    (then (loop-pop) `(with ,var initially (desetq ,var ,val) increment (desetq ,var ,(loop-pop))))
    (t  `(with ,var do (desetq ,var ,val)))))

(def-loop-macro sloop (for-loop)
  (lcase (car for-loop)
    (for))
  (let (*inner-sloop* *loop-body* *loop-map* inner-body
	(finish-loop (gensym "FINISH"))
	a b c e f (*loop-form* for-loop))
    (declare (special *inner-sloop* *loop-end-test* *loop-increment*
		      *product-for* *loop-map*
		      *loop-form*  *loop-body*  *loop-prologue*
		      *loop-epilogue* *loop-end-test*
		      *loop-bindings*
		      ))
    (setf *product-for* t)
    (loop-pop)
    (sloop-swap)
    (parse-loop-for)
     (sloop-swap)
    (do ()
	((null *loop-form*))
      (cond ((catch 'collect  (parse-loop1)))
	    ((null *loop-form*)(return 'done))
	    (t ;(fsignal "hi")
	     (print *loop-form*)
	     (sloop-swap)
	     (parse-loop-collect)
	     (sloop-swap)
	     	     (print *loop-form*)
	     )))
    (sloop-swap)
    (setf inner-body (nreverse *loop-body*))
    (and *loop-map*  (setf inner-body (substitute-sloop-body inner-body)))
    (let ((bod
	    `(macrolet ((local-finish () `(go ,',finish-loop)))
	      (tagbody
		  ,@ (nreverse *loop-prologue*)
	          ,@ (and (null *loop-map*) '(next-loop))
		  ,@ (nreverse *loop-end-test*)
		  ,@ inner-body
		  ,@ (nreverse *loop-increment*)
		  ,@ (and (null *loop-map*) '((go next-loop)))
		  ,finish-loop
		  ,@ (nreverse *loop-epilogue*)))))
      (dolist (v *loop-bindings*)
	(setf bod
	      `(let ,(loop-let-bindings v) ,@(and (cdr v) `(,(cons 'declare (cdr v))))
		    ,bod)))
      (sloop-swap)
      `(do ,bod))))

;;; Usage: SLOOP (FOR 
;;; (defun te ()
;;;   (sloop for i below 5
;;;	  sloop (for j  to i collecting (list i j))))

(def-loop-for in-carefully (var lis)
  "Path with var in lis except lis may end with a non nil cdr" 
  (let ((point (gensym "POINT")))
    `(with ,point and with ,var initially (setf ,point ,lis)
           do(desetq ,var (car ,point))
	   end-test (and (atom ,point)(local-finish))
	   increment (setf ,point (cdr ,point)))))

;;; Usage: IN-CAREFULLY
;;; (defun te (l)
;;;   (sloop for v in-carefully l collecting v))

;;; Note the following is much like the mit for i first expr1 then expr2
;;; but it is not identical, in that if expr1 refers to paralell for loop
;;; it will not get the correct initialization.  But since we have such
;;; generality in the our definition of a for construct, it is unlikely
;;; that all people who define This is why we use a different name

(def-loop-for first-use (var expr1 then expr2)
  (or (l-equal then 'then) (error "First must be followed by then"))
  `(with ,var initially (desetq ,var ,expr1) increment (desetq ,var ,expr2)))

;;; I believe the following is what the original loop does with the FIRST
;;; THEN construction.  

(def-loop-for first (var expr1 then expr2)
  (declare (special *loop-increment*))
  (or (l-equal then 'then) (error "First must be followed by then"))
  ;; If this is the first for, then we don't need the flag, but can
  ;; move the FIRST setting into the INITIALLY section
  (cond ((null *loop-increment*)
	 `(with ,var initially (desetq ,var ,expr1)
		increment (desetq ,var ,expr2)))
	(t
	  (let ((flag (gensym)))
	    `(with ,var with ,flag
		   do (cond (,flag (desetq ,var ,expr2))
			    (t (desetq ,var ,expr1)))
		   increment (desetq ,flag t))))))


(defvar *collate-order* #'<)

;;; of course this should be a search of the list based on the order and
;;; splitting into halves (binary search).  I was too lazy to include one
;;; here, but it should be done.

(defun find-in-ordered-list
       (it list &optional (order-function *collate-order*) &aux prev)
  (do ((v list (cdr v)))
      ((null v) (values prev nil))
	 (cond ((eql (car v) it) (return (values v t)))
	       ((funcall order-function it (car v))
		(return (values prev nil))))
	 (setq prev v)))

(def-loop-collect collate (ans val)
  "Collects values into a sorted list without duplicates.
Order based order function *collate-order*"
  `(do (multiple-value-bind
       (after already-there )
       (find-in-ordered-list ,val ,ans)
       (unless already-there
	  (cond (after (setf (cdr after) (cons ,val (cdr after))))
		(t (setf ,ans (cons ,val ,ans))))))))

;;; Usage: COLLATE
;;; (defun te ()
;;;   (let ((res
;;;	   (sloop for i below 10
;;;               sloop (for j downfrom 8 to 0 
;;;		          collate (* i (mod j (max i 1)) (random 2)))))
;;;

;;;  Two implementations of slooping over the fringe of a tree

;;;(defun map-fringe (fun tree)
;;;      (do ((v tree))
;;;	       (())
;;;	(cond ((atom v)
;;;		    (and v (funcall fun v))(return 'done))
;;;	      ((atom (car v))
;;;		    (funcall fun (car v)))
;;;	      (t (map-fringe fun (car v) )))
;;;	     (setf v (cdr v))))
;;;
;;;(def-loop-map in-fringe (var tree)
;;;  "Map over the non nil atoms in the fringe of tree"
;;;  `(map-fringe #'(lambda (,var) :sloop-map-declares :sloop-body) ,tree))

;;; The next version is equivalent to the previous but uses labels and so
;;; avoids having to funcall an anonymous function. [as suggested
;;; by M. Ballantyne]

(def-loop-map in-fringe (var tree)
  "Map over the non nil atoms in the fringe of tree"
  (let  ((v (gensym)))
    `(let (,var)
       (labels
	   ((map-fringe-aux (.xtree.)
			    (do ((,v .xtree.))
				((null ,v))
			      (cond ((atom ,v) (setf ,var ,v) (setf ,v nil))
				    (t (setf ,var (car ,v))(setf ,v (cdr ,v))))
			      (cond ((null ,var))
				    ((atom ,var)
				     :sloop-map-declares :sloop-body)
				    (t (map-fringe-aux ,var ))))))
	 (map-fringe-aux ,tree)))))

;;; Usage: IN-FRINGE
;;; (sloop for v in-fringe '(1 2 (3 (4 5) . 6) 8 1 2)
;;;        declare (fixnum v)
;;;        maximize v)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_sloop.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./lsp/gcl_predlib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-Lisp-*-
;; Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

;; This file is part of GNU Common Lisp, herein referred to as GCL
;;
;; GCL is free software; you can redistribute it and/or modify it under
;;  the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GCL is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
;; License for more details.
;; 
;; You should have received a copy of the GNU Library General Public License 
;; along with GCL; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;;    predlib.lsp
;;;;
;;;;                              predicate routines

(in-package :system)

(export '(int void static 
	      non-standard-generic-function
	      non-standard-generic-compiled-function
	      non-standard-generic-interpreted-function
	      standard-generic-compiled-function
	      standard-generic-interpreted-function
	      non-logical-pathname
	      non-standard-base-char true gsym
	      std-instance
	      hash-table-eq hash-table-eql hash-table-equal hash-table-equalp
	      +type-alist+ 
	      sequencep ratiop short-float-p long-float-p
	      eql-is-eq equal-is-eq equalp-is-eq eql-is-eq-tp equal-is-eq-tp equalp-is-eq-tp
	      +array-types+
	      +aet-type-object+ 
	      returns-exactly
	      immfix
	      file-input-stream file-output-stream file-io-stream file-probe-stream
	      string-input-stream string-output-stream
	      proper-sequence proper-sequencep proper-cons proper-consp
	      fcomplex dcomplex 
	      cnum-type
	      subtypep1 ;FIXME
	      resolve-type))

;(defconstant +array-types+ (si::aelttype-list))

;; (export '(lisp::upgraded-complex-part-type lisp::type-of
;; 	  lisp::deftype lisp::typep lisp::subtypep 
;; 	  lisp::coerce lisp::upgraded-array-element-type) 'lisp)

;;Exported functions need to be compiled with some safety to ensure
;;that user-supplied arguments are checked.  They should therefore
;;primarily be wrappers around computationally intensive internal
;;functions compiled with more aggressive settings. 20050718 CM

(defmacro check-type (place typespec &optional string)
  (declare (optimize (safety 2)))
  `(progn (,(if (symbolp place) 'setq 'setf) ,place 
	   (the ,typespec (if (typep ,place ',typespec) ,place (check-type-symbol ',place ,place ',typespec ',string)))) nil))

(defun ratiop (x) (and (rationalp x) (not (integerp x))))

(defun upgraded-complex-part-type (type &optional environment) 
  (declare (ignore environment) (optimize (safety 2)))
  type)

(defmacro check-type-eval (place type)
  `(values (assert (typep ,place ,type) (,place) 'type-error :datum ,place :expected-type ,type)));fixme

;;; COERCE function.
;(defconstant +coerce-list+ '(list vector string array character short-float
;				  long-float float complex function null cons))

(defconstant +objnull+ (objnull))

#.`(defun coerce (object type &aux ntype (atp (listp type)) (ctp (if atp (car type) type)) (tp (when atp (cdr type))))
  (declare (optimize (safety 2))) ;(print (list 'coerce object type))
  (check-type type (or (member function) type-spec));FIXME
  (case ctp
	(function
	 (let ((object object))
	   (check-type object (or function (and symbol (not boolean)) (cons (member lambda) t)))
	   (typecase
	    object
	    (function object) 
	    ((and symbol (not boolean)) 
	     (let* ((f (c-symbol-gfdef object))(fi (address f))(m (c-symbol-mflag object)))
	       (check-type fi (and fixnum (not (integer ,+objnull+ ,+objnull+))))
	       (check-type m  (integer 0 0))
	       f))
	    (cons (the function (eval object))))))
	((list cons vector array member) (if (typep object type) object (replace (make-sequence type (length object)) object)))
	(character (character object))
	(short-float (float object 0.0S0))
	(long-float (float object 0.0L0))
	(float (float object))
	(complex (if (typep object type) object
	 (let ((rtp (or (car tp) t)))
	   (complex (coerce (realpart object) rtp) (coerce (imagpart object) rtp)))))
	(otherwise 
	 (cond ((typep object type) object)
	       ((setq ntype (expand-deftype type)) (coerce object ntype))
	       ((check-type-eval object type))))))

;; (defun coerce (object type &aux ntype (atp (listp type)) (ctp (if atp (car type) type)) (tp (when atp (cdr type))))
;;   (declare (optimize (safety 2)))
;;   (check-type type type-spec)
;;   (when (typep object type)
;;     (return-from coerce object))
;;   (case ctp
;; 	(function (if (symbolp object) (symbol-function object) (values (eval `(function ,object)))));FIXME
;; 	((list cons vector array member) (replace (make-sequence type (length object)) object))
;; 	(character (character object))
;; 	(short-float (float object 0.0S0))
;; 	(long-float (float object 0.0L0))
;; 	(float (float object))
;; 	(complex
;; 	 (let ((rtp (or (car tp) t)))
;; 	   (complex (coerce (realpart object) rtp) (coerce (imagpart object) rtp))))
;; 	(otherwise 
;; 	 (cond ((setq ntype (expand-deftype type)) (coerce object ntype))
;; 	       ((check-type-eval object type))))))

;; (defun coerce (object type &aux ntype (atp (listp type)) (ctp (if atp (car type) type)) (tp (when atp (cdr type))))
;;   (declare (optimize (safety 2)))
;;   (check-type type type-spec)
;;   (when (typep object type)
;;     (return-from coerce object))
;;   (case ctp
;; 	(function (if (symbolp object) (symbol-function object) (values (eval `(function ,object)))));FIXME
;; 	((list cons vector array member) (replace (make-sequence type (length object)) object))
;; 	(character (character object))
;; 	(short-float (float object 0.0S0))
;; 	(long-float (float object 0.0L0))
;; 	(float (float object))
;; 	(complex
;; 	 (let ((rtp (or (car tp) t)))
;; 	   (complex (coerce (realpart object) rtp) (coerce (imagpart object) rtp))))
;; 	(otherwise 
;; 	 (cond ((si-classp ctp) (coerce object (si-class-name ctp)))
;; 	       ((let ((tem (get ctp 'deftype-definition)))
;; 		  (when tem
;; 		    (setq ntype (apply tem tp))
;; 		    (not (eq ctp (if (listp ntype) (car ntype) ntype)))))
;; 		(coerce object ntype))
;; 	       ((check-type-eval object type))))))

;; (defun coerce (object type)
;;   (declare (optimize (safety 2)))
;;   (check-type type (and (not null) type-spec))
;;   (when (typep object type)
;;     (return-from coerce object))
;;   (let ((tp (or (car (member (if (atom type) type (car type)) +coerce-list+))
;; 		(car (member type +coerce-list+ :test 'subtypep1)))))
;;     (case tp
;;       (function
;;        (coerce 
;; 	(cond ((let ((object (funid-sym-p object))) 
;; 		 (when object 
;; 		   (cond ((fboundp object) (symbol-function object)) ((check-type-eval object type))))))
;; 	      ((and (consp object) (eq (car object) 'lambda)) (values (eval `(function ,object))))
;; 	      ((check-type-eval object type)))
;; 	type))
;;        ((null cons list)
;;        (let* ((l (length object))
;; 	      (x (sequence-type-length-type type)))
;; 	 (when x (check-type l x))
;; 	 (do ((ll nil (cons (aref object i) ll))
;; 	      (i (1- l) (1- i)))
;; 	     ((< i 0) ll))))
;;       ((vector string array)
;;        (let* ((l (length object))
;; 	      (x (sequence-type-length-type type))
;; 	      (v (typep object 'list)))
;; 	 (when x (check-type-eval l x))
;; 	 (do ((seq (make-sequence type l))
;; 	      (i 0 (1+ i))
;; 	      (p (and v object) (and p (cdr p))))
;; 	     ((>= i l) seq)
;; 	  (setf (aref seq i) (if p (car p) (aref object i))))));;FIXME
;;       (character (character object))
;;       (short-float (float object 0.0S0))
;;       (long-float (float object 0.0L0))
;;       (float (float object))
;;       (complex
;;        (if (or (atom type) (null (cdr type)) (null (cadr type)) (eq (cadr type) '*))
;; 	   (complex (realpart object) (imagpart object))
;; 	 (complex (coerce (realpart object) (cadr type))
;; 		  (coerce (imagpart object) (cadr type)))))
;;       (otherwise (check-type-eval object type)))))
;(putprop 'coerce t 'compiler::cmp-notinline);FIXME


(defun maybe-clear-tp (sym)
  (let* ((p (find-package "COMPILER")) (s (and p (find-symbol "*NORM-TP-HASH*" p))))
    (when (and s (boundp s)) (remhash sym (symbol-value s)))))

(defun satisfies-predicate (form)
  (cond ((atom form) nil)
	((consp (car form)) (when (null (cdr form)) (satisfies-predicate (car form))))
	((eq (car form) 'quote) (satisfies-predicate (cadr form)))
	((eq (car form) 'satisfies) (cadr form))))
	

;;; DEFTYPE macro.
(defmacro deftype (name lambda-list &rest body &aux decls)
  ;; Replace undefaultized optional parameter X by (X '*).
  (declare (optimize (safety 2)))
  (do ((b body body)) ((or (not b) (not (consp (car b))) (not (eq 'declare (caar b)))) (nreverse decls))
      (let ((d (pop body)))
	(unless (member-if (lambda (x) (and (consp x) (eq (car x) 'special))) (cdr d)) ;FIXME
	  (push d decls))))
  (do ((l lambda-list (cdr l))
       (m nil (cons (car l) m)))
      ((null l))
      (when (member (car l) lambda-list-keywords)
	(unless (member (car l) '(&optional &key)) (return nil))
	(setq m (cons (car l) m))
	(setq l (cdr l))
	(do ()
	    ((or (null l) (member (car l) lambda-list-keywords)))
	    (if (symbolp (car l))
		(setq m (cons (list (car l) ''*) m))
	      (setq m (cons (car l) m)))
	    (setq l (cdr l)))
	(setq lambda-list (nreconc m l))
	(return nil)))
  (let ((fun-name (gensym (string name))))
    `(eval-when (compile eval load)
		(putprop ',name
			 '(deftype ,name ,lambda-list ,@body)
			 'deftype-form)
		(defun ,fun-name ,lambda-list ,@decls (block ,name ,@body))
		(putprop ',name ',fun-name 'deftype-definition)
		(maybe-clear-tp ',name)
		(putprop ',name
			 ,(find-documentation body)
			 'type-documentation)
		,@(let* ((r (unless lambda-list (eval (cons 'progn body))))
			 (p (satisfies-predicate r)))
		    (when p
		      `((putprop ',name ',p 'type-predicate)
			(putprop ',p ',name 'predicate-type))))
		',name)))

;;; Some DEFTYPE definitions.

;; (deftype lambda-expression nil `(cons (member lambda) t))
;; (deftype function-sym nil `(and symbol (not boolean)))
;; (deftype setf-function-name nil `(cons (member setf) (cons function-sym null)))
;; (deftype function-name nil `(or function-sym setf-function-name))
;; (deftype function-designator `(or function function-sym))
;; (deftype extended-function-designator `(or function function-sym))


(deftype function-designator nil `(or (and symbol (not boolean)) function))
(deftype extended-function-designator nil `(or function-designator (cons (member setf) (cons symbol null))))

(deftype hash-table nil `(or hash-table-eq hash-table-eql hash-table-equal hash-table-equalp))
;(deftype compiler::funcallable-symbol nil `(satisfies compiler::funcallable-symbol-p));FIXME

(defconstant +ifb+ (- (car (last (multiple-value-list (si::heap-report))))))
(defconstant +ifr+ (ash (- +ifb+)  -1))
(defconstant +ift+ (when (> #.+ifr+ 0) '(immfix #.(- +ifr+) #.(1- +ifr+))))

;(deftype immfix () +ift+)
;(deftype bfix nil `(and fixnum (not immfix)))
(deftype eql-is-eq-tp () `(or #.+ift+ (not number)))
(deftype equal-is-eq-tp () `(or #.+ift+ (not (or cons string bit-vector pathname number))))
(deftype equalp-is-eq-tp () `(not (or array hash-table structure cons string
				      bit-vector pathname number)))

(defun eql-is-eq (x)
  (typecase
   x
   (immfix t)
   (number nil)
   (otherwise t)))
(setf (get 'eql-is-eq 'cmp-inline) t)

;To pevent typep/predicate loops
;(defun eql-is-eq (x) (typep x (funcall (get 'eql-is-eq-tp 'deftype-definition))))
(defun equal-is-eq (x) (typep x (funcall (get 'equal-is-eq-tp 'deftype-definition))))
(defun equalp-is-eq (x) (typep x (funcall (get 'equalp-is-eq-tp 'deftype-definition))))

(defun seqindp (x) (and (fixnump x) (>= x 0) (< x array-dimension-limit)))
(si::putprop 'seqindp t 'cmp-inline)

(deftype non-negative-char () `(non-negative-byte ,char-length))
(deftype negative-char () `(negative-byte ,char-length))
(deftype signed-char ()`(signed-byte ,char-length))
(deftype unsigned-char ()`(unsigned-byte ,char-length))
(deftype char ()`(signed-char))

(deftype non-negative-short () `(non-negative-byte ,short-length))
(deftype negative-short () `(negative-byte ,short-length))
(deftype signed-short ()`(signed-byte ,short-length))
(deftype unsigned-short ()`(unsigned-byte ,short-length))
(deftype short ()`(signed-short))

(deftype non-negative-int () `(non-negative-byte ,int-length))
(deftype negative-int () `(negative-byte ,int-length))
(deftype signed-int ()`(signed-byte ,int-length))
(deftype unsigned-int ()`(unsigned-byte ,int-length))
(deftype int ()`(signed-int))

(deftype non-negative-fixnum () `(non-negative-byte ,fixnum-length))
(deftype negative-fixnum () `(negative-byte ,fixnum-length))
(deftype signed-fixnum ()`(signed-byte ,fixnum-length))
(deftype unsigned-fixnum ()`(unsigned-byte ,fixnum-length))
;(deftype fixnum ()`(signed-fixnum))

(deftype non-negative-lfixnum () `(non-negative-byte ,lfixnum-length))
(deftype negative-lfixnum () `(negative-byte ,lfixnum-length))
(deftype signed-lfixnum ()`(signed-byte ,lfixnum-length))
(deftype unsigned-lfixnum ()`(unsigned-byte ,lfixnum-length))
(deftype lfixnum ()`(signed-lfixnum))

(deftype fcomplex nil `(complex short-float))
(deftype dcomplex nil `(complex long-float))

;; (defun key-fn-p (x)
;;   (or (funcall x nil) t))
;; (deftype key-fn nil `(or (eql ,#'identity) (satisfies key-fn-p)))

;; (defun test-fn-p (x)
;;   (or (funcall x nil nil) t))
;; (deftype test-fn nil `(satisfies test-fn-p))

(deftype string (&optional size)
  `(array character (,size)))
(deftype base-string (&optional size)
  `(array base-char (,size)))
(deftype bit-vector (&optional size)
  `(array bit (,size)))

(deftype simple-vector (&optional size)
  `(array t (,size)))
(deftype simple-string (&optional size)
  `(array character (,size)))
(deftype simple-base-string (&optional size)
  `(array base-char (,size)))
(deftype simple-bit-vector (&optional size)
  `(array bit (,size)))

(deftype function-name nil `(or symbol (cons (member setf) (cons symbol null))))
(deftype function-identifier nil `(or function-name (cons (member lambda) t)));;FIXME? t?

(deftype list () `(or cons null))
(deftype sequence () `(or list vector))

(defun standard-charp (x)
  (when (characterp x)
    (standard-char-p x)))

(defun non-standard-base-char-p (x)
  (and (characterp x) (not (standard-char-p x))))

(defun improper-consp (s &optional (f nil fp) (z (if fp f s)))
  (cond ((atom z) (when fp (when z t)))
	((atom (cdr z)) (when (cdr z) t))
	((eq s f))
	((improper-consp (cdr s) (cddr z)))))

;; (defun improper-consp (s &optional (f nil fp))
;;   (cond ((not fp) (cond ((atom s) nil)
;; 			((atom (cdr s)) (when (cdr s) t))
;; 			((improper-consp (cdr s) (cddr s)))))
;; 	((atom f) (when f t))
;; 	((atom (cdr f)) (when (cdr f) t))
;; 	((eq s f))
;; 	((improper-consp (cdr s) (cddr f)))))

;(deftype proper-list () `(or null (and cons (not (satisfies improper-consp)))))

(deftype extended-char () nil)
(deftype base-char () `(or standard-char non-standard-base-char))
(deftype character () `(or base-char extended-char))

(deftype stream () `(or broadcast-stream concatenated-stream echo-stream
			file-stream string-stream synonym-stream two-way-stream))
(deftype file-stream nil `(or file-input-stream file-output-stream file-io-stream file-probe-stream))
(deftype path-stream nil `(or file-stream file-synonym-stream))
(deftype pathname-designator nil `(or pathname string path-stream))
(deftype synonym-stream nil `(or file-synonym-stream non-file-synonym-stream))
(deftype string-stream nil `(or string-input-stream string-output-stream))

(deftype input-stream ()  `(and stream (satisfies  input-stream-p)))
(deftype output-stream () `(and stream (satisfies  output-stream-p)))

;(deftype bignum () `(and integer (not fixnum)))
(deftype non-negative-bignum () `(and non-negative-byte (not non-negative-fixnum)))
(deftype negative-bignum () `(and negative-byte (not negative-fixnum)))
;;FIXME adjust integer bounds here?

;; (deftype fixnum (&optional (low most-negative-fixnum) (high most-positive-fixnum)
;; 			   &aux (low (if (listp low) (1+ (car low)) low))
;; 			   (high (if (listp high) (1- (car high)) high)) (lf (typep low 'immfix))
;; 			   (hf (typep high 'immfix)))
;;   (cond ((and lf hf) `(immfix ,low ,high))
;; 	(lf `(or (immfix ,low ,most-positive-immfix) (bfix ,(1+ most-positive-immfix) ,high)))
;; 	(hf `(or (immfix ,most-negative-immfix ,high) (bfix ,low ,(1- most-negative-immfix))))
;; 	((eql (if (eq low '*) -1 (signum low)) (if (eq high '*) 1 (signum high))) `(bfix ,low ,high))
;; 	(`(or (immfix ,most-negative-immfix ,most-positive-immfix) (bfix ,low ,(1- most-negative-immfix))
;; 	      (bfix ,(1+ most-positive-immfix) ,high)))))

;; (deftype integer (&optional (low '*) (high '*) &aux (low (if (listp low) (1+ (car low)) low))
;; 			    (high (if (listp high) (1- (car high)) high)) (lf (typep low 'fixnum))
;; 			    (hf (typep high 'fixnum)))
;;   (cond ((and lf hf) `(fixnum ,low ,high))
;; 	(lf `(or (fixnum ,low ,most-positive-fixnum) (bignum ,(1+ most-positive-fixnum) ,high)))
;; 	(hf `(or (fixnum ,most-negative-fixnum ,high) (bignum ,low ,(1- most-negative-fixnum))))
;; 	((eql (if (eq low '*) -1 (signum low)) (if (eq high '*) 1 (signum high))) `(bignum ,low ,high))
;; 	(`(or (fixnum ,most-negative-fixnum ,most-positive-fixnum) (bignum ,low ,(1- most-negative-fixnum))
;; 	      (bignum ,(1+ most-positive-fixnum) ,high)))))

(defconstant most-negative-immfix (or (cadr +ift+) 1))
(defconstant most-positive-immfix (or (caddr +ift+) -1))

(deftype rational (&optional (low '*) (high '*)) `(or (integer ,low ,high) (ratio ,low ,high)))
(deftype float (&optional (low '*) (high '*)) `(or (short-float ,low ,high) (long-float ,low ,high)))
(deftype single-float (&optional (low '*) (high '*)) `(long-float ,low ,high))
(deftype double-float (&optional (low '*) (high '*)) `(long-float ,low ,high))
(deftype real (&optional (low '*) (high '*)) `(or (rational ,low ,high) (float ,low ,high)))
(deftype number () `(or real complex))
(deftype atom () `(not cons))
(deftype compiled-function nil `(or standard-generic-compiled-function non-standard-generic-compiled-function))
(deftype interpreted-function nil `(or standard-generic-interpreted-function non-standard-generic-interpreted-function))
(deftype function (&rest r) (declare (ignore r)) `(or compiled-function interpreted-function))
(deftype standard-generic-function nil `(or standard-generic-compiled-function standard-generic-interpreted-function))
(deftype non-standard-generic-function nil `(and function (not standard-generic-function)))

;(deftype integer (&optional (low '*) (high '*)) `(integer ,low ,high))
(deftype ratio (&optional (low '*) (high '*)) `(ratio ,low ,high))
(deftype short-float (&optional (low '*) (high '*)) `(short-float ,low ,high))
(deftype long-float (&optional (low '*) (high '*)) `(long-float ,low ,high))

(deftype array (&optional (et '*) (dims '*)) `(array ,et ,(or dims 0)))
(deftype simple-array (&rest r) (cons 'array r))
;; (deftype simple-array (&optional (et '*) (dims '*)) 
;;   `(array ,et ,(if (not dims) 0 dims)))

(deftype eql (&optional (x nil xp)) (when xp `(member ,x)))

(deftype true nil `(member t))
(deftype null () `(member nil))
(deftype boolean () `(or true null))

(deftype symbol () `(or boolean keyword gsym))
; (deftype symbol nil '(or keyword null true gsym))

(deftype complex (&optional (rp 'real)) `(complex ,(if (eq rp '*) 'real rp)))
(deftype cons (&optional (car t) (cdr t)) `(cons ,(if (eq car '*) t car)  ,(if (eq cdr '*) t cdr)))
;(deftype structure-object nil `(structure))
;(deftype logical-pathname nil `(and pathname (satisfies logical-pathname-p)))
(deftype pathname () `(or non-logical-pathname logical-pathname))

;; (defun non-logical-pathname-p (x)
;;   (and (pathnamep x) (not (logical-pathname-p x))))

;; (defun non-keyword-symbol-p (x)
;;   (and (symbolp x) (not (keywordp x))))

(defun gsym-p (x) (when x (unless (eq x t) (unless (keywordp x) (symbolp x)))))

;(defun simple-array-p (x)
;  (and (arrayp x)
       ;; should be (not (expressly-adjustable-p x))
       ;; since the following will always return T
       ;; (not (adjustable-array-p x))
;       (not (array-has-fill-pointer-p x))
;       (not (displaced-array-p x))))


;(defun extended-char-p (x) (declare (ignore x)) nil)

;(defun interpreted-function-p (x) 
;  (and (function-p x) (not (compiled-function-p x))))

;(defun function-p (x)
;  (and (not (symbolp x)) (functionp x)))

(defun sequencep (x)
  (or (listp x) (vectorp x)))

(defun short-float-p (x)
  (= (c-type x) #.(c-type 0.0s0)))
;  (and (floatp x) (eql x (float x 0.0s0))))

(defun long-float-p (x)
  (= (c-type x) #.(c-type 0.0)))
;  (and (floatp x) (eql x (float x 0.0))))

(defun fcomplexp (x)
  (and (complexp x) (short-float-p (realpart x)) (short-float-p (imagpart x))))

(defun dcomplexp (x)
  (and (complexp x) (long-float-p (realpart x)) (long-float-p (imagpart x))))

;(defun proper-listp (x)
;  (or (not x) (and (consp x) (not (improper-consp x)))))

(deftype proper-sequence () `(or vector proper-list))

(deftype proper-list () `(or null proper-cons))

(defun proper-consp (x)
  (and (consp x) (not (improper-consp x))))

(defun proper-listp (x)
  (or (null x) (proper-consp x)))

(defun proper-sequencep (x)
  (or (vectorp x) (proper-listp x)))

(deftype not-type nil 'null)
;; (deftype list-of (y) 
;;   (let* ((sym (intern (string-concatenate "LIST-OF-" (symbol-name y)) 'si))) 
;;     (unless (fboundp sym)
;;       (eval `(defun ,sym (x) 
;; 	       (declare (proper-list x))
;; 	       (not (member-if-not (lambda (x) (typep x ',y)) x)))) )
;;     `(and proper-list (satisfies ,sym))))

;; #.`(eval-when 
;;     (compile load eval) 
;;     ,@(mapcar (lambda (x) 
;; 		`(defun ,(intern (string-concatenate "LIST-OF-" (symbol-name x))) (w)
;; 		   (declare (proper-list w))
;; 		   (not (member-if-not (lambda (z) (typep z ',x)) w))))
;; 	      '(symbol seqind proper-list)))

(deftype type-spec nil `(satisfies type-spec-p))

(defun type-list-p (spec r &aux s)
  (not (member-if-not (lambda (x &aux (q (member x r)))
			(or (when q (setq s (car q) r (cdr q)) q)
			    (unless (eq s '&allow-other-keys)
			      (when (typep x (if (eq s '&key) '(cons keyword (cons type-spec null)) 'type-spec))
				(if (eq s '&rest) (setq s '&allow-other-keys) t))))) spec)))

(defun arg-list-type-p (x) (type-list-p x '(&optional &rest &key)))

(defun values-list-type-p (x)
  (if (when (listp x) (eq (car x) 'values))
      (type-list-p (cdr x) '(&optional &rest &allow-other-keys))
    (typep x 'type-spec)))

(deftype ftype-spec nil `(cons (member function)
			       (cons (satisfies arg-list-type-p)
				     (cons (satisfies values-list-type-p) null))))

(deftype fpvec nil `(and vector (satisfies array-has-fill-pointer-p)))

;; (defun non-generic-compiled-function-p (x)
;;   (cond ((typep x 'generic-function) nil)
;; 	((typep x 'function) (typep (caddr (c-function-plist x)) 'string))))
					;(compiled-function-p x)

(defconstant +type-alist+ '((null . null)
	  (not-type . not)
          (symbol . symbolp)
          (eql-is-eq-tp . eql-is-eq)
          (equal-is-eq-tp . equal-is-eq)
          (equalp-is-eq-tp . equalp-is-eq)
          (keyword . keywordp)
	  ;; (non-logical-pathname . non-logical-pathname-p)
	  (logical-pathname . logical-pathname-p)
	  (proper-cons . proper-consp)
	  (proper-list . proper-listp)
	  (proper-sequence . proper-sequencep)
;	  (non-keyword-symbol . non-keyword-symbol-p)
	  (gsym . gsym-p)
	  (standard-char . standard-charp)
	  (non-standard-base-char . non-standard-base-char-p)
;	  (interpreted-function . interpreted-function-p)
	  (real . realp)
	  (float . floatp)
	  (short-float . short-float-p)
	  (long-float . long-float-p)
	  (fcomplex . fcomplexp)
	  (dcomplex . dcomplexp)
	  (array . arrayp)
	  (vector . vectorp)
	  (bit-vector . bit-vector-p)
	  (string . stringp)
	  (complex . complexp)
	  (ratio . ratiop)
	  (sequence . sequencep)
          (atom . atom)
          (cons . consp)
          (list . listp)
          (seqind . seqindp)
          (fixnum . fixnump)
          (integer . integerp)
          (rational . rationalp)
          (number . numberp)
          (character . characterp)
          (package . packagep)
          (stream . streamp)
          (pathname . pathnamep)
          (readtable . readtablep)
          (hash-table . hash-table-p)
          (hash-table-eq . hash-table-eq-p)
          (hash-table-eql . hash-table-eql-p)
          (hash-table-equal . hash-table-equal-p)
          (hash-table-equalp . hash-table-equalp-p)
          (random-state . random-state-p)
          (structure . structurep)
          (function . functionp)
          ;; (compiled-function . compiled-function-p)
          ;; (non-generic-compiled-function . non-generic-compiled-function-p)
          ;; (generic-function . generic-function-p)
          (common . commonp)))

(dolist (l +type-alist+)
  (putprop (car l) (cdr l) 'type-predicate)
  (when (symbolp (cdr l)) 
    (putprop (cdr l) (car l) 'predicate-type)))


(defconstant +singleton-types+ '(null true gsym keyword standard-char
				      non-standard-base-char 
				      package cons
				      broadcast-stream concatenated-stream echo-stream
				      file-input-stream file-output-stream file-io-stream file-probe-stream
				      string-input-stream string-output-stream
				      file-synonym-stream non-file-synonym-stream two-way-stream 
				      non-logical-pathname logical-pathname
				      readtable 
				      hash-table-eq hash-table-eql hash-table-equal hash-table-equalp
				      random-state std-instance structure 
				      non-standard-generic-interpreted-function
				      non-standard-generic-compiled-function
				      standard-generic-interpreted-function
				      standard-generic-compiled-function
				      spice
				      ))


(defconstant +range-types+ `(immfix bfix bignum ratio short-float long-float))
(defconstant +complex-types+ `(integer ratio short-float long-float))

;(defconstant +array-types+ (append '(nil) +array-types+))

(deftype vector (&optional (element-type '* ep) size)
  (if ep 
      `(array ,element-type (,size))
    `(or ,@(mapcar (lambda (x) `(array ,x (,size))) +array-types+))))

(defconstant +known-types+ (append +range-types+ 
				   (mapcar (lambda (x) `(complex ,x)) +complex-types+)
				   +singleton-types+
				   (mapcar (lambda (x) `(array ,x)) +array-types+)))

(defconstant +array-type-alist+ (mapcar (lambda (x) (cons x (intern (string-concatenate "ARRAY-" (string x)))))
					+array-types+))
(defconstant +complex-type-alist+ (mapcar (lambda (x) (cons x (intern (string-concatenate "COMPLEX-" (string x)))))
					+complex-types+))

(defconstant +kingdom-logic-ops-alist+ `(,@(mapcar (lambda (x) `(,x . range-op)) +range-types+)
					  (cons . cons-op)
;					  (standard-object . standard-op)
					  (std-instance . standard-op)
					  (standard-generic-compiled-function . standard-op)
					  (standard-generic-interpreted-function . standard-op)
					  (structure . structure-op)
					  ,@(mapcar (lambda (x) `(,(cdr x) . complex-op)) +complex-type-alist+)
					  ,@(mapcar (lambda (x) `(,(cdr x) . array-op)) +array-type-alist+)
					  ,@(mapcar (lambda (x) `(,x . single-op)) +singleton-types+)))

(defconstant +kingdom-recon-ops-alist+ `(,@(mapcar (lambda (x) `(,x . range-recon)) +range-types+)
					  (cons . cons-recon)
;					  (standard-object . standard-recon)
					  (std-instance . standard-recon)
					  (standard-generic-compiled-function . standard-recon)
					  (standard-generic-interpreted-function . standard-recon)
					  (structure . structure-recon)
					  ,@(mapcar (lambda (x) `(,(cdr x) . complex-recon)) +complex-type-alist+)
					  ,@(mapcar (lambda (x) `(,(cdr x) . array-recon)) +array-type-alist+)
					  ,@(mapcar (lambda (x) `(,x . single-recon)) +singleton-types+)))

(defconstant +kingdom-load-ops-alist+ `(,@(mapcar (lambda (x) `(,x . range-load)) +range-types+)
					 (complex . complex-load)
					 (cons . cons-load)
;					 (standard-object . standard-load)
					 (std-instance . standard-load)
					 (standard-generic-compiled-function . standard-load)
					 (standard-generic-interpreted-function . standard-load)
					 (structure . structure-load)
					 (array . array-load)
;					 (simple-array . array-load)
					 ,@(mapcar (lambda (x) `(,x . single-load)) +singleton-types+)))



(defmacro make-ntp nil `(make-list 3))
(defconstant +tp-nil+ (make-ntp))
(defconstant +tp-t+   '(nil t nil));FIXME cannot call ntp-not as depends on setf too early
(defmacro ntp-tps (ntp) `(first ,ntp))  ;FIXME reinstate these for clarity when fixing early setf
(defmacro ntp-def (ntp) `(second ,ntp))
(defmacro ntp-ukn (ntp) `(third ,ntp))
(defmacro ntp-ld (ntp tp) (let ((ns (gensym)) (ts (gensym)))
				`(let ((,ns ,ntp) (,ts ,tp))
				   (when ,ts
				     (if (car ,ns) (nconc (car
                                                               ,ns)
                                                              (list
                                                               ,ts))
                                       (setf (car ,ns) (list ,ts)))))))
(defmacro range-num (x) `(if (atom ,x) ,x (car ,x)))
(defmacro elcomp (x) `(cond ((eq ,x '*) ,x) 
			    ((atom ,x) (list ,x)) 
			    ((car ,x))))

;;FIXME --remove these when compiler is ready

(mapc (lambda (x) (setf (get x 'cmp-inline) t))
      '(lremove lremove-if lremove-if-not lremove-duplicates lreduce lsubstitute ldelete ldelete-if ldelete-if-not))

(defun lremove (q l &key (key #'identity) (test #'eql))
  (labels ((l (l) (when l
		    (let* ((x (car l))(z (cdr l))(y (l z)))
		      (if (funcall test q (funcall key x)) y (if (eq y z) l (cons x y)))))))
	  (l l)))


(defun lremove-if (f l) (lremove f l :test 'funcall))
(defun lremove-if-not (f l) (lremove (lambda (x) (not (funcall f x))) l :test 'funcall))

(defun lremove-duplicates (l &key (test #'eql))
  (labels ((f (l) (when l
		    (let* ((x (car l))(z (cdr l))(y (f z)))
		      (if (member x y :test test) y (if (eq y z) l (cons x y)))))))
	  (f l)))

(defun lreduce (f l &key (key #'identity) (initial-value nil ivp))
  (labels ((rl (s &optional (res initial-value)(ft ivp))
	       (if s (rl (cdr s) (let ((k (funcall key (car s)))) (if ft (funcall f res k) k)) t)
		 (if ft res (values (funcall f))))))
	  (rl l)))

(defun lsubstitute (n o l &key (test #'eql))
  (labels ((f (l) (when l
		    (let* ((x (car l))(z (cdr l))(y (f z))(sp (funcall test x o)))
		      (if (unless sp (eq y z)) l (cons (if sp n x) y))))))
	  (f l)))

(defun ldelete (q l &key (key #'identity)(test #'eql))
  (labels ((l (l) 
	      (when l
		(let* ((x (car l))(z (cdr l))(y (l z)))
		  (if (funcall test q (funcall key x)) y (if (eq y z) l (rplacd l y)))))))
	  (l l)))

(defun ldelete-if (f l) (ldelete f l :test 'funcall))
(defun ldelete-if-not (f l) (ldelete (lambda (x) (not (funcall f x))) l :test 'funcall))




;; (defmacro ldelete-if (fn lst)
;;   (let ((lsym (gensym)) (f (gensym)) (p (gensym)) (ind (gensym)))
;;     `(let ((,lsym ,lst))
;;        (do ((,p nil (or ,p ,f)) ,f (,ind ,lsym (cdr ,ind))) ((not ,ind) ,f)
;; 	   (if (funcall ,fn (car ,ind))
;; 	       (when ,p (setq ,p (rplacd ,p (cdr ,ind))))
;; 	     (setq ,f (or ,f ,ind) ,p (cdr ,p)))))))

;; (defmacro lsubstitute (n o s)
;;   `(mapcar (lambda (x) (if (eq x ,o) ,n x)) ,s))

;; (defmacro lreduce (f l &key (initial-value nil ivp))
;;   `(labels ((rl (s &optional (res ,initial-value)(ft ,ivp))
;; 		(if s (rl (cdr s) (if ft (funcall ,f res (car s)) (car s)) t)
;; 		  (if ft res (values (funcall ,f))))))
;; 	   (rl ,l)))

;; (defun lremove-duplicates (l &key (test #'eql) &aux y yp)
;;   (mapl (lambda (x) (unless (member (car x) (cdr x) :test test) 
;; 		      (let ((r (cons (car x) nil))) 
;; 			(setq yp (if yp (cdr (rplacd yp r)) (setq y r)))))) l)
;; ;  (print (list 'lremove-duplicates l y))
;;   y)

;; (defmacro lremove-if (fn lst)
;;   (let ((lsym (gensym)) (tmp (gensym)) (r (gensym)) (l (gensym)) (ind (gensym)))
;;     `(let ((,lsym ,lst) ,r ,l)
;;        (do ((,ind ,lsym (cdr ,ind))) ((not ,ind) ,r)
;; 	   (unless (funcall ,fn (car ,ind))
;; 	     (setf (car (setq ,l (let ((,tmp (cons nil nil))) (if ,l (cdr (rplacd ,l ,tmp)) (setq ,r ,tmp))))) (car ,ind)))))))

;; (defmacro lremove-if-not (fn lst)
;;   (let ((lsym (gensym)) (tmp (gensym)) (r (gensym)) (l (gensym)) (ind (gensym)))
;;     `(let ((,lsym ,lst) ,r ,l)
;;        (do ((,ind ,lsym (cdr ,ind))) ((not ,ind) ,r)
;; 	   (when (funcall ,fn (car ,ind))
;; 	     (setf (car (setq ,l (let ((,tmp (cons nil nil))) (if ,l (cdr (rplacd ,l ,tmp)) (setq ,r ,tmp))))) (car ,ind)))))))

(defvar *tp-mod* 0)
(eval-when (compile) (proclaim '(type (integer -1 1) *tp-mod*)));FIXME

(defun tp-mod (x d)
  (unless (or (not (caddr x)) (/= (let ((x *tp-mod*)) (if (>= x 0) x (- x))) 1))
    (if d (setf (car x) nil (car (cdr x)) (= *tp-mod* 1) (car (cddr x)) nil)
      (setq x (if (= *tp-mod* 1) +tp-t+ +tp-nil+))))
  x)

(defun ntp-clean (x) ;; FIXME same for t and (cadr x)
 (when (and (rassoc nil (car x) :key 'car) (not (cadr x)))
    (setf (car x) (lremove-if (lambda (x) (not (cadr x))) (car x)))))

(defun ntp-and (&rest xy)
  (when xy
    (let ((x (tp-mod (car xy) t)) (y (tp-mod (cadr xy) nil)))
      (dolist (l (car x))
	(let ((op (cdr (assoc (car l) +kingdom-logic-ops-alist+)))
	      (ny (assoc (car l) (car y))))
	  (when (or ny (not (cadr y)))
	    (funcall op 'and l ny))))
      (dolist (l (car y))
	(when (and (cadr x) (not (assoc (car l) (car x))))
	  (ntp-ld x l)))
      (setf (car (cdr x)) (and (cadr x) (cadr y)))
      (setf (car (cddr x)) (or  (caddr x) (caddr y)))
      (ntp-clean x)
      x)))

(defun ntp-or (&rest xy)
  (when xy
    (let ((x (tp-mod (car xy) t)) (y (tp-mod (cadr xy) nil)))
      (dolist (l (car x))
	(let* ((op (cdr (assoc (car l) +kingdom-logic-ops-alist+)))
	      (ny (assoc (car l) (car y)))
	      (ny (or ny (when (cadr y) (let ((z `(,(car l) nil))) (funcall op 'not z nil) z)))))
	  (when ny
	    (funcall op 'or l ny))))
      (dolist (l (car y))
	(unless (or (cadr x) (assoc (car l) (car x)))
	  (ntp-ld x l)))
      (setf (car (cdr x)) (or (cadr x) (cadr y)))
      (setf (car (cddr x)) (or (caddr x) (caddr y)))
      (ntp-clean x)
      x)))

(defun ntp-not (x)
  (dolist (l (car x))
    (funcall (cdr (assoc (car l) +kingdom-logic-ops-alist+)) 'not l nil))
  (setf (car (cdr x)) (negate (cadr x)))
  (ntp-clean x)
  x)

;; GENERIC SIGMA ALGEBRA OPERATIONS

(defun memberv (x y)
  (and (consp x) (consp y)
       (eq (car x) 'member) (eq (car y) 'member)
       (let ((z (union (cdr x) (cdr y))))
	 (and z `(member ,@z)))))

(defun member- (x y)
  (and (consp x) (consp y)
       (eq (car x) 'member) (eq (car y) 'member)
       (let ((z (set-difference (cdr x) (cdr y))))
	 (and z `(member ,@z)))))


(defun clean-or (atm lst)
  (if (funcall atm lst) lst
    (or 
     (car (member t lst))
     (let ((nl (lremove-if 'not (lremove-duplicates (cdr lst) :test 'equal))))
       (if (cdr nl) `(or ,@nl) (car nl))))))

(defun clean-and (atm lst)
  (if (funcall atm lst) lst
    (unless (member nil lst)
      (let ((nl (lremove-if (lambda (z) (eq z t)) (lremove-duplicates (cdr lst) :test 'equal))))
	(if (cdr nl) `(and ,@nl) (or (car nl) t))))))


(defun clean-tp (op atm x &optional y)
  (let ((cf (case op (or 'clean-or) (and 'clean-and) (otherwise (error "Bad op~%"))))
	(x (if (funcall atm x) (list x) x)))
    (let ((z (cond ((eq (car y) op) (funcall cf  atm `(,op ,@x ,@(cdr y))))
		   ((member (car y) '(and not or)) `(,op ,@x ,y))
		   ((funcall cf atm  `(,op ,@x ,@y))))))
      z)))
    
(defmacro negate (lst)
  (let ((l (gensym)))
    `(let ((,l ,lst))
       (cond ((not ,l))
	     ((eq ,l t) nil)
	     ((and (consp ,l) (eq (car ,l) 'not)) (cadr ,l))
	     (`(not ,,l))))))
  

(defun sigalg-atom-op (op ox oy ^ atm)
  (let* ((cx (and (consp ox) (eq (car ox) 'not)))
	 (x (if cx (cadr ox) ox))
	 (cy (and (consp oy) (eq (car oy) 'not)))
	 (y (if cy (cadr oy) oy))
	 (^xy (funcall ^ x y))
	 (vxy (memberv x y))
	 (-xy (if vxy (member- x y) x))
	 (-yx (if vxy (member- y x) y))
	 (<xy (equal ^xy x))
	 (<yx (equal ^xy y))
	 (opor (case op (or t) (and nil) (otherwise (error "Bad op~%")))))
    (cond ((not (or cx cy)) 
	   (cond (<xy (if opor y x))
		 (<yx (if opor x y))
		 (opor (or vxy (clean-or atm `(or ,x ,y))))
		 (^xy)))
	  ((and cx cy)
	   (cond (<xy `(not ,(if opor x y)))
		 (<yx `(not ,(if opor y x)))
		 (opor (negate ^xy))
		 ((negate (sigalg-atom-op 'or x y ^ atm)))))
	  (cx (sigalg-atom-op op oy ox ^ atm))
	  ((and <xy <yx) opor)
	  (<xy (when opor 
		 (if (equal -yx y)
		     (clean-or atm `(or ,x ,(negate y)))
		   (negate -yx))))
	  (<yx (if opor t (if (equal x -xy) (clean-and atm `(and ,x ,(negate y))) -xy)))
	  (opor (if (funcall ^ ^xy -yx) (clean-or atm `(or ,^xy ,(negate -yx))) (negate -yx)))
	  ((clean-and atm (if (funcall ^ -xy ^xy) `(and ,-xy ,(negate ^xy)) -xy))))))

(defun sigalg-op (op x y ^ atm)
  (let ((ax (funcall atm x)))
    (cond ((and (not ax) (eq op (car x)))
	   (sigalg-op op (cadr x) (clean-tp op atm (cddr x) (if (funcall atm y) (list y) y)) ^ atm))
	  ((and (not ax) (eq 'not (car x)))
	   (negate (sigalg-op (if (eq op 'or) 'and 'or) (negate x) (negate y) ^ atm)))
	  ((not ax);FIXME reduce here?
	   (clean-tp 
	    (if (eq op 'or) 'and 'or) 
	    atm 
	    (mapcar (lambda (w) (sigalg-op op w y ^ atm)) (cdr x))))
	  ((funcall atm y) (sigalg-atom-op op x y ^ atm))
	  ((eq op (car y))
	   (let* ((y (if (cddr y) (sigalg-op op (cadr y) `(,op ,@(cddr y)) ^ atm) y))
		  (y (if (or (atom y) (not (eq (car y) op))) `(,op ,y) y))
		  (q (mapcar (lambda (z) (sigalg-op op x z ^ atm)) (cdr y))))
	     (let ((z 
		    (clean-tp 
		     op atm
		     (do ((y (cdr y) (cdr y)) (q q (cdr q))) 
			 ((or (not y) (not q) (equal (car y) (car q))) (if (and y q) (eq op 'and) x)))
		     (mapcar (lambda (x y) (if (funcall atm y) y x))
			     (cdr y) q))))
	       (if (funcall atm z) z 
		 (let ((ny (clean-tp op atm (cddr z)))) 
		   (if (and (equal x (cadr z)) (equal ny y)) 
		       z 
		     (sigalg-op op (cadr z) ny ^ atm)))))))
	  ((eq 'not (car y))
	   (negate (sigalg-op (if (eq op 'or) 'and 'or) (negate x) (negate y) ^ atm)))
	  (t
	    (let ((z (mapcar (lambda (w) (sigalg-op op x w ^ atm)) (cdr y))))
	      (lreduce (lambda (&rest xy) (when xy (sigalg-op (if (eq op 'or) 'and 'or) (car xy) (cadr xy) ^ atm))) z))))))


;; RANGE-TYPES

(defun adj-bnd (nx fn bot top &optional (hmin top) (hmax top))
  (cond ((eq nx '*) bot)
	((unless (eq bot '*) (funcall fn nx bot)) bot)
	((funcall fn nx hmin) nx)
	((funcall fn nx hmax) hmax)
	((or (eq top '*) (funcall fn nx top)) nx)))

(defun adj-bndd (nx inc bot top &optional (hmin top) (hmax top))
  (if (> inc 0) (adj-bnd nx '<= bot top hmin hmax) (adj-bnd nx '>= top bot hmax hmin)))


(defun rational (x);FIXME different file -- bootstrap
  (declare (optimize (safety 1)))
  (check-type x real)
  (typecase x
    (float
      (multiple-value-bind
       (i e s) (integer-decode-float x)
       (let ((x (if (>= e 0) (ash i e) (/ i (ash 1 (- e))))))
	 (if (>= s 0) x (- x)))))
    (rational x)))
(setf (symbol-function 'rationalize) (symbol-function 'rational))

(defun ctp-bnd (x tp inc &aux (a (atom x))(nx (if a x (car x))))
  (cond ((isinf nx) (when (or (< nx 0 inc) (< inc 0 nx)) '*))
	((case tp
	   ((immfix bfix bignum integer)
	    (let* ((nx (if (unless a (integerp nx)) (+ nx inc) nx))
		   (nx (if (eq nx '*) nx (if (> inc 0) (ceiling nx) (floor nx)))))
	      (case tp
		(immfix (adj-bndd nx inc most-negative-immfix most-positive-immfix))
		(bfix (adj-bndd nx inc most-negative-fixnum most-positive-fixnum
				#.(1- most-negative-immfix) #.(1+ most-positive-immfix)))
		(bignum (adj-bndd nx inc '* '* #.(1- most-negative-fixnum) #.(1+ most-positive-fixnum)))
		(integer nx))))
	   (ratio (let ((z (if (eq nx '*) nx (rational nx)))) (if (unless (integerp z) a) z (list z))))
	   (short-float (let ((z (if (eq nx '*) nx (float nx 0.0s0)))) (if a z (list z))))
	   (long-float (let ((z (if (eq nx '*) nx (float nx 0.0)))) (if a z (list z))))))))

(defun number-range-fixup (x tp)
  (lremove-if-not (lambda (x) (let* ((a (car x))(d (cdr x))
				     (la (listp a))(na (if la (car a) a))
				     (ld (listp d))(nd (if ld (car d) d)))
				(when (and na nd)
				  (or (eq na '*) (eq nd '*) (< na nd) (when (= na nd) (unless la (not ld)))))))
	      (mapl (lambda (x) (let* ((x (car x)))
				  (setf (car x) (ctp-bnd (car x) tp +1) (cdr x) (ctp-bnd (cdr x) tp -1)))) x)))

(defun range-load (ntp type)
  (ntp-ld ntp (list (car type) (number-range-fixup (list (cons (cadr type) (caddr type))) (car type)))))

;; (defun range-load (ntp type)
;;   (let* ((z `((,(cadr type) . ,(caddr type))))
;; 	 (z (number-range-fixup z (car type))))
;;     (ntp-ld ntp (let ((z `(,(car type) ,z))) z))))

(defun elgt (x y)
  (cond ((or (eq (car x) '*) (eq (cdr y) '*)) nil)
	((and (integerp (car x)) (integerp (cdr y))) (> (car x) (1+ (cdr y))))
	((and (listp (car x)) (listp (cdr y))) (>= (range-num (car x)) (range-num (cdr y))))
	((> (range-num (car x)) (range-num (cdr y))))))

(defun elin (x y op)
  (cond ((eq x '*) y)
	((eq y '*) x)
	((= (range-num x) (range-num y)) (if (atom x) y x))
	((funcall op (range-num x) (range-num y)) x)
	(y)))

(defun elout (x y op)
  (cond ((eq x '*) x)
	((eq y '*) y)
	((= (range-num x) (range-num y)) (if (atom x) x y))
	((funcall op (range-num x) (range-num y)) x)
	(y)))

(defun goodel (x y)
  (cond ((eq x '*))
	((eq y '*))
	((let ((ax (range-num x))
	       (ay (range-num y)))
	   (or (< ax ay) (and (= ax ay) (atom x) (atom y)))))))

(defun range-and (x y &optional res)
  (cond ((or (not x) (not y)) (nreverse res))
	((elgt (car y) (car x)) (range-and (cdr x) y res))
	((elgt (car x) (car y)) (range-and x (cdr y) res))
	((let* ((w (elin (caar x) (caar y) '>))
		(z (elin (cdar y) (cdar x) '<))
		(zy (eq (cdar y) z))
		(nr (if (goodel w z) (cons (cons w z) res) res)))
	   (range-and (if zy x (cdr x)) (if zy (cdr y) y) nr)))))

(defun maybe-or-push (x res)
  (cond ((not res) (list x))
	((elgt x (car res)) (cons x res))
	((cons (cons (elout (caar res) (car x) '<) (elout (cdr x) (cdar res) '>)) (cdr res)))))

;;FIXME handle cleanups here and in and
(defun range-or (x y &optional res)
  (cond ((and (not x) (not y)) (nreverse res))
	((not x) (range-or x (cdr y) (maybe-or-push (car y) res)))
	((not y) (range-or (cdr x) y (maybe-or-push (car x) res)))
	((elgt (car y) (car x)) (range-or (cdr x) y (maybe-or-push (car x) res)))
	((elgt (car x) (car y)) (range-or x (cdr y) (maybe-or-push (car y) res)))
	((range-or (cdr x) (cdr y)
		    (maybe-or-push (cons (elout (caar x) (caar y) '<) (elout (cdar y) (cdar x) '>)) res)))))

(defun range-not (x &optional res (last '* lastp))
  (cond ((not x) (nreverse (if (and lastp (eq last '*)) res (cons (cons (elcomp last) '*) res))))
	((range-not (cdr x) (if (eq (caar x) last) res (cons (cons (elcomp last) (elcomp (caar x))) res)) (cdar x)))))

(defun range-op (op x y)
  (case op
	(and (setf (car (cdr x)) (range-and (cadr x) (cadr y))))
	(or (setf (car (cdr x)) (range-or (cadr x) (cadr y))))
	(not (setf (car (cdr x)) (range-not (cadr x))))))

(defun range-recon (x)
  (let ((z (mapcar (lambda (y) `(,(car x) ,(car y) ,(cdr y))) (number-range-fixup (cadr x) (car x)))))
    (if (cdr z) `(or ,@z) (car z))))

;; COMPLEX

(defun minmax (i1 i2 low-p e &aux (fn (if low-p (if e '< '>) (if e '> '<))))
  (cond ((eq i1 '*) (if e i1 i2))
	((eq i2 '*) (if e i2 i1))
	((funcall fn i1 i2) i1)
	(i2)))

(defun expand-range (low high bottom top)
  (let ((low (minmax low bottom t t))(high (minmax high top nil t)))
    (when (or (eq low '*) (eq high '*) (<= low high)) (list low high))))

(defun nc (tp)
  (when (consp tp)
    (case (car tp)
	  (immfix (let ((m (cadr tp))(x (caddr tp))) 
		    (list (list 'integer (if (eq m '*) most-negative-immfix m) (if (eq x '*) most-positive-immfix x)))))
	  (bfix (let* ((m (cadr tp))(x (caddr tp))(m (if (eq m '*) most-negative-fixnum m))(x (if (eq x '*) most-positive-fixnum x))) 
		  (if (< (* m x) 0)
		      `((integer ,m ,(1- most-negative-immfix))(integer ,(1+ most-positive-immfix) ,x))
		    `((integer ,m ,x)))))
	  (bignum (let* ((m (cadr tp))(x (caddr tp))(sm (or (eq m '*) (< m 0)))(sx (or (eq x '*) (>= x 0))))
		    (if (and sm sx)
			`((integer ,m ,(1- most-negative-fixnum))(integer ,(1+ most-positive-fixnum) ,x))
		      `((integer ,m ,x)))))
	  ((ratio short-float long-float) (list tp))
	  (otherwise (append (nc (car tp)) (nc (cdr tp)))))))

;; (defun nc (tp)
;;   (cond ((atom tp) nil)
;; 	((case (car tp)
;; 	       (immfix (let ((m (cadr tp))(x (caddr tp))) 
;; 			 (list (list 'integer (if (eq m '*) most-negative-immfix m) (if (eq x '*) most-positive-immfix x)))))
;; 	       (bfix (let ((m (cadr tp))(x (caddr tp))) 
;; 			 (list (list 'integer (if (eq m '*) most-negative-fixnum m) (if (eq x '*) most-positive-fixnum x)))))
;; 	       (bignum (list (cons 'integer (cdr tp))))
;; 	       ((ratio short-float long-float) (list tp))
;; 	       (otherwise (append (nc (car tp)) (nc (cdr tp))))))))

(defun expand-ranges (type)
  (lreduce (lambda (y x &aux (z (assoc (car x) y)))
	     (if z (subst (cons (car z) (apply 'expand-range (cadr x) (caddr x) (cdr z))) z y)
	       (cons x y))) (nc type) :initial-value nil))

(defun group-complex-types (type)
  (lreduce (lambda (y x &aux (tp (pop x))(x (cons (car x) (cadr x)))(z (assoc tp y)))
	     (if z (subst (cons tp (cons x (cdr z))) z y)
	       (cons (list tp x) y))) (nc type) :initial-value nil))

(defun complex-load-sub (ntp z &aux (tp (pop z))(q (cdr (assoc tp +complex-type-alist+)))
			     (z (lreduce (lambda (y x) (range-or y (number-range-fixup (list x) tp))) z :initial-value nil)))
  (ntp-ld ntp (list q z)))

(defun complex-load (ntp type)
  (mapc (lambda (x) (complex-load-sub ntp x)) (group-complex-types (cadr type))))


;; (defun complex-load (ntp type)
;;   (mapc (lambda (x &aux (tp (pop x)) (z (number-range-fixup (list (cons (car x) (cadr x))) tp)))
;; 	  (ntp-ld ntp `(,(cdr (assoc tp +complex-type-alist+)) (,z . ,z))))
;; 	(expand-ranges (cadr type))))

(defun complex-op (op x y)
  (let ((z
	 (case op
	       (and (range-and (cadr x) (cadr y)))
	       (or  (range-or  (cadr x) (cadr y)))
	       (not (range-not (cadr x) (cadr y))))))
    (setf (car (cdr x)) z)))

;; (defun complex-op (op x y)
;;   (let ((z
;; 	 (case op
;; 	       (and (cons (range-and (caadr x) (caadr y)) (range-and (cdadr x) (cdadr y))))
;; 	       (or  (cons (range-or  (caadr x) (caadr y)) (range-or  (cdadr x) (cdadr y))))
;; 	       (not (cons (range-not (caadr x) (caadr y)) (range-not (cdadr x) (cdadr y)))))))
;;     (setf (car (cdr x)) (and (car z) (cdr z) z))))

(defun complex-recon (x)
  (let* ((tp (car (rassoc (car x) +complex-type-alist+)))
	 (z (mapcar (lambda (x) (list tp (car x) (cdr x))) (number-range-fixup (cadr x) tp))))
    `(complex ,(if (cdr z) (cons 'or z) (car z)))))

;; (defun complex-recon (x)
;;   (let* ((tp (car (rassoc (car x) +complex-type-alist+)))
;; 	 (z (range-or (caadr x) (cdadr x)))
;; 	 (z (number-range-fixup z tp))
;; 	 (z (mapcar (lambda (y) `(complex (,tp ,(car y) ,(cdr y)))) z)))
;;     (if (cdr z) `(or ,@z) (car z))))


;; SINGLETON-TYPES

(defun single-load (ntp type)
  (ntp-ld ntp `(,(car type) ,(or (cadr type) t))))

(defun single-atm (x)
  (cond ((or (eq x t) (not x)))
	((and (consp x) (eq (car x) 'member)))
	((and (consp x) (eq (car x) 'not)) (single-atm (cadr x)))))

(defun ordered-intersection-eq (l1 l2)
  (let (z zt)
    (do ((l l1 (cdr l))) ((not l))
      (when (memq (car l) l2)
	(setf zt (let ((p (cons (car l) nil))) (if zt (cdr (rplacd zt p)) (setf z p))))))
    z))

(defun ordered-intersection (l1 l2)
  (let (z zt)
    (do ((l l1 (cdr l))) ((not l))
      (when (member (car l) l2)
	(setf zt (let ((p (cons (car l) nil))) (if zt (cdr (rplacd zt p)) (setf z p))))))
    z))

(defun single^ (x y)
  (cond ((eq x t) y)
	((eq y t) x)
	((and x y) (let ((z (ordered-intersection (cdr x) (cdr y)))) (when z `(member ,@z))))))

(defun single-op (op x y)
  (setf (car (cdr x))
	(case op
	      ((and or) (sigalg-atom-op op (cadr x) (cadr y) 'single^ 'single-atm))
	      (not (negate (cadr x))))))

(defun single-recon (x)
  (cond ((eq (cadr x) t) (car x))
	((and (consp (cadr x)) (eq (caadr x) 'not)) `(and ,(car x) ,(cadr x)))
	((cadr x))))


;; ARRAY TYPES

(defun expand-array-element-type (type)
   (or (car (member type +array-types+ :test (lambda (x y) (unless (eq y t) (subtypep x y))))) t))

#.`(defun upgraded-array-element-type (type &optional environment)
     (declare (ignore environment) (optimize (safety 1)))
     (case type
	   ((nil t) type)
	   ,@(mapcar (lambda (x) `(,x type)) (cons '* (lremove t +array-types+)))
	   (otherwise (expand-array-element-type type))))

(defun array-load (ntp type)
  (let* ((z (upgraded-array-element-type (cadr type)))
	 (z (if (eq z '*) z (car (member z +array-types+)))))
    (unless (or z (not (cadr type))) (error "Bad array type ~a~%" (cadr type)))
    (let* ((dim (caddr type))(dim (or dim 0))
	   (dim (cond ((eq dim '*) t)
		      ((integerp dim) (if (= 0 dim) dim (make-list dim :initial-element t)))
		      ((listp dim) (lsubstitute t '* dim)))))
      (cond ((eq z '*) 
	     (dolist (l +array-type-alist+)
	       (ntp-ld ntp `(,(cdr l) ,(when z dim)))))
	    (z (ntp-ld ntp `(,(cdr (assoc z +array-type-alist+)) ,dim)))))))
  
(defun array-atm (x)
  (cond ((atom x))
	((eq (car x) 'not) (array-atm (cadr x)))
	((integerp (car x)))
	((eq (car x) 'member))
	((eq (car x) t))))

(defun array^ (x y)
  (cond ((not (and x y)) nil)
	((eq y t) x)
	((eq x t) y)
	((and (integerp x) (integerp y)) (when (= x y) x))
	((integerp x) (when (and (consp y) (= x (length y))) y))
	((integerp y) (array^ y x))
	((and (eq (car x) 'member) (eq (car y) 'member))
	 (let ((q (ordered-intersection-eq (cdr x) (cdr y))))
	   (when q `(member ,@q))))
	((eq (car x) 'member)
	 (let* ((y (lsubstitute '* t y))
		(q (lremove-if-not (lambda (x) 
				     (labels ((m (x y &aux (cy (car y))) 
						 (if (when x (or (eql (car x) cy) (eq cy '*)))
						     (m (cdr x) (cdr y))
						   (not y))))
					     (m (array-dimensions x) y))) (cdr x))))
	   (when q `(member ,@q))))
	((and (consp y) (eq (car y) 'member)) (array^ y x))
	((/= (length x) (length y)) nil)
	((mapcar 'array^ x y))))

(defun array-op (op x y)
  (setf (car (cdr x))
	(case op
	      ((and or) (sigalg-op op (cadr x) (cadr y) 'array^ 'array-atm))
	      (not (negate (cadr x))))))

(defun array-recon (x &aux (tp (pop x))(x (car x))) 
  (cond ((not (array-atm x))
	 (cons 'or (mapcar (lambda (z) (array-recon `(,tp ,z))) (cdr x))))
	((and (consp x) (eq (car x) 'member) x))
	((when (consp x) (eq (car x) 'not)) 
	 `(and ,(array-recon `(,tp *)) (not ,(array-recon `(,tp ,(cadr x))))))
	(`(array ,(car (rassoc tp +array-type-alist+)) 
		 ,(cond ((eq x t) '*)
			((atom x) x) 
			((lsubstitute '* t x)))))))

;; STRUCTURES

(defun structure-load (ntp type) (single-load ntp (cons (car type) (mapcar (lambda (x) (get x 's-data)) (cdr type)))))

(defun structure-atm (x)
  (cond ((atom x))
	((eq (car x) 'not) (structure-atm (cadr x)))
	((eq (car x) 'member))))

(defun structure^ (x y)
  (cond ((not (and x y)) nil)
	((eq x t) y)
	((eq y t) x)
	((and (consp x) (eq (car x) 'member))
	 (let ((q (lremove-if-not (lambda (z) (typep z y)) (cdr x))))
	   (when q `(member ,@q))))
	((and (consp y) (eq (car y) 'member)) (structure^ y x))
	((do ((z x (s-data-includes z))) ((or (not z) (eq z y)) (and z x))))
	((do ((z y (s-data-includes z))) ((or (not z) (eq z x)) (and z y))))))

(defun structure-op (op x y)
  (setf (car (cdr x))
	(case op
	      ((and or) (sigalg-op op (cadr x) (cadr y) 'structure^ 'structure-atm))
	      (not (negate (cadr x))))))

(defun structure-recon (x) (single-recon x))

;; CLASS HACKS

(eval-when
 (compile eval)
 (defmacro clh nil
  `(progn
     ,@(mapcar (lambda (x &aux (f (when (eq x 'find-class) `(&optional ep))) (z (intern (string-concatenate "SI-" (symbol-name x)))))
		 `(defun ,z (o ,@f &aux e)
		    (cond ((and (fboundp ',x) (fboundp 'classp))
			   (prog1 (funcall ',x o ,@(cdr f))
			     (fset ',z (symbol-function ',x))
;			     (setf (symbol-function ',z) (symbol-function ',x))
			     ))
			  ((setq e (get ',z 'early)) (values (funcall e o ,@(cdr f)))))))
	       '(classp class-precedence-list find-class class-name class-of class-direct-subclasses)))))
(clh)

(defun is-standard-class (object)
  (and (si-classp object)
       (member (si-find-class 'standard-object) (si-class-precedence-list object))
       object))

(defun find-standard-class (object)
  (when (symbolp object)
    (is-standard-class (si-find-class object nil))))

(defun coerce-to-standard-class (object)
  (cond ((is-standard-class object))
	((find-standard-class object))))

;; STANDARD-OBJECTS

(defun standard-load (ntp type) (single-load ntp type))

(defun standard-atm (x) (structure-atm x))

(defun standard^ (x y)
  (cond ((not (and x y)) nil)
	((eq x t) y)
	((eq y t) x)
	((eq x y) x) ;needed for early generic function processing
	((and (consp x) (eq (car x) 'member))
	 (let ((q (lremove-if-not (lambda (z) (typep z y)) (cdr x))))
	   (when q `(member ,@q))))
	((and (consp y) (eq (car y) 'member)) (standard^ y x))
	((member y (si-class-precedence-list x)) x)
	((member x (si-class-precedence-list y)) y)
	((lreduce (lambda (&rest xy) (when xy (standard^ (car xy) (cadr xy))))
		 (ordered-intersection-eq (si-class-direct-subclasses x) (si-class-direct-subclasses y))))))

(defun standard-op (op x y)
  (setf (car (cdr x))
	(case op
	      ((and or) (sigalg-op op (cadr x) (cadr y) 'standard^ 'standard-atm))
	      (not (negate (cadr x))))))

(defun standard-recon (x)
  (let ((z (or (si-find-class (car x) nil) (car x))))
    (cond ((eq (cadr x) t) z)
	  ((and (consp (cadr x)) (eq (caadr x) 'not)) `(and ,z ,(cadr x)))
	  ((cadr x)))))

(deftype fixnum (&optional (low '*) (high '*)) `(or (immfix ,low ,high) (bfix ,low ,high)))
(deftype integer (&optional (low '*) (high '*)) `(or (fixnum ,low ,high) (bignum ,low ,high)))

;; CONS

(defun cons-load (ntp type)
  (ntp-ld ntp `(cons (,(nprocess-type (cadr type)) . ,(nprocess-type (caddr type))))))

(defun cons-atm (x)
  (cond ((not x))
	((eq x t))
	((eq x 'proper-cons))
	((eq (car x) 'not) (cons-atm (cadr x)))
	((and (consp x) (eq (car x) 'member)))
	((and (consp x) 
	      (consp (car x)) (listp (caar x)) (= (length (car x)) 3)
	      (consp (cdr x)) (listp (cadr x)) (= (length (cdr x)) 3)))))

(defun ntp-to-nil (ntp)
  (when (or (car ntp) (cadr ntp))
    ntp))

(defun cons-to-nil (cntp)
  (let* ((at (or (atom cntp) (not (cons-atm cntp)) (eq (car cntp) 'member)))
	 (car (or at (atom (car cntp)) (ntp-to-nil (car cntp))))
	 (cdr (or at (atom (cdr cntp)) (ntp-to-nil (cdr cntp)))))
    (and car cdr cntp)))

(defun cons^ (x y)
  (cond ((eq x t) y)
	((eq y t) x)
	((not (and x y)) nil)
	((and (eq x 'proper-cons) (eq y 'proper-cons)) x)
	((and (eq x 'proper-cons) (eq (car y) 'member))
	 (let ((q (lremove-if-not 'proper-consp (cdr y))))
	   (when q `(member ,@q))))
	((eq x 'proper-cons)
	 (let* ((z '(((null (member nil)) (cons proper-cons)) nil nil))
		(cy (copy-tree (cdr y)))
		(ca (ntp-and cy z)))
	   (if (equal z ca) x
	     (cons-to-nil (cons (copy-tree (car y)) ca)))));FIXME
	((eq y 'proper-cons)
	 (cons^ y x))
	((and (eq (car x) 'member) (eq (car y) 'member))
	 (let ((q (ordered-intersection-eq (cdr x) (cdr y))))
	   (when q `(member ,@q))))
	((eq (car x) 'member)
	 (let ((q (lremove-if-not
		   (lambda (x) 
		     (cons^ (cons (ntp-load `(member ,(car x))) 
				  (ntp-load `(member ,(cdr x)))) y)) (cdr x))))
	   (when q `(member ,@q))))
	((eq (car y) 'member) (cons^ y x))
	((and x y)
	 (let ((ax (copy-tree (car x)))
	       (dx (copy-tree (cdr x))))
	   (cons-to-nil
	    (cons (ntp-and ax (car y))
		  (ntp-and dx (cdr y))))))))

(defun cons-op (op x y &aux (w (cadr x)))
  (setf (car (cdr x))
	(case op
	      ((and or) (cons-to-nil (sigalg-op op w (cadr y) 'cons^ 'cons-atm)))
	      (not
	       (cond ((not w))
		     ((eq w t) nil)
		     ((eq w 'proper-cons) (negate w))
		     ((eq (car w) 'member) (negate w))
		     ((eq (car w) 'not) (cadr w))
		     ((cons-atm w)
		      (let ((car (ntp-to-nil (ntp-not (copy-tree (car w)))));FIXME
			    (cdr (ntp-to-nil (ntp-not (copy-tree (cdr w))))))
			(cond ((and car cdr) 
			       (cons-to-nil 
				(sigalg-op 'or `(,car . ,(nprocess-type '(t))) 
					   `(,(nprocess-type '(t)) . ,cdr) 'cons^ 'cons-atm)))
			      (car (cons car (nprocess-type '(t))))
			      (cdr (cons (nprocess-type '(t)) cdr)))))
		     (t (lreduce (lambda (zx zy) 
				  (cons-to-nil
				   (sigalg-op 
				    (if (eq (car w) 'or) 'and 'or) zx zy 'cons^ 'cons-atm)))
				(mapcar (lambda (x) (cons-op 'not `(cons ,x) nil)) (cdr w))
				:initial-value (eq (car w) 'or))))))))

(defun cons-recon (x &aux (w (cadr x)))
  (cond ((eq t w) `(cons t t))
	((eq w 'proper-cons) w)
	((atom w) nil)
	((eq (car w) 'member) w)
	((eq (car w) 'not)
	 (let ((r (cons-recon `(cons ,(cadr w))))) 
	   (if (and (consp r) (eq (car r) 'cons))
	       `(not ,r)
	     `(and (cons t t) (not ,r)))))
	((cons-atm w)
	 (let ((car (nreconstruct-type-int (copy-tree (car w))));FIXME
	       (cdr (nreconstruct-type-int (copy-tree (cdr w)))))
	   (and car cdr (if (and (eq car t) 
				 (or (eq cdr 'proper-cons)
				     (equal cdr '(or (member nil) proper-cons))
				     (equal cdr '(or proper-cons (member nil)))));FIXME
			    'proper-cons `(cons ,car ,cdr)))))
	((let* ((z (lremove-if 'not (mapcar (lambda (x) (cons-recon `(cons ,x))) (cdr w)))))
	   (if (cdr z) `(,(car w) ,@z) (car z))))))


(defun and-or-flatten (tp &aux (ctp (when (listp tp) (car tp))))
  (if (member ctp '(and or))
      (let ((x (mapcan (lambda (x &aux (x (and-or-flatten x)))
			 (cond ((when (listp x) (eq (car x) ctp)) (cdr x))
			       ((eq x (eq ctp 'and)) nil)
			       ((list x)))) (cdr tp))))
	(cond ((not x) (when (eq ctp 'and) '(t))) ((not (cdr x)) (car x)) ((cons ctp x))))
    tp))

(defun get-included (name)
  (let ((sd (get name 's-data)))
    (cons (sdata-name sd) (mapcan 'get-included (sdata-included sd)))))

(defun expand-deftype (type &aux (atp (listp type)) (ctp (if atp (car type) type)) (tp (when atp (cdr type))))
  (cond ((unless (symbolp ctp) (si-classp ctp)) (or (valid-class-name ctp) `(std-instance ,ctp)));FIXME classp loop, also accept s-data?
	((let ((tem (get ctp 's-data))) (when tem (null (sdata-type tem)))) `(structure ,ctp)); (cons 'structure (get-included ctp))
	((let ((tem (get ctp 'deftype-definition)))
	   (when tem
	     (let ((ntype (apply tem tp)))
	       (unless (eq ctp (if (listp ntype) (car ntype) ntype))
		 ntype)))))))


#.`(defun normalize-type (type &aux (lp (listp type))(ctp (if lp (car type) type))(tp (when lp (cdr type))))

     (case ctp
	   (,+range-types+ (if (cdr tp) type (list ctp (or (car tp) '*) (or (cadr tp) '*))))
	   (complex (let ((s (normalize-type (if (when tp (not (eq (car tp) '*))) (car tp) 'real))))
		      (if (when tp (eq s (car tp))) type `(complex ,s)))) 
	   (array (if (cdr tp) type (list ctp (if tp (car tp) '*) '*)))
	   (member type)
	   (cons
	    (let* ((def '(t))
		   (a (if tp (normalize-type (car tp)) def))
		   (d (if (cdr tp) (normalize-type (cadr tp)) def)))
	      (if (when (cdr tp) (and (eq a (car tp)) (eq d (cadr tp)))) type (list ctp a d))))
	   (,(lremove-if (lambda (x) (member x '(null true))) +singleton-types+) (if lp type (list type)))
	   (structure-object `(structure))
	   (satisfies (if (get (car tp) 'predicate-type) (list (get (car tp) 'predicate-type)) type));FIXME
	   (not	 (let ((x (normalize-type (car tp))))
		   (if (atom x) (cons 'not x)
		     (case (car x)
			   (not (cadr x))
			   (and (cons 'or (mapcar (lambda (x) `(not ,x)) (cdr x))))
					;		   (or (cons 'and (cdr x)))
			   (otherwise `(not ,x))))))
	   ((and or) (and-or-flatten (cons ctp (mapcar 'normalize-type tp))))
	   ((type-min type-max) (list ctp (normalize-type (car tp))))
	   ((t nil proper-cons) (if lp type (list type)))
	   (otherwise (or (normalize-type (expand-deftype type))
			  (unless (get ctp 'deftype-definition) '(unknown))))))

(defun ntp-load (type &aux tem)
  (let ((ntp (make-ntp)))
    (cond ((eq (car type) t) (ntp-not ntp))
	  ((eq (car type) 'proper-cons) (ntp-ld ntp (list 'cons 'proper-cons)))
	  ((setq tem (cdr (assoc (car type) +kingdom-load-ops-alist+)))
	   (funcall tem ntp type))
	  ((setq tem (coerce-to-standard-class (car type)))
	   (let ((s (load-time-value nil))(q (load-time-value nil)))
	     (setq s (or s (coerce-to-standard-class 'generic-function)) q (or q (si-class-precedence-list s)))
	     (cond  ((member s (si-class-precedence-list tem)) 
		     (ntp-ld ntp (list 'standard-generic-compiled-function (if (eq s tem) t tem)))
		     (ntp-ld ntp (list 'standard-generic-interpreted-function (if (eq s tem) t tem))))
		    ((member tem q) 
		     (ntp-ld ntp (list 'standard-generic-interpreted-function t))
		     (ntp-ld ntp (list 'standard-generic-compiled-function t))
		     (ntp-ld ntp (list 'std-instance t)))
		    ((ntp-ld ntp (list 'std-instance tem))))))
	  ((and (symbolp (car type)) (setq tem (get (car type) 's-data)))
	   (ntp-ld ntp `(structure ,tem)))
	  ((eq (car type) 'member)
	   (let ((els (cdr type)))
	     (dolist (l +known-types+)
	       (let ((z (lremove-if-not (lambda (x) (case x ((nil) (eq l 'null)) ((t) (eq l 'true)) (otherwise (typep x l)))) els)))
		 (when z
		   (setq els (set-difference els z))
		   (let* ((z (cond ((member l +range-types+)
				    (lreduce 'range-or
					    (mapcar
					     (lambda (x) 
					       (number-range-fixup `((,x . ,x)) l)) z)))
				   ((and (consp l) (eq (car l) 'complex))
				    (lreduce 'range-or
					     (mapcar 
					      (lambda (x) 
						(let ((q (realpart x))(r (imagpart x)))
						  (number-range-fixup
						   `((,(min q r) . ,(max q r)))
						   (cadr l)))) z)))
				   (`(member ,@z))))
			  (lst (and (consp l) (if (eq (car l) 'array) +array-type-alist+ +complex-type-alist+)))
			  (z (and z `(,(if lst (cdr (assoc (cadr l) lst)) l) ,z))))
		     (ntp-ld ntp z)))))
	     (when els (let ((ntp (ntp-not ntp))) (setf (car (cddr ntp)) t)))))
	  ((car type) (let ((ntp (ntp-not ntp))) (setf (car (cddr ntp)) t))))
    ntp))

(defun nprocess-type (type)
  (cond	
   ((eq (car type) 'type-min) (let ((*tp-mod* -1)) (nprocess-type (cadr type))))
   ((eq (car type) 'type-max) (let ((*tp-mod*  1)) (nprocess-type (cadr type))))
   ((eq (car type) 'and)  (lreduce 'ntp-and (mapcar (lambda (x) (nprocess-type x)) (cdr type))))
   ((eq (car type) 'or)   (lreduce 'ntp-or (mapcar (lambda (x) (nprocess-type x)) (cdr type))))
   ((eq (car type) 'not)  (ntp-not (nprocess-type (cadr type))))
   ((ntp-load type))))

(defun prune-type (z q i w) ;FIXME optional tail recursion
  (declare (seqind i))
  (cond ((= i (length +array-type-alist+))
	 (setf (car (cdar q)) '*)
	 (ldelete-if (lambda (y) (unless (eq y (car q)) (and (consp y) (eq (car y) 'array)))) z))
	((not w) z)
	((or (atom (car w)) (not (eq (caar w) 'array))) (prune-type z q i (cdr w)))
	((not q) (prune-type z w (1+ i) (cdr w)))
	((equal (caddar w) (caddar q)) (prune-type z q (1+ i) (cdr w)))
	(z)))

(defun nreconstruct-type-int (x)
  (cond ((caddr x) t)
	((cadr x) (let ((z (nreconstruct-type-int (ntp-not x))))
		    (or (not z) `(not ,z))))
	((let* ((z (mapcar (lambda (x) (funcall (cdr (assoc (car x) +kingdom-recon-ops-alist+)) x)) (car x)))
		(z (prune-type z nil 0 z)))
	(and-or-flatten (if (cdr z) `(or ,@z) (car z)))))))

(defun nreconstruct-type (x)
  (list (nreconstruct-type-int x) (caddr x)))

(defun resolve-type (type)
  (nreconstruct-type (nprocess-type (normalize-type type))))

(defun subtypep1 (t1 t2)
  (or (not t1) (eq t2 t)
    (let* ((rt (resolve-type `(and ,t1 ,(negate t2))))
	   (mt (when (cadr rt) (resolve-type `(and (type-max ,t1) ,(negate `(type-min ,t2))))))
	   (rt (when (or (not (cadr rt)) (car mt)) rt)))
      (not (car rt)))))
    
(defun subtypep (t1 t2 &optional env)
  (declare (ignore env) (optimize (safety 2)))
  (check-type t1 type-spec)
  (check-type t2 type-spec)
  (if (or (not t1) (eq t2 t))
      (values t t)
    (let* ((rt (resolve-type `(and ,t1 ,(negate t2))))
	   (mt (when (cadr rt) (resolve-type `(and (type-max ,t1) ,(negate `(type-min ,t2))))))
	   (rt (if (or (not (cadr rt)) (car mt)) rt `(nil nil))))
      (values (not (car rt))  (not (cadr rt))))))

(deftype seqind (&aux (s (1- array-dimension-limit))) `(,(if (<= s most-positive-immfix) 'immfix 'fixnum) 0 ,s))
(deftype rnkind (&aux (s array-rank-limit)) `(,(if (<= s most-positive-immfix) 'immfix 'fixnum) 0 ,s))
(deftype mod (n) `(,(cond ((<= (1- n) most-positive-immfix) 'immfix)((<= (1- n) most-positive-fixnum) 'fixnum)('integer))
		   0 ,(1- n)))
(deftype bit () `(mod 2))
(deftype non-negative-byte (&optional (s '*)) `(unsigned-byte ,(1- s)))
(deftype negative-byte (&optional (s '*)) (normalize-type `(integer  ,(if (eq s '*) s (- (ash 1 (1- s)))) -1)))
(deftype signed-byte (&optional (s '*)) (normalize-type `(integer ,(if (eq s '*) s (- (ash 1 (1- s)))) ,(if (eq s '*) s (1- (ash 1 (1- s)))))))
(deftype unsigned-byte (&optional (s '*)) (normalize-type `(integer 0 ,(if (eq s '*) s (1- (ash 1 s))))))

;; set by unixport/init_kcl.lsp
;; warn if a file was comopiled in another version
(defvar *gcl-extra-version* nil)
(defvar *gcl-minor-version* nil)
(defvar *gcl-major-version* nil)

(defun warn-version (majvers minvers extvers)
  (and *gcl-major-version* *gcl-minor-version* *gcl-extra-version*
       (or (not (eql extvers *gcl-extra-version*))
	   (not (eql minvers *gcl-minor-version*))
	   (not (eql majvers *gcl-major-version*)))
       *load-verbose*
       (format t "[compiled in GCL ~a.~a.~a] " majvers minvers extvers)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./lsp/gcl_predlib.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




