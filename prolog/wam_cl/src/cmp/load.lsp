
(in-package 'compiler)

#+MCL
(progn
  (setf (logical-pathname-translations "ecl")
	'(("**;*.*.*" "PowerBook:Languages:ECoLisp:**:")))
  (setq *default-pathname-defaults* (parse-namestring "ecl:cmp;"))
  (setq *working-directory* (pathname "ecl:cmp;"))

  (defpackage "SI" (:shadow "DEFINE-COMPILER-MACRO"))
  (defpackage "COMPILER")
  (proclaim '(special
	      *compiler-output1* *compiler-output2*
	      *compiler-push-events* *compile-time-too*
	      *constants* *closures*
	      *destination* *env* *eval-when-compile* *exit*
	      *function-declarations* *funs*
	      *global-entries* *global-funs*
	      *last-label* *lex* *linking-calls* *local-funs*
	      *max-temp* *lcl*
	      *next-cvar* *next-cfun* *next-cmacro* *next-lcl* *temp*
	      *next-vv* *non-package-operation* *notinline*
	      *objects*
	      *register-min* *reservation-cmacro* *reservations*
	      *setjmps* *safe-compile* *sharp-commas*
	      *tags* *tail-recursion-info* *temp* *top-level-forms*
	      *undefined-vars* *unwind-exit*
	      *vars* *volatile*))
 (load "compat.lisp")
 )

(push ':debug *features*)
(in-package "COMPILER")

#+KCL (setq compiler:*compile-print* t)

(load "/project/lisp/ecl/src/cmp/cmpdefs.lsp")
(setq *c1nil* (list 'LOCATION (make-info :type (object-type nil)) nil))
(setq *c1t* (list 'LOCATION (make-info :type (object-type t)) t))
(setq *info* (make-info))

#+AKCL
(progn
  ;; otherwise it will not compile defmacro:
  (defun sys:clear-compiler-properties (symbol))
  ;; enable redefinition of system macros:
  (setq sys:*inhibit-macro-special* nil)
  (sys:*make-constant 'most-positive-fixnum 536870911)
  (sys:*make-constant 'most-negative-fixnum -536870912)
  (setq compiler::*compile-time-too* nil)
  (setq sys:*debug-print-level* 5)
  (setq compiler::*cmpinclude* "<ecl.h>")
  (setq compiler::*eval-when-defaults* '(eval load))
  (defun c2bind-loc (var loc) (bind var loc))
  (defun sys::expand-defmacro (&rest args) (apply 'sys:defmacro* args))
  ;; poiche' AKCL ha una diversa get-output-pathname:
  (defun disassemble (&rest system::args &aux (*print-pretty* nil))
    (apply 'compiler::disassemble1 system::args))
  (defsetf get (s p) (v) `(sys::putprop ,s ,v ,p))
  ;; AKCL bug:
  (defun var-p (x)
    (and (vectorp x)
	 (> (length x) 3)
	 ;; AKCL has (aref (the (vector t) x) 3)
	 (eq (elt x 3) 'VAR)))
  (defun sys::put-properties (sym &rest pairs)
    (do ((prop)) ((null pairs))
      (setq prop (pop pairs))
      (sys::putprop sym (pop pairs) prop)))
  (setq sys:*notify-gbc* t)
  (defun sys::read-bytes (stream string start end)
    (sys::fread string start (- end start) stream))
  (defun sys::write-bytes (stream string start end)
    (sys::fwrite string start (- end start) stream))
  )

(load "/project/lisp/ecl/src/cmp/cmpinline")
(load "/project/lisp/ecl/src/cmp/cmputil")
(load "/project/lisp/ecl/src/cmp/cmptype")
(load "/project/lisp/ecl/src/cmp/cmpbind")
(load "/project/lisp/ecl/src/cmp/cmpblock")
(load "/project/lisp/ecl/src/cmp/cmpcall")
(load "/project/lisp/ecl/src/cmp/cmpcatch")
(load "/project/lisp/ecl/src/cmp/cmpenv")
(load "/project/lisp/ecl/src/cmp/cmpeval")
(load "/project/lisp/ecl/src/cmp/cmpexit")
(load "/project/lisp/ecl/src/cmp/cmpflet")
(load "/project/lisp/ecl/src/cmp/cmpfun")
(load "/project/lisp/ecl/src/cmp/cmpif")
(load "/project/lisp/ecl/src/cmp/cmplam")
(load "/project/lisp/ecl/src/cmp/cmplet")
(load "/project/lisp/ecl/src/cmp/cmploc")
(load "/project/lisp/ecl/src/cmp/cmpmap")
(load "/project/lisp/ecl/src/cmp/cmpmulti")
(load "/project/lisp/ecl/src/cmp/cmpspecial")
(load "/project/lisp/ecl/src/cmp/cmptag")
(load "/project/lisp/ecl/src/cmp/cmptop")
(load "/project/lisp/ecl/src/cmp/cmpvar")
(load "/project/lisp/ecl/src/cmp/cmpwt")
(load "/project/lisp/ecl/src/cmp/cmpmain")
#-ecl
(load "/project/lisp/ecl/src/cmp/sysfun")
;(load "/project/lisp/ecl/src/cmp/test")

