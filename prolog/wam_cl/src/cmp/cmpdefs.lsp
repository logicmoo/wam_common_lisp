;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPDEF	Definitions

(in-package 'compiler)

;;; Use structures of type vector to avoid creating
;;; normal structures before booting CLOS.

(defstruct (ref (:type vector))
  name			;;; Identifier of reference.
  (ref 0 :type fixnum)	;;; Number of references.
  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the index into the closure env
)

(defstruct (var (:type vector) (:include ref) :named)
;  name		;;; Variable name.
;  (ref 0 :type fixnum)
		;;; Number of references to the variable (-1 means IGNORE).
		;;; During Pass 2: set below *register-min* for non register.
;  ref-ccb	;;; Cross closure reference: T or NIL.
  kind		;;; One of LEXICAL, SPECIAL, GLOBAL, OBJECT, FIXNUM,
  		;;; CHARACTER, LONG-FLOAT, SHORT-FLOAT, or REPLACED (used for
		;;; LET variables).
		;;; A value DUMMY is used for missing supplied-p keyword
		;;; variables
  (loc 'OBJECT)	;;; During Pass 1: indicates whether the variable can
		;;; be allocated on the c-stack: OBJECT means
		;;; the variable is declared as OBJECT, and CLB means
		;;; the variable is referenced across Level Boundary and thus
		;;; cannot be allocated on the C stack.  Note that OBJECT is
		;;; set during variable binding and CLB is set when the
		;;; variable is used later, and therefore CLB may supersede
		;;; OBJECT.
		;;; During Pass 2:
  		;;; For REPLACED: the actual location of the variable.
  		;;; For FIXNUM, CHARACTER, LONG-FLOAT, SHORT-FLOAT, OBJECT:
  		;;;   the cvar for the C variable that holds the value.
  		;;; For LEXICAL: the frame-relative address for the variable.
		;;; For SPECIAL and GLOBAL: the vv-index for variable name.
  (type t)	;;; Type of the variable.
  )

;;; A function may be compiled into a CFUN, CCLOSURE or CCLOSURE+LISP_CLOSURE
;;; Here are examples of function FOO for the 3 cases:
;;; 1.  (flet ((foo () (bar))) (foo))		CFUN
;;; 2.  (flet ((foo () (bar))) #'foo)		CFUN+LISP_CFUN
;;; 3.  (flet ((foo () x)) #'(lambda () (foo))) CCLOSURE
;;; 4.  (flet ((foo () x)) #'foo)		CCLOSURE+LISP_CLOSURE

;;; A function can be referred across a ccb without being a closure, e.g:
;;;   (flet ((foo () (bar))) #'(lambda () (foo)))
;;;   [the lambda also need not be a closure]
;;; and it can be a closure without being referred across ccb, e.g.:
;;;   (flet ((foo () x)) #'foo)  [ is this a mistake in c1local-closure?]
;;; Here instead the lambda must be a closure, but no closure is needed for foo
;;;   (flet ((foo () x)) #'(lambda () (foo)))
;;; So we use two separate fields: ref-ccb and closure.
;;; A CCLOSURE must be created for a function when:
;;; 1. it appears within a FUNCTION construct and
;;; 2. it uses some ccb references (directly or indirectly).
;;; ref-ccb corresponds to the first condition, i.e. function is referred
;;;   across CCB. It is computed during Pass 1. A value of 'RETURNED means
;;;   that it is immediately within FUNCTION.
;;; closure corresponds to second condition and is computed in Pass 2 by
;;;   looking at the info-referred-vars and info-local-referred of its body.

;;; A LISP_CFUN or LISP_CLOSURE must be created when the function is returned.
;;; The LISP funob may then be referred locally or across LB or CB:
;;;     (flet ((foo (z) (bar z))) (list #'foo)))
;;;     (flet ((foo (z) z)) (flet ((bar () #'foo)) (bar)))
;;;     (flet ((foo (z) (bar z))) #'(lambda () #'foo)))
;;; therefore we need field funob.

(defstruct (fun (:type vector) (:include ref) :named)
;  name			;;; Function name.
;  (ref 0 :type fixnum)	;;; Number of references.
;  ref-ccb		;;; Cross closure reference.
 			;;; During Pass1, T or NIL.
  cfun			;;; The cfun for the function.
  (level 0)		;;; Level of lexical nesting for a function.
  (env 0)     		;;; Size of env of closure.
  closure		;;; During Pass2, T if env is used inside the function
  var			;;; the variable holding the funob
  )

(defstruct (blk (:type vector) (:include ref) :named)
;  name			;;; Block name.
;  (ref 0 :type fixnum)	;;; Number of references.
;  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the ccb-lex for the
			;;; block id, or NIL.
  ref-clb		;;; Cross local function reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the lex-address for the
			;;; block id, or NIL.
  exit			;;; Where to return.  A label.
  destination		;;; Where the value of the block to go.
  var			;;; Variable containing the block ID.
  )

(defstruct (tag (:type vector) (:include ref) :named)
;  name			;;; Tag name.
;  (ref 0 :type fixnum)	;;; Number of references.
;  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
  ref-clb		;;; Cross local function reference.
			;;; During Pass1, T or NIL.
  label			;;; Where to jump.  A label.
  unwind-exit		;;; Where to unwind-no-exit.
  var			;;; Variable containing frame ID.
  )

(defstruct (info (:type vector) :named)
  (changed-vars nil)	;;; List of var-objects changed by the form.
  (referred-vars nil)	;;; List of var-objects referred in the form.
  (type t)		;;; Type of the form.
  (sp-change nil)	;;; Whether execution of the form may change
			;;; the value of a special variable.
  (volatile nil)	;;; whether there is a possible setjmp. Beppe
;  (referred-tags nil)   ;;; Tags or block names referenced in the body.
  (local-referred nil)  ;;; directly referenced in the body:
;;;	each reference operator (c1call-symbol, c1go, c1return-from, c1vref
;;;	and c1setq1) adds the reference to the info-local-referred of the form
;;;	they appear in.
;;;	This information is not propagated to an enclosing function (see
;;;	add-info) so that we can determine exactly which frame is used
;;;	in the body of a function.
  )
