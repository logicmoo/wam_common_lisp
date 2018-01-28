;;;-*-Mode:LISP; Package:(PCL Lisp 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;
;;; The version of low for Kyoto Common Lisp (KCL)
(in-package 'pcl)

;;;
;;; The reason these are here is because the KCL compiler does not allow
;;; LET to return FIXNUM values as values of (c) type int, hence the use
;;; of LOCALLY (which expands into (LET () (DECLARE ...) ...)) forces
;;; conversion of ints to objects.
;;; 
(defmacro %logand (&rest args)
  (reduce-variadic-to-binary 'logand args 0 t 'fixnum))

;(defmacro %logxor (&rest args)
;  (reduce-variadic-to-binary 'logxor args 0 t 'fixnum))

(defmacro %+ (&rest args)
  (reduce-variadic-to-binary '+ args 0 t 'fixnum))

;(defmacro %- (x y)
;  `(the fixnum (- (the fixnum ,x) (the fixnum ,y))))

(defmacro %* (&rest args)
  (reduce-variadic-to-binary '* args 1 t 'fixnum))

(defmacro %/ (x y)
  `(the fixnum (/ (the fixnum ,x) (the fixnum ,y))))

(defmacro %1+ (x)
  `(the fixnum (1+ (the fixnum ,x))))

(defmacro %1- (x)
  `(the fixnum (1- (the fixnum ,x))))

(defmacro %svref (vector index)
  `(svref (the simple-vector ,vector) (the fixnum ,index)))

(defsetf %svref (vector index) (new-value)
  `(setf (svref (the simple-vector ,vector) (the fixnum ,index))
         ,new-value))


;;;
;;; std-instance-p
;;;
(si:define-compiler-macro std-instance-p (x)
  (once-only (x)
    `(and (si:structurep ,x)
	  (eq (si:structure-name ,x) 'std-instance))))

(import 'si:structurep)

(defmacro structure-type (x)
  `(si:structure-name ,x))

(dolist (inline '((si:structurep
		    ((t) compiler::boolean nil nil "type_of(#0)==t_structure")
		    compiler::inline-always)
		  (si:structure-name
		    ((t) t nil nil "(#0)->str.str_name")
		    compiler::inline-unsafe)))
  (setf (get (first inline) (third inline)) (list (second inline))))

(setf (get 'cclosure-env 'compiler::inline-always)
      (list '((t) t nil nil "(#0)->cc.cc_env")))

;;;
;;; turbo-closure patch.  See the file kcl-mods.text for details.
;;;
#+:turbo-closure
(progn
(CLines
  "object tc_cc_env_nthcdr (n,tc)"
  "object n,tc;                        "
  "{return (type_of(tc)==t_cclosure&&  "
  "         tc->cc.cc_turbo!=NULL&&    "
  "         type_of(n)==t_fixnum)?     "
  "         tc->cc.cc_turbo[fix(n)]:   " ; assume that n is in bounds
  "         Cnil;                      "
  "}                                   "
  )

(defentry tc-cclosure-env-nthcdr (object object) (object tc_cc_env_nthcdr))

(setf (get 'tc-cclosure-env-nthcdr 'compiler::inline-unsafe)
      '(((fixnum t) t nil nil "(#1)->cc.cc_turbo[#0]")))
)


;;;; low level stuff to hack compiled functions and compiled closures.
;;;
;;; The primary client for this is fsc-low, but since we make some use of
;;; it here (e.g. to implement set-function-name-1) it all appears here.
;;;

(eval-when (compile eval)

(defmacro define-cstruct-accessor (accessor structure-type field value-type
					    field-type tag-name)
  (let ((setf (intern (concatenate 'string "SET-" (string accessor))))
	(caccessor (format nil "pcl_get_~A_~A" structure-type field))
	(csetf     (format nil "pcl_set_~A_~A" structure-type field))
	(vtype (intern (string-upcase value-type))))
    `(progn
       (CLines ,(format nil "~A ~A(~A)                ~%~
                             object ~A;               ~%~
                             { return ((~A) ~A->~A.~A); }       ~%~
                                                      ~%~
                             ~A ~A(~A, new)           ~%~
                             object ~A;               ~%~
                             ~A new;                  ~%~
                             { return ((~A)(~A->~A.~A = ~Anew)); } ~%~
                            "
			value-type caccessor structure-type 
			structure-type
			value-type structure-type tag-name field
			value-type csetf structure-type
			structure-type 
			value-type 
			value-type structure-type tag-name field field-type
			))

       (defentry ,accessor (object) (,vtype ,caccessor))
       (defentry ,setf (object ,vtype) (,vtype ,csetf))


       (defsetf ,accessor ,setf)

       )))
)
;;; 
;;; struct cfun {                   /*  compiled function header  */
;;;         short   t, m;
;;;         object  cf_name;        /*  compiled function name  */
;;;         int     (*cf_self)();   /*  entry address  */
;;;         object  cf_data;        /*  data the function uses  */
;;;                                 /*  for GBC  */
;;;         char    *cf_start;      /*  start address of the code  */
;;;         int     cf_size;        /*  code size  */
;;; };
;;; add field-type tag-name
(define-cstruct-accessor cfun-name  "cfun" "cf_name"  "object" "(object)" "cf")
(define-cstruct-accessor cfun-self  "cfun" "cf_self"  "int" "(int (*)())" 
                         "cf")
(define-cstruct-accessor cfun-data  "cfun" "cf_data"  "object" "(object)" "cf")
(define-cstruct-accessor cfun-start "cfun" "cf_start" "int" "(char *)" "cf")
(define-cstruct-accessor cfun-size  "cfun" "cf_size"  "int" "(int)" "cf")

(CLines
  "object pcl_cfunp (x)              "
  "object x;                         "
  "{if(x->c.t == (int) t_cfun)       "
  "  return (Ct);                    "
  "  else                            "
  "    return (Cnil);                "
  "  }                               "
  )

(defentry cfunp (object) (object pcl_cfunp))

;;; 
;;; struct cclosure {               /*  compiled closure header  */
;;;         short   t, m;
;;;         object  cc_name;        /*  compiled closure name  */
;;;         int     (*cc_self)();   /*  entry address  */
;;;         object  cc_env;         /*  environment  */
;;;         object  cc_data;        /*  data the closure uses  */
;;;                                 /*  for GBC  */
;;;         char    *cc_start;      /*  start address of the code  */
;;;         int     cc_size;        /*  code size  */
;;; };
;;; 
(define-cstruct-accessor cclosure-name "cclosure"  "cc_name"  "object"
                         "(object)" "cc")          
(define-cstruct-accessor cclosure-self "cclosure"  "cc_self"  "int" 
                         "(int (*)())" "cc")
(define-cstruct-accessor cclosure-data "cclosure"  "cc_data"  "object"
                          "(object)" "cc")
(define-cstruct-accessor cclosure-start "cclosure" "cc_start" "int" 
                         "(char *)" "cc")
(define-cstruct-accessor cclosure-size "cclosure"  "cc_size"  "int"
			 "(int)" "cc")
(define-cstruct-accessor cclosure-env "cclosure"   "cc_env"   "object"
                         "(object)" "cc")


(CLines
  "object pcl_cclosurep (x)          "
  "object x;                         "
  "{if(x->c.t == (int) t_cclosure)   "
  "  return (Ct);                    "
  "  else                            "
  "   return (Cnil);                 "
  "  }                               "
  )

(defentry cclosurep (object) (object pcl_cclosurep))

  ;;   
;;;;;; Load Time Eval
  ;;
;;; 

;;; This doesn't work because it looks at a global variable to see if it is
;;; in the compiler rather than looking at the macroexpansion environment.
;;; 
;;; The result is that if in the process of compiling a file, we evaluate a
;;; form that has a call to load-time-eval, we will get faked into thinking
;;; that we are compiling that form.
;;;
;;; THIS NEEDS TO BE DONE RIGHT!!!
;;; 
;(defmacro load-time-eval (form)
;  ;; In KCL there is no compile-to-core case.  For things that we are 
;  ;; "compiling to core" we just expand the same way as if were are
;  ;; compiling a file since the form will be evaluated in just a little
;  ;; bit when gazonk.o is loaded.
;  (if (and (boundp 'compiler::*compiler-input*)  ;Hack to see of we are
;	   compiler::*compiler-input*)		  ;in the compiler!
;      `'(si:|#,| . ,form)
;      `(progn ,form)))

(defmacro load-time-eval (form)
  (read-from-string (format nil "'#,~S" form)))

(defmacro memory-block-ref (block offset)
  `(svref (the simple-vector ,block) (the fixnum ,offset)))

  ;;   
;;;;;; Generating CACHE numbers
  ;;
;;; This needs more work to be sure it is going as fast as possible.
;;;   -  The calls to si:address should be open-coded.
;;;   -  The logand should be open coded.
;;;   

;(defmacro symbol-cache-no (symbol mask)
;  (if (and (constantp symbol)
;	   (constantp mask))
;      `(load-time-eval (logand (ash (si:address ,symbol) -2) ,mask))
;      `(logand (ash (the fixnum (si:address ,symbol)) -2) ,mask)))

(defmacro object-cache-no (object mask)
  `(logand (the fixnum (si:address ,object)) ,mask))

  ;;   
;;;;;; printing-random-thing-internal
  ;;
(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (si:address thing)))


(defun set-function-name-1 (fn new-name ignore)
  (cond ((cclosurep fn)
	 (setf (cclosure-name fn) new-name))
	((cfunp fn)
	 (setf (cfun-name fn) new-name))
	((and (listp fn)
	      (eq (car fn) 'lambda-block))
	 (setf (cadr fn) new-name))
	((and (listp fn)
	      (eq (car fn) 'lambda))
	 (setf (car fn) 'lambda-block
	       (cdr fn) (cons new-name (cdr fn)))))
  fn)




#|
(defconstant most-positive-small-fixnum 1024)  /* should be supplied */
(defconstant most-negative-small-fixnum -1024) /* by ibuki */

(defmacro symbol-cache-no (symbol mask)
  (if (constantp mask)
      (if (and (> mask 0)
	       (< mask most-positive-small-fixnum))
	  (if (constantp symbol)
	      `(load-time-eval (coffset ,symbol ,mask 2))
	    `(coffset ,symbol ,mask 2))
	(if (constantp symbol)
	    `(load-time-eval 
	       (logand (ash (the fixnum (si:address ,symbol)) -2) ,mask))
	  `(logand (ash (the fixnum (si:address ,symbol)) -2) ,mask)))
    `(logand (ash (the fixnum (si:address ,symbol)) -2) ,mask)))


(defmacro object-cache-no (object mask)
  (if (and (constantp mask)
	   (> mask 0)
	   (< mask most-positive-small-fixnum))
      `(coffset ,object ,mask 4)
    `(logand (ash (the fixnum (si:address ,object)) -4) ,mask)))

(CLines
  "object pcl_coffset (sym,mask,lshift)"
  "object sym,mask,lshift;"
  "{"
  "	return(small_fixnum(((int)sym >> fix(lshift)) & fix(mask)));"
  "}"
  )

(defentry coffset (object object object) (object pcl_coffset))


|#

