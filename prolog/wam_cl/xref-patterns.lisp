;;; ****************************************************************
;;; Cross Referencing Patterns for Common Lisp *********************
;;; ****************************************************************
;;(clear-patterns)


;;; ********************************
;;; Pattern Substitutions **********
;;; ********************************
(data-assrt :define-pattern-substitution integer (:test #'integerp))
(data-assrt :define-pattern-substitution rational (:test #'rationalp))
(data-assrt :define-pattern-substitution symbol  (:test #'symbolp))
(data-assrt :define-pattern-substitution string  (:test #'stringp))
(data-assrt :define-pattern-substitution number  (:test #'numberp))
(data-assrt :define-pattern-substitution lambda-list
  ((:star var)
   (:optional (:eq &optional)
	      (:star (:or var
			  (var (:optional form (:optional var))))))
   (:optional (:eq &rest) var)
   (:optional (:eq &key) (:star (:or var
			       ((:or var
				     (keyword var))
				(:optional form (:optional var)))))
	      (:optional &allow-other-keys))
   (:optional (:eq &aux)
	      (:star (:or var
			  (var (:optional form)))))))
(data-assrt :define-pattern-substitution test form)
(data-assrt :define-pattern-substitution body
  ((:star (:or declaration documentation-string))
   (:star form)))
(data-assrt :define-pattern-substitution documentation-string string)
(data-assrt :define-pattern-substitution initial-value form)
(data-assrt :define-pattern-substitution tag symbol)
(data-assrt :define-pattern-substitution declaration ((:eq declare)(:rest :ignore)))
(data-assrt :define-pattern-substitution destination form)
(data-assrt :define-pattern-substitution control-string string)
(data-assrt :define-pattern-substitution format-arguments 
  ((:star form)))
(data-assrt :define-pattern-substitution fn
  (:or ((:eq quote) function) 
       ((:eq function) function)
       function))

;;; ********************************
;;; Caller Patterns ****************
;;; ********************************

;;; Types Related
(data-assrt :define-caller-pattern coerce (form :ignore) :lisp)
(data-assrt :define-caller-pattern type-of (form) :lisp)
(data-assrt :define-caller-pattern upgraded-array-element-type (:ignore) :lisp2)
(data-assrt :define-caller-pattern upgraded-complex-part-type (:ignore) :lisp2)

;;; Lambdas and Definitions
(data-assrt :define-variable-pattern lambda-list-keywords :lisp)
(data-assrt :define-variable-pattern lambda-parameters-limit :lisp)
(data-assrt :define-caller-pattern lambda (lambda-list (:rest body)) :lisp)

(data-assrt :define-caller-pattern defun 
  (name lambda-list
	(:star (:or documentation-string declaration))
	(:star form))
  :lisp)

;;; perhaps this should use VAR, instead of NAME
(data-assrt :define-caller-pattern defvar 
  (var (:optional initial-value (:optional documentation-string)))
  :lisp)
(data-assrt :define-caller-pattern defparameter
  (var initial-value (:optional documentation-string))
  :lisp)
(data-assrt :define-caller-pattern defconstant
  (var initial-value (:optional documentation-string))
  :lisp)

(data-assrt :define-caller-pattern eval-when
  (:ignore				; the situations
   (:star form))
  :lisp)

;;; Logical Values
(data-assrt :define-variable-pattern nil :lisp)
(data-assrt :define-variable-pattern t :lisp)

;;; Predicates
(data-assrt :define-caller-pattern typep (form form) :lisp)
(data-assrt :define-caller-pattern subtypep (form form) :lisp)

(data-assrt :define-caller-pattern null (form) :lisp)
(data-assrt :define-caller-pattern symbolp (form) :lisp)
(data-assrt :define-caller-pattern atom (form) :lisp)
(data-assrt :define-caller-pattern consp (form) :lisp)
(data-assrt :define-caller-pattern listp (form) :lisp)
(data-assrt :define-caller-pattern numberp (form) :lisp)
(data-assrt :define-caller-pattern integerp (form) :lisp)
(data-assrt :define-caller-pattern rationalp (form) :lisp)
(data-assrt :define-caller-pattern floatp (form) :lisp)
(data-assrt :define-caller-pattern realp (form) :lisp2)
(data-assrt :define-caller-pattern complexp (form) :lisp)
(data-assrt :define-caller-pattern characterp (form) :lisp)
(data-assrt :define-caller-pattern stringp (form) :lisp)
(data-assrt :define-caller-pattern bit-vector-p (form) :lisp)
(data-assrt :define-caller-pattern vectorp (form) :lisp)
(data-assrt :define-caller-pattern simple-vector-p (form) :lisp)
(data-assrt :define-caller-pattern simple-string-p (form) :lisp)
(data-assrt :define-caller-pattern simple-bit-vector-p (form) :lisp)
(data-assrt :define-caller-pattern arrayp (form) :lisp)
(data-assrt :define-caller-pattern packagep (form) :lisp)
(data-assrt :define-caller-pattern functionp (form) :lisp)
(data-assrt :define-caller-pattern compiled-function-p (form) :lisp)
(data-assrt :define-caller-pattern commonp (form) :lisp)

;;; Equality Predicates
(data-assrt :define-caller-pattern eq (form form) :lisp)
(data-assrt :define-caller-pattern eql (form form) :lisp)
(data-assrt :define-caller-pattern equal (form form) :lisp)
(data-assrt :define-caller-pattern equalp (form form) :lisp)

;;; Logical Operators
(data-assrt :define-caller-pattern not (form) :lisp)
(data-assrt :define-caller-pattern or ((:star form)) :lisp)
(data-assrt :define-caller-pattern and ((:star form)) :lisp)

;;; Reference

;;; Quote is a problem. In Defmacro & friends, we'd like to actually
;;; look at the argument, 'cause it hides internal function calls
;;; of the defmacro. 
(data-assrt :define-caller-pattern quote (:ignore) :lisp)

(data-assrt :define-caller-pattern function ((:or fn form)) :lisp)
(data-assrt :define-caller-pattern symbol-value (form) :lisp)
(data-assrt :define-caller-pattern symbol-function (form) :lisp)
(data-assrt :define-caller-pattern fdefinition (form) :lisp2)
(data-assrt :define-caller-pattern boundp (form) :lisp)
(data-assrt :define-caller-pattern fboundp (form) :lisp)
(data-assrt :define-caller-pattern special-form-p (form) :lisp)

;;; Assignment
(data-assrt :define-caller-pattern setq ((:star var form)) :lisp)
(data-assrt :define-caller-pattern psetq ((:star var form)) :lisp)
(data-assrt :define-caller-pattern set (form form) :lisp)
(data-assrt :define-caller-pattern makunbound (form) :lisp)
(data-assrt :define-caller-pattern fmakunbound (form) :lisp)

;;; Generalized Variables
(data-assrt :define-caller-pattern setf ((:star form form)) :lisp)
(data-assrt :define-caller-pattern psetf ((:star form form)) :lisp)
(data-assrt :define-caller-pattern shiftf ((:plus form) form) :lisp)
(data-assrt :define-caller-pattern rotatef ((:star form)) :lisp)
(data-assrt :define-caller-pattern define-modify-macro 
  (name
   lambda-list
   fn
   (:optional documentation-string))
  :lisp)
(data-assrt :define-caller-pattern defsetf 
  (:or (name name (:optional documentation-string))
       (name lambda-list (var)
	(:star (:or declaration documentation-string))
	(:star form)))
  :lisp)
(data-assrt :define-caller-pattern define-setf-method
  (name lambda-list
   (:star (:or declaration documentation-string))
   (:star form))
  :lisp)
(data-assrt :define-caller-pattern get-setf-method (form) :lisp)
(data-assrt :define-caller-pattern get-setf-method-multiple-value (form) :lisp)


;;; Function invocation
(data-assrt :define-caller-pattern apply (fn form (:star form)) :lisp)
(data-assrt :define-caller-pattern funcall (fn (:star form)) :lisp)


;;; Simple sequencing
(data-assrt :define-caller-pattern progn ((:star form)) :lisp)
(data-assrt :define-caller-pattern prog1 (form (:star form)) :lisp)
(data-assrt :define-caller-pattern prog2 (form form (:star form)) :lisp)

;;; Variable bindings
(data-assrt :define-caller-pattern let
  (((:star (:or var (var &optional form))))
   (:star declaration)
   (:star form))
  :lisp)
(data-assrt :define-caller-pattern let*
  (((:star (:or var (var &optional form))))
    (:star declaration)
    (:star form))
  :lisp)
(data-assrt :define-caller-pattern compiler-let
  (((:star (:or var (var form))))
    (:star form))
  :lisp)
(data-assrt :define-caller-pattern progv
  (form form (:star form)) :lisp)
(data-assrt :define-caller-pattern flet
  (((:star (name lambda-list 
		 (:star (:or declaration
			     documentation-string))
		 (:star form))))
   (:star form))
  :lisp)
(data-assrt :define-caller-pattern labels
  (((:star (name lambda-list 
		 (:star (:or declaration
			     documentation-string))
		 (:star form))))
   (:star form))
  :lisp)
(data-assrt :define-caller-pattern macrolet
  (((:star (name lambda-list 
		 (:star (:or declaration
			     documentation-string))
		 (:star form))))
   (:star form))
  :lisp)
(data-assrt :define-caller-pattern symbol-macrolet
  (((:star (var form))) (:star declaration) (:star form))
  :lisp2)

;;; Conditionals
(data-assrt :define-caller-pattern if (test form (:optional form)) :lisp)
(data-assrt :define-caller-pattern when (test (:star form)) :lisp)
(data-assrt :define-caller-pattern unless (test (:star form)) :lisp)
(data-assrt :define-caller-pattern cond ((:star (test (:star form)))) :lisp)
(data-assrt :define-caller-pattern case
  (form
   (:star ((:or symbol
		((:star symbol)))
	   (:star form)))) 
  :lisp)
(data-assrt :define-caller-pattern typecase (form (:star (symbol (:star form)))) 
  :lisp)

;;; Blocks and Exits
(data-assrt :define-caller-pattern block (name (:star form)) :lisp)
(data-assrt :define-caller-pattern return-from (function (:optional form)) :lisp)
(data-assrt :define-caller-pattern return ((:optional form)) :lisp)

;;; Iteration
(data-assrt :define-caller-pattern loop ((:star form)) :lisp)
(data-assrt :define-caller-pattern do
  (((:star (:or var
		(var (:optional form (:optional form)))))) ; init step
   (form (:star form)) ; end-test result
   (:star declaration)
   (:star (:or tag form)))		; statement
  :lisp)
(data-assrt :define-caller-pattern do*
  (((:star (:or var
		(var (:optional form (:optional form)))))) 
   (form (:star form))
   (:star declaration)
   (:star (:or tag form)))
  :lisp)
(data-assrt :define-caller-pattern dolist
  ((var form (:optional form))
   (:star declaration)
   (:star (:or tag form)))
  :lisp)
(data-assrt :define-caller-pattern dotimes
  ((var form (:optional form))
   (:star declaration)
   (:star (:or tag form)))
  :lisp)

;;; Mapping
(data-assrt :define-caller-pattern mapcar (fn form (:star form)) :lisp)
(data-assrt :define-caller-pattern maplist (fn form (:star form)) :lisp)
(data-assrt :define-caller-pattern mapc (fn form (:star form)) :lisp)
(data-assrt :define-caller-pattern mapl (fn form (:star form)) :lisp)
(data-assrt :define-caller-pattern mapcan (fn form (:star form)) :lisp)
(data-assrt :define-caller-pattern mapcon (fn form (:star form)) :lisp)

;;; The "Program Feature"
(data-assrt :define-caller-pattern tagbody ((:star (:or tag form))) :lisp)
(data-assrt :define-caller-pattern prog
  (((:star (:or var (var (:optional form)))))
   (:star declaration)
   (:star (:or tag form)))
  :lisp)
(data-assrt :define-caller-pattern prog*    
  (((:star (:or var (var (:optional form)))))
   (:star declaration)
   (:star (:or tag form)))
  :lisp)
(data-assrt :define-caller-pattern go (tag) :lisp)

;;; Multiple Values
(data-assrt :define-caller-pattern values ((:star form)) :lisp)
(data-assrt :define-variable-pattern multiple-values-limit :lisp)
(data-assrt :define-caller-pattern values-list (form) :lisp)
(data-assrt :define-caller-pattern multiple-value-list (form) :lisp)
(data-assrt :define-caller-pattern multiple-value-call (fn (:star form)) :lisp)
(data-assrt :define-caller-pattern multiple-value-prog1 (form (:star form)) :lisp)
(data-assrt :define-caller-pattern multiple-value-bind
  (((:star var)) form
   (:star declaration)
   (:star form))
  :lisp)
(data-assrt :define-caller-pattern multiple-value-setq (((:star var)) form) :lisp)
(data-assrt :define-caller-pattern nth-value (form form) :lisp2)

;;; Dynamic Non-Local Exits
(data-assrt :define-caller-pattern catch (tag (:star form)) :lisp)
(data-assrt :define-caller-pattern throw (tag form) :lisp)
(data-assrt :define-caller-pattern unwind-protect (form (:star form)) :lisp)

;;; Macros
(data-assrt :define-caller-pattern macro-function (form) :lisp)
(data-assrt :define-caller-pattern defmacro
  (name
   lambda-list
   (:star (:or declaration documentation-string))
   (:star form))
  :lisp)
(data-assrt :define-caller-pattern macroexpand (form (:optional :ignore)) :lisp)
(data-assrt :define-caller-pattern macroexpand-1 (form (:optional :ignore)) :lisp)
(data-assrt :define-variable-pattern *macroexpand-hook* :lisp)

;;; Destructuring
(data-assrt :define-caller-pattern destructuring-bind 
  (lambda-list form
	       (:star declaration)
	       (:star form))
  :lisp2)

;;; Compiler Macros
(data-assrt :define-caller-pattern define-compiler-macro
  (name lambda-list
	(:star (:or declaration documentation-string))
	(:star form))
  :lisp2)
(data-assrt :define-caller-pattern compiler-macro-function (form) :lisp2)
(data-assrt :define-caller-pattern compiler-macroexpand (form (:optional :ignore)) :lisp2)
(data-assrt :define-caller-pattern compiler-macroexpand-1 (form (:optional :ignore))
  :lisp2)

;;; Environments
(data-assrt :define-caller-pattern variable-information (form &optional :ignore) 
  :lisp2)
(data-assrt :define-caller-pattern function-information (fn &optional :ignore) :lisp2)
(data-assrt :define-caller-pattern declaration-information (form &optional :ignore) :lisp2)
(data-assrt :define-caller-pattern augment-environment (form &key (:star :ignore)) :lisp2)
(data-assrt :define-caller-pattern define-declaration 
  (name
   lambda-list
   (:star form)) 
  :lisp2)
(data-assrt :define-caller-pattern parse-macro (name lambda-list form) :lisp2)
(data-assrt :define-caller-pattern enclose (form &optional :ignore) :lisp2)


;;; Declarations
(data-assrt :define-caller-pattern declare ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern proclaim ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern locally ((:star declaration) (:star form)) :lisp)
(data-assrt :define-caller-pattern declaim ((:rest :ignore)) :lisp2)
(data-assrt :define-caller-pattern the (form form) :lisp)

;;; Symbols
(data-assrt :define-caller-pattern get (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern remprop (form form) :lisp)
(data-assrt :define-caller-pattern symbol-plist (form) :lisp)
(data-assrt :define-caller-pattern getf (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern remf (form form) :lisp)
(data-assrt :define-caller-pattern get-properties (form form) :lisp)

(data-assrt :define-caller-pattern symbol-name (form) :lisp)
(data-assrt :define-caller-pattern make-symbol (form) :lisp)
(data-assrt :define-caller-pattern copy-symbol (form (:optional :ignore)) :lisp)
(data-assrt :define-caller-pattern gensym ((:optional :ignore)) :lisp)
(data-assrt :define-variable-pattern *gensym-counter* :lisp2)
(data-assrt :define-caller-pattern gentemp ((:optional :ignore :ignore)) :lisp)
(data-assrt :define-caller-pattern symbol-package (form) :lisp)
(data-assrt :define-caller-pattern keywordp (form) :lisp)

;;; Packages
(data-assrt :define-variable-pattern *package* :lisp)
(data-assrt :define-caller-pattern make-package ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern in-package ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern find-package ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern package-name ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern package-nicknames ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern rename-package ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern package-use-list ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern package-used-by-list ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern package-shadowing-symbols ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern list-all-packages () :lisp)
(data-assrt :define-caller-pattern delete-package ((:rest :ignore)) :lisp2)
(data-assrt :define-caller-pattern intern (form &optional :ignore) :lisp)
(data-assrt :define-caller-pattern find-symbol (form &optional :ignore) :lisp)
(data-assrt :define-caller-pattern unintern (form &optional :ignore) :lisp)

(data-assrt :define-caller-pattern export ((:or symbol ((:star symbol)))
			       &optional :ignore) :lisp)
(data-assrt :define-caller-pattern unexport ((:or symbol ((:star symbol)))
			       &optional :ignore) :lisp)
(data-assrt :define-caller-pattern import ((:or symbol ((:star symbol)))
			       &optional :ignore) :lisp)
(data-assrt :define-caller-pattern shadowing-import ((:or symbol ((:star symbol)))
			       &optional :ignore) :lisp)
(data-assrt :define-caller-pattern shadow ((:or symbol ((:star symbol)))
			       &optional :ignore) :lisp)

(data-assrt :define-caller-pattern use-package ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern unuse-package ((:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern defpackage (name (:rest :ignore)) :lisp2)
(data-assrt :define-caller-pattern find-all-symbols (form) :lisp)
(data-assrt :define-caller-pattern do-symbols 
  ((var (:optional form (:optional form)))
   (:star declaration) 
   (:star (:or tag form))) 
  :lisp)
(data-assrt :define-caller-pattern do-external-symbols 
  ((var (:optional form (:optional form)))
   (:star declaration) 
   (:star (:or tag form))) 
  :lisp)
(data-assrt :define-caller-pattern do-all-symbols 
  ((var (:optional form))
   (:star declaration) 
   (:star (:or tag form))) 
  :lisp)
(data-assrt :define-caller-pattern with-package-iterator
  ((name form (:plus :ignore))
   (:star form))
  :lisp2)

;;; Modules
(data-assrt :define-variable-pattern *modules* :lisp)
(data-assrt :define-caller-pattern provide (form) :lisp)
(data-assrt :define-caller-pattern require (form &optional :ignore) :lisp)


;;; Numbers
(data-assrt :define-caller-pattern zerop (form) :lisp)
(data-assrt :define-caller-pattern plusp (form) :lisp)
(data-assrt :define-caller-pattern minusp (form) :lisp)
(data-assrt :define-caller-pattern oddp (form) :lisp)
(data-assrt :define-caller-pattern evenp (form) :lisp)

(data-assrt :define-caller-pattern = (form (:star form)) :lisp)
(data-assrt :define-caller-pattern /= (form (:star form)) :lisp)
(data-assrt :define-caller-pattern > (form (:star form)) :lisp)
(data-assrt :define-caller-pattern < (form (:star form)) :lisp)
(data-assrt :define-caller-pattern <= (form (:star form)) :lisp)
(data-assrt :define-caller-pattern >= (form (:star form)) :lisp)

(data-assrt :define-caller-pattern max (form (:star form)) :lisp)
(data-assrt :define-caller-pattern min (form (:star form)) :lisp)

(data-assrt :define-caller-pattern - (form (:star form)) :lisp)
(data-assrt :define-caller-pattern + (form (:star form)) :lisp)
(data-assrt :define-caller-pattern * (form (:star form)) :lisp)
(data-assrt :define-caller-pattern / (form (:star form)) :lisp)
(data-assrt :define-caller-pattern 1+ (form) :lisp)
(data-assrt :define-caller-pattern 1- (form) :lisp)

(data-assrt :define-caller-pattern incf (form form) :lisp)
(data-assrt :define-caller-pattern decf (form form) :lisp)

(data-assrt :define-caller-pattern conjugate (form) :lisp)

(data-assrt :define-caller-pattern gcd ((:star form)) :lisp)
(data-assrt :define-caller-pattern lcm ((:star form)) :lisp)

(data-assrt :define-caller-pattern exp (form) :lisp)
(data-assrt :define-caller-pattern expt (form form) :lisp)
(data-assrt :define-caller-pattern log (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern sqrt (form) :lisp)
(data-assrt :define-caller-pattern isqrt (form) :lisp)

(data-assrt :define-caller-pattern abs (form) :lisp)
(data-assrt :define-caller-pattern phase (form) :lisp)
(data-assrt :define-caller-pattern signum (form) :lisp)
(data-assrt :define-caller-pattern sin (form) :lisp)
(data-assrt :define-caller-pattern cos (form) :lisp)
(data-assrt :define-caller-pattern tan (form) :lisp)
(data-assrt :define-caller-pattern cis (form) :lisp)
(data-assrt :define-caller-pattern asin (form) :lisp)
(data-assrt :define-caller-pattern acos (form) :lisp)
(data-assrt :define-caller-pattern atan (form &optional form) :lisp)
(data-assrt :define-variable-pattern pi :lisp)

(data-assrt :define-caller-pattern sinh (form) :lisp)
(data-assrt :define-caller-pattern cosh (form) :lisp)
(data-assrt :define-caller-pattern tanh (form) :lisp)
(data-assrt :define-caller-pattern asinh (form) :lisp)
(data-assrt :define-caller-pattern acosh (form) :lisp)
(data-assrt :define-caller-pattern atanh (form) :lisp)

;;; Type Conversions and Extractions
(data-assrt :define-caller-pattern float (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern rational (form) :lisp)
(data-assrt :define-caller-pattern rationalize (form) :lisp)
(data-assrt :define-caller-pattern numerator (form) :lisp)
(data-assrt :define-caller-pattern denominator (form) :lisp)

(data-assrt :define-caller-pattern floor (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern ceiling (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern truncate (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern round (form (:optional form)) :lisp)

(data-assrt :define-caller-pattern mod (form form) :lisp)
(data-assrt :define-caller-pattern rem (form form) :lisp)

(data-assrt :define-caller-pattern ffloor (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern fceiling (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern ftruncate (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern fround (form (:optional form)) :lisp)

(data-assrt :define-caller-pattern decode-float (form) :lisp)
(data-assrt :define-caller-pattern scale-float (form form) :lisp)
(data-assrt :define-caller-pattern float-radix (form) :lisp)
(data-assrt :define-caller-pattern float-sign (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern float-digits (form) :lisp)
(data-assrt :define-caller-pattern float-precision (form) :lisp)
(data-assrt :define-caller-pattern integer-decode-float (form) :lisp)

(data-assrt :define-caller-pattern complex (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern realpart (form) :lisp)
(data-assrt :define-caller-pattern imagpart (form) :lisp)

(data-assrt :define-caller-pattern logior ((:star form)) :lisp)
(data-assrt :define-caller-pattern logxor ((:star form)) :lisp)
(data-assrt :define-caller-pattern logand ((:star form)) :lisp)
(data-assrt :define-caller-pattern logeqv ((:star form)) :lisp)

(data-assrt :define-caller-pattern lognand (form form) :lisp)
(data-assrt :define-caller-pattern lognor (form form) :lisp)
(data-assrt :define-caller-pattern logandc1 (form form) :lisp)
(data-assrt :define-caller-pattern logandc2 (form form) :lisp)
(data-assrt :define-caller-pattern logorc1 (form form) :lisp)
(data-assrt :define-caller-pattern logorc2 (form form) :lisp)

(data-assrt :define-caller-pattern boole (form form form) :lisp)
(data-assrt :define-variable-pattern boole-clr :lisp)
(data-assrt :define-variable-pattern boole-set :lisp)
(data-assrt :define-variable-pattern boole-1 :lisp)
(data-assrt :define-variable-pattern boole-2 :lisp)
(data-assrt :define-variable-pattern boole-c1 :lisp)
(data-assrt :define-variable-pattern boole-c2 :lisp)
(data-assrt :define-variable-pattern boole-and :lisp)
(data-assrt :define-variable-pattern boole-ior :lisp)
(data-assrt :define-variable-pattern boole-xor :lisp)
(data-assrt :define-variable-pattern boole-eqv :lisp)
(data-assrt :define-variable-pattern boole-nand :lisp)
(data-assrt :define-variable-pattern boole-nor :lisp)
(data-assrt :define-variable-pattern boole-andc1 :lisp)
(data-assrt :define-variable-pattern boole-andc2 :lisp)
(data-assrt :define-variable-pattern boole-orc1 :lisp)
(data-assrt :define-variable-pattern boole-orc2 :lisp)

(data-assrt :define-caller-pattern lognot (form) :lisp)
(data-assrt :define-caller-pattern logtest (form form) :lisp)
(data-assrt :define-caller-pattern logbitp (form form) :lisp)
(data-assrt :define-caller-pattern ash (form form) :lisp)
(data-assrt :define-caller-pattern logcount (form) :lisp)
(data-assrt :define-caller-pattern integer-length (form) :lisp)

(data-assrt :define-caller-pattern byte (form form) :lisp)
(data-assrt :define-caller-pattern byte-size (form) :lisp)
(data-assrt :define-caller-pattern byte-position (form) :lisp)
(data-assrt :define-caller-pattern ldb (form form) :lisp)
(data-assrt :define-caller-pattern ldb-test (form form) :lisp)
(data-assrt :define-caller-pattern mask-field (form form) :lisp)
(data-assrt :define-caller-pattern dpb (form form form) :lisp)
(data-assrt :define-caller-pattern deposit-field (form form form) :lisp)

;;; Random Numbers
(data-assrt :define-caller-pattern random (form (:optional form)) :lisp)
(data-assrt :define-variable-pattern *random-state* :lisp)
(data-assrt :define-caller-pattern make-random-state ((:optional form)) :lisp)
(data-assrt :define-caller-pattern random-state-p (form) :lisp)

;;; Implementation Parameters
(data-assrt :define-variable-pattern most-positive-fixnum :lisp)
(data-assrt :define-variable-pattern most-negative-fixnum :lisp)
(data-assrt :define-variable-pattern most-positive-short-float :lisp)
(data-assrt :define-variable-pattern least-positive-short-float :lisp)
(data-assrt :define-variable-pattern least-negative-short-float :lisp)
(data-assrt :define-variable-pattern most-negative-short-float :lisp)
(data-assrt :define-variable-pattern most-positive-single-float :lisp)
(data-assrt :define-variable-pattern least-positive-single-float :lisp)
(data-assrt :define-variable-pattern least-negative-single-float :lisp)
(data-assrt :define-variable-pattern most-negative-single-float :lisp)
(data-assrt :define-variable-pattern most-positive-double-float :lisp)
(data-assrt :define-variable-pattern least-positive-double-float :lisp)
(data-assrt :define-variable-pattern least-negative-double-float :lisp)
(data-assrt :define-variable-pattern most-negative-double-float :lisp)
(data-assrt :define-variable-pattern most-positive-long-float :lisp)
(data-assrt :define-variable-pattern least-positive-long-float :lisp)
(data-assrt :define-variable-pattern least-negative-long-float :lisp)
(data-assrt :define-variable-pattern most-negative-long-float :lisp)
(data-assrt :define-variable-pattern least-positive-normalized-short-float :lisp2)
(data-assrt :define-variable-pattern least-negative-normalized-short-float :lisp2)
(data-assrt :define-variable-pattern least-positive-normalized-single-float :lisp2)
(data-assrt :define-variable-pattern least-negative-normalized-single-float :lisp2)
(data-assrt :define-variable-pattern least-positive-normalized-double-float :lisp2)
(data-assrt :define-variable-pattern least-negative-normalized-double-float :lisp2)
(data-assrt :define-variable-pattern least-positive-normalized-long-float :lisp2)
(data-assrt :define-variable-pattern least-negative-normalized-long-float :lisp2)
(data-assrt :define-variable-pattern short-float-epsilon :lisp)
(data-assrt :define-variable-pattern single-float-epsilon :lisp)
(data-assrt :define-variable-pattern double-float-epsilon :lisp)
(data-assrt :define-variable-pattern long-float-epsilon :lisp)
(data-assrt :define-variable-pattern short-float-negative-epsilon :lisp)
(data-assrt :define-variable-pattern single-float-negative-epsilon :lisp)
(data-assrt :define-variable-pattern double-float-negative-epsilon :lisp)
(data-assrt :define-variable-pattern long-float-negative-epsilon :lisp)

;;; Characters 
(data-assrt :define-variable-pattern char-code-limit :lisp)
(data-assrt :define-variable-pattern char-font-limit :lisp)
(data-assrt :define-variable-pattern char-bits-limit :lisp)
(data-assrt :define-caller-pattern standard-char-p (form) :lisp)
(data-assrt :define-caller-pattern graphic-char-p (form) :lisp)
(data-assrt :define-caller-pattern string-char-p (form) :lisp)
(data-assrt :define-caller-pattern alpha-char-p (form) :lisp)
(data-assrt :define-caller-pattern upper-case-p (form) :lisp)
(data-assrt :define-caller-pattern lower-case-p (form) :lisp)
(data-assrt :define-caller-pattern both-case-p (form) :lisp)
(data-assrt :define-caller-pattern digit-char-p (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern alphanumericp (form) :lisp)

(data-assrt :define-caller-pattern char= ((:star form)) :lisp)
(data-assrt :define-caller-pattern char/= ((:star form)) :lisp)
(data-assrt :define-caller-pattern char< ((:star form)) :lisp)
(data-assrt :define-caller-pattern char> ((:star form)) :lisp)
(data-assrt :define-caller-pattern char<= ((:star form)) :lisp)
(data-assrt :define-caller-pattern char>= ((:star form)) :lisp)

(data-assrt :define-caller-pattern char-equal ((:star form)) :lisp)
(data-assrt :define-caller-pattern char-not-equal ((:star form)) :lisp)
(data-assrt :define-caller-pattern char-lessp ((:star form)) :lisp)
(data-assrt :define-caller-pattern char-greaterp ((:star form)) :lisp)
(data-assrt :define-caller-pattern char-not-greaterp ((:star form)) :lisp)
(data-assrt :define-caller-pattern char-not-lessp ((:star form)) :lisp)

(data-assrt :define-caller-pattern char-code (form) :lisp)
(data-assrt :define-caller-pattern char-bits (form) :lisp)
(data-assrt :define-caller-pattern char-font (form) :lisp)
(data-assrt :define-caller-pattern code-char (form (:optional form form)) :lisp)
(data-assrt :define-caller-pattern make-char (form (:optional form form)) :lisp)
(data-assrt :define-caller-pattern characterp (form) :lisp)
(data-assrt :define-caller-pattern char-upcase (form) :lisp)
(data-assrt :define-caller-pattern char-downcase (form) :lisp)
(data-assrt :define-caller-pattern digit-char (form (:optional form form)) :lisp)
(data-assrt :define-caller-pattern char-int (form) :lisp)
(data-assrt :define-caller-pattern int-char (form) :lisp)
(data-assrt :define-caller-pattern char-name (form) :lisp)
(data-assrt :define-caller-pattern name-char (form) :lisp)
(data-assrt :define-variable-pattern char-control-bit :lisp)
(data-assrt :define-variable-pattern char-meta-bit :lisp)
(data-assrt :define-variable-pattern char-super-bit :lisp)
(data-assrt :define-variable-pattern char-hyper-bit :lisp)
(data-assrt :define-caller-pattern char-bit (form form) :lisp)
(data-assrt :define-caller-pattern set-char-bit (form form form) :lisp)

;;; Sequences
(data-assrt :define-caller-pattern complement (fn) :lisp2)
(data-assrt :define-caller-pattern elt (form form) :lisp)
(data-assrt :define-caller-pattern subseq (form form &optional form) :lisp)
(data-assrt :define-caller-pattern copy-seq (form) :lisp)
(data-assrt :define-caller-pattern length (form) :lisp)
(data-assrt :define-caller-pattern reverse (form) :lisp)
(data-assrt :define-caller-pattern nreverse (form) :lisp)
(data-assrt :define-caller-pattern make-sequence (form form &key form) :lisp)

(data-assrt :define-caller-pattern concatenate (form (:star form)) :lisp)
(data-assrt :define-caller-pattern map (form fn form (:star form)) :lisp)
(data-assrt :define-caller-pattern map-into (form fn (:star form)) :lisp2)

(data-assrt :define-caller-pattern some (fn form (:star form)) :lisp)
(data-assrt :define-caller-pattern every (fn form (:star form)) :lisp)
(data-assrt :define-caller-pattern notany (fn form (:star form)) :lisp)
(data-assrt :define-caller-pattern notevery (fn form (:star form)) :lisp)

(data-assrt :define-caller-pattern reduce (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern fill (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern replace (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern remove (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern remove-if (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern remove-if-not (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern delete (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern delete-if (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern delete-if-not (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern remove-duplicates (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern delete-duplicates (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern substitute (form form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern substitute-if (form fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern substitute-if-not (form fn form &key (:star form))
  :lisp)
(data-assrt :define-caller-pattern nsubstitute (form form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nsubstitute-if (form fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nsubstitute-if-not (form fn form &key (:star form))
  :lisp)
(data-assrt :define-caller-pattern find (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern find-if (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern find-if-not (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern position (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern position-if (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern position-if-not (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern count (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern count-if (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern count-if-not (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern mismatch (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern search (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern sort (form fn &key (:star form)) :lisp)
(data-assrt :define-caller-pattern stable-sort (form fn &key (:star form)) :lisp)
(data-assrt :define-caller-pattern merge (form form form fn &key (:star form)) :lisp)

;;; Lists
(data-assrt :define-caller-pattern car (form) :lisp)
(data-assrt :define-caller-pattern cdr (form) :lisp)
(data-assrt :define-caller-pattern caar (form) :lisp)
(data-assrt :define-caller-pattern cadr (form) :lisp)
(data-assrt :define-caller-pattern cdar (form) :lisp)
(data-assrt :define-caller-pattern cddr (form) :lisp)
(data-assrt :define-caller-pattern caaar (form) :lisp)
(data-assrt :define-caller-pattern caadr (form) :lisp)
(data-assrt :define-caller-pattern cadar (form) :lisp)
(data-assrt :define-caller-pattern caddr (form) :lisp)
(data-assrt :define-caller-pattern cdaar (form) :lisp)
(data-assrt :define-caller-pattern cdadr (form) :lisp)
(data-assrt :define-caller-pattern cddar (form) :lisp)
(data-assrt :define-caller-pattern cdddr (form) :lisp)
(data-assrt :define-caller-pattern caaaar (form) :lisp)
(data-assrt :define-caller-pattern caaadr (form) :lisp)
(data-assrt :define-caller-pattern caadar (form) :lisp)
(data-assrt :define-caller-pattern caaddr (form) :lisp)
(data-assrt :define-caller-pattern cadaar (form) :lisp)
(data-assrt :define-caller-pattern cadadr (form) :lisp)
(data-assrt :define-caller-pattern caddar (form) :lisp)
(data-assrt :define-caller-pattern cadddr (form) :lisp)
(data-assrt :define-caller-pattern cdaaar (form) :lisp)
(data-assrt :define-caller-pattern cdaadr (form) :lisp)
(data-assrt :define-caller-pattern cdadar (form) :lisp)
(data-assrt :define-caller-pattern cdaddr (form) :lisp)
(data-assrt :define-caller-pattern cddaar (form) :lisp)
(data-assrt :define-caller-pattern cddadr (form) :lisp)
(data-assrt :define-caller-pattern cdddar (form) :lisp)
(data-assrt :define-caller-pattern cddddr (form) :lisp)

(data-assrt :define-caller-pattern cons (form form) :lisp)
(data-assrt :define-caller-pattern tree-equal (form form &key (:star fn)) :lisp)
(data-assrt :define-caller-pattern endp (form) :lisp)
(data-assrt :define-caller-pattern list-length (form) :lisp)
(data-assrt :define-caller-pattern nth (form form) :lisp)

(data-assrt :define-caller-pattern first (form) :lisp)
(data-assrt :define-caller-pattern second (form) :lisp)
(data-assrt :define-caller-pattern third (form) :lisp)
(data-assrt :define-caller-pattern fourth (form) :lisp)
(data-assrt :define-caller-pattern fifth (form) :lisp)
(data-assrt :define-caller-pattern sixth (form) :lisp)
(data-assrt :define-caller-pattern seventh (form) :lisp)
(data-assrt :define-caller-pattern eighth (form) :lisp)
(data-assrt :define-caller-pattern ninth (form) :lisp)
(data-assrt :define-caller-pattern tenth (form) :lisp)

(data-assrt :define-caller-pattern rest (form) :lisp)
(data-assrt :define-caller-pattern nthcdr (form form) :lisp)
(data-assrt :define-caller-pattern last (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern list ((:star form)) :lisp)
(data-assrt :define-caller-pattern list* ((:star form)) :lisp)
(data-assrt :define-caller-pattern make-list (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern append ((:star form)) :lisp)
(data-assrt :define-caller-pattern copy-list (form) :lisp)
(data-assrt :define-caller-pattern copy-alist (form) :lisp)
(data-assrt :define-caller-pattern copy-tree (form) :lisp)
(data-assrt :define-caller-pattern revappend (form form) :lisp)
(data-assrt :define-caller-pattern nconc ((:star form)) :lisp)
(data-assrt :define-caller-pattern nreconc (form form) :lisp)
(data-assrt :define-caller-pattern push (form form) :lisp)
(data-assrt :define-caller-pattern pushnew (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern pop (form) :lisp)
(data-assrt :define-caller-pattern butlast (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern nbutlast (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern ldiff (form form) :lisp)
(data-assrt :define-caller-pattern rplaca (form form) :lisp)
(data-assrt :define-caller-pattern rplacd (form form) :lisp)

(data-assrt :define-caller-pattern subst (form form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern subst-if (form fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern subst-if-not (form fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nsubst (form form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nsubst-if (form fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nsubst-if-not (form fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern sublis (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nsublis (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern member (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern member-if (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern member-if-not (fn form &key (:star form)) :lisp)

(data-assrt :define-caller-pattern tailp (form form) :lisp)
(data-assrt :define-caller-pattern adjoin (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern union (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nunion (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern intersection (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nintersection (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern set-difference (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nset-difference (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern set-exclusive-or (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nset-exclusive-or (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern subsetp (form form &key (:star form)) :lisp)

(data-assrt :define-caller-pattern acons (form form form) :lisp)
(data-assrt :define-caller-pattern pairlis (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern assoc (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern assoc-if (fn form) :lisp)
(data-assrt :define-caller-pattern assoc-if-not (fn form) :lisp)
(data-assrt :define-caller-pattern rassoc (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern rassoc-if (fn form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern rassoc-if-not (fn form &key (:star form)) :lisp)

;;; Hash Tables
(data-assrt :define-caller-pattern make-hash-table (&key (:star form)) :lisp)
(data-assrt :define-caller-pattern hash-table-p (form) :lisp)
(data-assrt :define-caller-pattern gethash (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern remhash (form form) :lisp)
(data-assrt :define-caller-pattern maphash (fn form) :lisp)
(data-assrt :define-caller-pattern clrhash (form) :lisp)
(data-assrt :define-caller-pattern hash-table-count (form) :lisp)
(data-assrt :define-caller-pattern with-hash-table-iterator
  ((name form) (:star form)) :lisp2)
(data-assrt :define-caller-pattern hash-table-rehash-size (form) :lisp2)
(data-assrt :define-caller-pattern hash-table-rehash-threshold (form) :lisp2)
(data-assrt :define-caller-pattern hash-table-size (form) :lisp2)
(data-assrt :define-caller-pattern hash-table-test (form) :lisp2)
(data-assrt :define-caller-pattern sxhash (form) :lisp)

;;; Arrays
(data-assrt :define-caller-pattern make-array (form &key (:star form)) :lisp)
(data-assrt :define-variable-pattern array-rank-limit :lisp)
(data-assrt :define-variable-pattern array-dimension-limit :lisp)
(data-assrt :define-variable-pattern array-total-size-limit :lisp)
(data-assrt :define-caller-pattern vector ((:star form)) :lisp)
(data-assrt :define-caller-pattern aref (form (:star form)) :lisp)
(data-assrt :define-caller-pattern svref (form form) :lisp)
(data-assrt :define-caller-pattern array-element-type (form) :lisp)
(data-assrt :define-caller-pattern array-rank (form) :lisp)
(data-assrt :define-caller-pattern array-dimension (form form) :lisp)
(data-assrt :define-caller-pattern array-dimensions (form) :lisp)
(data-assrt :define-caller-pattern array-total-size (form) :lisp)
(data-assrt :define-caller-pattern array-in-bounds-p (form (:star form)) :lisp)
(data-assrt :define-caller-pattern array-row-major-index (form (:star form)) :lisp)
(data-assrt :define-caller-pattern row-major-aref (form form) :lisp2)
(data-assrt :define-caller-pattern adjustable-array-p (form) :lisp)

(data-assrt :define-caller-pattern bit (form (:star form)) :lisp)
(data-assrt :define-caller-pattern sbit (form (:star form)) :lisp)

(data-assrt :define-caller-pattern bit-and (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern bit-ior (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern bit-xor (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern bit-eqv (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern bit-nand (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern bit-nor (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern bit-andc1 (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern bit-andc2 (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern bit-orc1 (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern bit-orc2 (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern bit-not (form (:optional form)) :lisp)

(data-assrt :define-caller-pattern array-has-fill-pointer-p (form) :lisp)
(data-assrt :define-caller-pattern fill-pointer (form) :lisp)
(data-assrt :define-caller-pattern vector-push (form form) :lisp)
(data-assrt :define-caller-pattern vector-push-extend (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern vector-pop (form) :lisp)
(data-assrt :define-caller-pattern adjust-array (form form &key (:star form)) :lisp)

;;; Strings
(data-assrt :define-caller-pattern char (form form) :lisp)
(data-assrt :define-caller-pattern schar (form form) :lisp)
(data-assrt :define-caller-pattern string= (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string-equal (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string< (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string> (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string<= (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string>= (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string/= (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string-lessp (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string-greaterp (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string-not-greaterp (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string-not-lessp (form form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string-not-equal (form form &key (:star form)) :lisp)

(data-assrt :define-caller-pattern make-string (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string-trim (form form) :lisp)
(data-assrt :define-caller-pattern string-left-trim (form form) :lisp)
(data-assrt :define-caller-pattern string-right-trim (form form) :lisp)
(data-assrt :define-caller-pattern string-upcase (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string-downcase (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string-capitalize (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nstring-upcase (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nstring-downcase (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern nstring-capitalize (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern string (form) :lisp)

;;; Structures
(data-assrt :define-caller-pattern defstruct 
  ((:or name (name (:rest :ignore)))
   (:optional documentation-string)
   (:plus :ignore))
  :lisp)

;;; The Evaluator
(data-assrt :define-caller-pattern eval (form) :lisp)
(data-assrt :define-variable-pattern *evalhook* :lisp)
(data-assrt :define-variable-pattern *applyhook* :lisp)
(data-assrt :define-caller-pattern evalhook (form fn fn &optional :ignore) :lisp)
(data-assrt :define-caller-pattern applyhook (fn form fn fn &optional :ignore) :lisp)
(data-assrt :define-caller-pattern constantp (form) :lisp)

;;; Streams
(data-assrt :define-variable-pattern *standard-input* :lisp)
(data-assrt :define-variable-pattern *standard-output* :lisp)
(data-assrt :define-variable-pattern *error-output* :lisp)
(data-assrt :define-variable-pattern *query-io* :lisp)
(data-assrt :define-variable-pattern *debug-io* :lisp)
(data-assrt :define-variable-pattern *terminal-io* :lisp)
(data-assrt :define-variable-pattern *trace-output* :lisp)
(data-assrt :define-caller-pattern make-synonym-stream (symbol) :lisp)
(data-assrt :define-caller-pattern make-broadcast-stream ((:star form)) :lisp)
(data-assrt :define-caller-pattern make-concatenated-stream ((:star form)) :lisp)
(data-assrt :define-caller-pattern make-two-way-stream (form form) :lisp)
(data-assrt :define-caller-pattern make-echo-stream (form form) :lisp)
(data-assrt :define-caller-pattern make-string-input-stream (form &optional form form)
  :lisp)
(data-assrt :define-caller-pattern make-string-output-stream (&key (:star form)) :lisp)
(data-assrt :define-caller-pattern get-output-stream-string (form) :lisp)

(data-assrt :define-caller-pattern with-open-stream
  ((var form)
   (:star declaration)
   (:star form))
  :lisp)

(data-assrt :define-caller-pattern with-input-from-string
  ((var form &key (:star form))
   (:star declaration)
   (:star form))
  :lisp)

(data-assrt :define-caller-pattern with-output-to-string
  ((var (:optional form))
   (:star declaration)
   (:star form))
  :lisp)
(data-assrt :define-caller-pattern streamp (form) :lisp)
(data-assrt :define-caller-pattern open-stream-p (form) :lisp2)
(data-assrt :define-caller-pattern input-stream-p (form) :lisp)
(data-assrt :define-caller-pattern output-stream-p (form) :lisp)
(data-assrt :define-caller-pattern stream-element-type (form) :lisp)
(data-assrt :define-caller-pattern close (form (:rest :ignore)) :lisp)
(data-assrt :define-caller-pattern broadcast-stream-streams (form) :lisp2)
(data-assrt :define-caller-pattern concatenated-stream-streams (form) :lisp2)
(data-assrt :define-caller-pattern echo-stream-input-stream (form) :lisp2)
(data-assrt :define-caller-pattern echo-stream-output-stream (form) :lisp2)
(data-assrt :define-caller-pattern synonym-stream-symbol (form) :lisp2)
(data-assrt :define-caller-pattern two-way-stream-input-stream (form) :lisp2)
(data-assrt :define-caller-pattern two-way-stream-output-stream (form) :lisp2)
(data-assrt :define-caller-pattern interactive-stream-p (form) :lisp2)
(data-assrt :define-caller-pattern stream-external-format (form) :lisp2)

;;; Reader
(data-assrt :define-variable-pattern *read-base* :lisp)
(data-assrt :define-variable-pattern *read-suppress* :lisp)
(data-assrt :define-variable-pattern *read-eval* :lisp2)
(data-assrt :define-variable-pattern *readtable* :lisp)
(data-assrt :define-caller-pattern copy-readtable (&optional form form) :lisp)
(data-assrt :define-caller-pattern readtablep (form) :lisp)
(data-assrt :define-caller-pattern set-syntax-from-char (form form &optional form form)
  :lisp)
(data-assrt :define-caller-pattern set-macro-character (form fn &optional form) :lisp)
(data-assrt :define-caller-pattern get-macro-character (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern make-dispatch-macro-character (form &optional form form)
  :lisp)
(data-assrt :define-caller-pattern set-dispatch-macro-character
  (form form fn (:optional form)) :lisp)
(data-assrt :define-caller-pattern get-dispatch-macro-character
  (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern readtable-case (form) :lisp2)
(data-assrt :define-variable-pattern *print-readably* :lisp2)
(data-assrt :define-variable-pattern *print-escape* :lisp)
(data-assrt :define-variable-pattern *print-pretty* :lisp)
(data-assrt :define-variable-pattern *print-circle* :lisp)
(data-assrt :define-variable-pattern *print-base* :lisp)
(data-assrt :define-variable-pattern *print-radix* :lisp)
(data-assrt :define-variable-pattern *print-case* :lisp)
(data-assrt :define-variable-pattern *print-gensym* :lisp)
(data-assrt :define-variable-pattern *print-level* :lisp)
(data-assrt :define-variable-pattern *print-length* :lisp)
(data-assrt :define-variable-pattern *print-array* :lisp)
(data-assrt :define-caller-pattern with-standard-io-syntax 
  ((:star declaration)
   (:star form))
  :lisp2)

(data-assrt :define-caller-pattern read (&optional form form form form) :lisp)
(data-assrt :define-variable-pattern *read-default-float-format* :lisp)
(data-assrt :define-caller-pattern read-preserving-whitespace
  (&optional form form form form) :lisp)
(data-assrt :define-caller-pattern read-delimited-list (form &optional form form) :lisp)
(data-assrt :define-caller-pattern read-line (&optional form form form form) :lisp)
(data-assrt :define-caller-pattern read-char (&optional form form form form) :lisp)
(data-assrt :define-caller-pattern unread-char (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern peek-char (&optional form form form form) :lisp)
(data-assrt :define-caller-pattern listen ((:optional form)) :lisp)
(data-assrt :define-caller-pattern read-char-no-hang ((:star form)) :lisp)
(data-assrt :define-caller-pattern clear-input ((:optional form)) :lisp)
(data-assrt :define-caller-pattern read-from-string (form (:star form)) :lisp)
(data-assrt :define-caller-pattern parse-integer (form &rest :ignore) :lisp)
(data-assrt :define-caller-pattern read-byte ((:star form)) :lisp)

(data-assrt :define-caller-pattern write (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern prin1 (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern print (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern pprint (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern princ (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern write-to-string (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern prin1-to-string (form) :lisp)
(data-assrt :define-caller-pattern princ-to-string (form) :lisp)
(data-assrt :define-caller-pattern write-char (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern write-string (form &optional form &key (:star form))
  :lisp)
(data-assrt :define-caller-pattern write-line (form &optional form &key (:star form))
  :lisp)
(data-assrt :define-caller-pattern terpri ((:optional form)) :lisp)
(data-assrt :define-caller-pattern fresh-line ((:optional form)) :lisp)
(data-assrt :define-caller-pattern finish-output ((:optional form)) :lisp)
(data-assrt :define-caller-pattern force-output ((:optional form)) :lisp)
(data-assrt :define-caller-pattern clear-output ((:optional form)) :lisp)
(data-assrt :define-caller-pattern print-unreadable-object 
  ((form form &key (:star form))
   (:star declaration)
   (:star form))
  :lisp2)
(data-assrt :define-caller-pattern write-byte (form form) :lisp)
(data-assrt :define-caller-pattern format
  (destination
   control-string
   (:rest format-arguments))
  :lisp)

(data-assrt :define-caller-pattern y-or-n-p (control-string (:star form)) :lisp)
(data-assrt :define-caller-pattern yes-or-no-p (control-string (:star form)) :lisp)

;;; Pathnames
(data-assrt :define-caller-pattern wild-pathname-p (form &optional form) :lisp2)
(data-assrt :define-caller-pattern pathname-match-p (form form) :lisp2)
(data-assrt :define-caller-pattern translate-pathname (form form form &key (:star form))
  :lisp2)

(data-assrt :define-caller-pattern logical-pathname (form) :lisp2)
(data-assrt :define-caller-pattern translate-logical-pathname (form &key (:star form))
  :lisp2)
(data-assrt :define-caller-pattern logical-pathname-translations (form) :lisp2)
(data-assrt :define-caller-pattern load-logical-pathname-translations (form) :lisp2)
(data-assrt :define-caller-pattern compile-file-pathname (form &key form) :lisp2)

(data-assrt :define-caller-pattern pathname (form) :lisp)
(data-assrt :define-caller-pattern truename (form) :lisp)
(data-assrt :define-caller-pattern parse-namestring ((:star form)) :lisp)
(data-assrt :define-caller-pattern merge-pathnames ((:star form)) :lisp)
(data-assrt :define-variable-pattern *default-pathname-defaults* :lisp)
(data-assrt :define-caller-pattern make-pathname ((:star form)) :lisp)
(data-assrt :define-caller-pattern pathnamep (form) :lisp)
(data-assrt :define-caller-pattern pathname-host (form) :lisp)
(data-assrt :define-caller-pattern pathname-device (form) :lisp)
(data-assrt :define-caller-pattern pathname-directory (form) :lisp)
(data-assrt :define-caller-pattern pathname-name (form) :lisp)
(data-assrt :define-caller-pattern pathname-type (form) :lisp)
(data-assrt :define-caller-pattern pathname-version (form) :lisp)
(data-assrt :define-caller-pattern namestring (form) :lisp)
(data-assrt :define-caller-pattern file-namestring (form) :lisp)
(data-assrt :define-caller-pattern directory-namestring (form) :lisp)
(data-assrt :define-caller-pattern host-namestring (form) :lisp)
(data-assrt :define-caller-pattern enough-namestring (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern user-homedir-pathname (&optional form) :lisp)
(data-assrt :define-caller-pattern open (form &key (:star form)) :lisp)
(data-assrt :define-caller-pattern with-open-file
  ((var form (:rest :ignore))
   (:star declaration)
   (:star form))
 :lisp)

(data-assrt :define-caller-pattern rename-file (form form) :lisp)
(data-assrt :define-caller-pattern delete-file (form) :lisp)
(data-assrt :define-caller-pattern probe-file (form) :lisp)
(data-assrt :define-caller-pattern file-write-date (form) :lisp)
(data-assrt :define-caller-pattern file-author (form) :lisp)
(data-assrt :define-caller-pattern file-position (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern file-length (form) :lisp)
(data-assrt :define-caller-pattern file-string-length (form form) :lisp2)
(data-assrt :define-caller-pattern load (form &key (:star form)) :lisp)
(data-assrt :define-variable-pattern *load-verbose* :lisp)
(data-assrt :define-variable-pattern *load-print* :lisp2)
(data-assrt :define-variable-pattern *load-pathname* :lisp2)
(data-assrt :define-variable-pattern *load-truename* :lisp2)
(data-assrt :define-caller-pattern make-load-form (form) :lisp2)
(data-assrt :define-caller-pattern make-load-form-saving-slots (form &optional form)
  :lisp2)
(data-assrt :define-caller-pattern directory (form &key (:star form)) :lisp)

;;; Errors
(data-assrt :define-caller-pattern error (form (:star form)) :lisp)
(data-assrt :define-caller-pattern cerror (form form (:star form)) :lisp)
(data-assrt :define-caller-pattern warn (form (:star form)) :lisp)
(data-assrt :define-variable-pattern *break-on-warnings* :lisp)
(data-assrt :define-caller-pattern break (&optional form (:star form)) :lisp)
(data-assrt :define-caller-pattern check-type (form form (:optional form)) :lisp)
(data-assrt :define-caller-pattern assert 
  (form
   (:optional ((:star var))
	      (:optional form (:star form)))) 
  :lisp)
(data-assrt :define-caller-pattern etypecase (form (:star (symbol (:star form)))) :lisp)
(data-assrt :define-caller-pattern ctypecase (form (:star (symbol (:star form)))) :lisp)
(data-assrt :define-caller-pattern ecase
  (form
   (:star ((:or symbol ((:star symbol)))
	   (:star form))))
  :lisp)
(data-assrt :define-caller-pattern ccase 
  (form
   (:star ((:or symbol ((:star symbol)))
	   (:star form))))
  :lisp)

;;; The Compiler
(data-assrt :define-caller-pattern compile (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern compile-file (form &key (:star form)) :lisp)
(data-assrt :define-variable-pattern *compile-verbose* :lisp2)
(data-assrt :define-variable-pattern *compile-print* :lisp2)
(data-assrt :define-variable-pattern *compile-file-pathname* :lisp2)
(data-assrt :define-variable-pattern *compile-file-truename* :lisp2)
(data-assrt :define-caller-pattern load-time-value (form (:optional form)) :lisp2)
(data-assrt :define-caller-pattern disassemble (form) :lisp)
(data-assrt :define-caller-pattern function-lambda-expression (fn) :lisp2)
(data-assrt :define-caller-pattern with-compilation-unit (((:star :ignore)) (:star form)) 
  :lisp2)

;;; Documentation
(data-assrt :define-caller-pattern documentation (form form) :lisp)
(data-assrt :define-caller-pattern trace ((:star form)) :lisp)
(data-assrt :define-caller-pattern untrace ((:star form)) :lisp)
(data-assrt :define-caller-pattern step (form) :lisp)
(data-assrt :define-caller-pattern time (form) :lisp)
(data-assrt :define-caller-pattern describe (form &optional form) :lisp)
(data-assrt :define-caller-pattern describe-object (form &optional form) :lisp2)
(data-assrt :define-caller-pattern inspect (form) :lisp)
(data-assrt :define-caller-pattern room ((:optional form)) :lisp)
(data-assrt :define-caller-pattern ed ((:optional form)) :lisp)
(data-assrt :define-caller-pattern dribble ((:optional form)) :lisp)
(data-assrt :define-caller-pattern apropos (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern apropos-list (form (:optional form)) :lisp)
(data-assrt :define-caller-pattern get-decoded-time () :lisp)
(data-assrt :define-caller-pattern get-universal-time () :lisp)
(data-assrt :define-caller-pattern decode-universal-time (form &optional form) :lisp)
(data-assrt :define-caller-pattern encode-universal-time 
  (form form form form form form &optional form) :lisp)
(data-assrt :define-caller-pattern get-internal-run-time () :lisp)
(data-assrt :define-caller-pattern get-internal-real-time () :lisp)
(data-assrt :define-caller-pattern sleep (form) :lisp)

(data-assrt :define-caller-pattern lisp-implementation-type () :lisp)
(data-assrt :define-caller-pattern lisp-implementation-version () :lisp)
(data-assrt :define-caller-pattern machine-type () :lisp)
(data-assrt :define-caller-pattern machine-version () :lisp)
(data-assrt :define-caller-pattern machine-instance () :lisp)
(data-assrt :define-caller-pattern software-type () :lisp)
(data-assrt :define-caller-pattern software-version () :lisp)
(data-assrt :define-caller-pattern short-site-name () :lisp)
(data-assrt :define-caller-pattern long-site-name () :lisp)
(data-assrt :define-variable-pattern *features* :lisp)

(data-assrt :define-caller-pattern identity (form) :lisp)

;;; Pretty Printing
(data-assrt :define-variable-pattern *print-pprint-dispatch* :lisp2)
(data-assrt :define-variable-pattern *print-right-margin* :lisp2)
(data-assrt :define-variable-pattern *print-miser-width* :lisp2)
(data-assrt :define-variable-pattern *print-lines* :lisp2)
(data-assrt :define-caller-pattern pprint-newline (form &optional form) :lisp2)
(data-assrt :define-caller-pattern pprint-logical-block
  ((var form &key (:star form))
   (:star form))
  :lisp2)
(data-assrt :define-caller-pattern pprint-exit-if-list-exhausted () :lisp2)
(data-assrt :define-caller-pattern pprint-pop () :lisp2)
(data-assrt :define-caller-pattern pprint-indent (form form &optional form) :lisp2)
(data-assrt :define-caller-pattern pprint-tab (form form form &optional form) :lisp2)
(data-assrt :define-caller-pattern pprint-fill (form form &optional form form) :lisp2)
(data-assrt :define-caller-pattern pprint-linear (form form &optional form form) :lisp2)
(data-assrt :define-caller-pattern pprint-tabular (form form &optional form form form)
  :lisp2)
(data-assrt :define-caller-pattern formatter (control-string) :lisp2)
(data-assrt :define-caller-pattern copy-pprint-dispatch (&optional form) :lisp2)
(data-assrt :define-caller-pattern pprint-dispatch (form &optional form) :lisp2)
(data-assrt :define-caller-pattern set-pprint-dispatch (form form &optional form form)
  :lisp2)

;;; CLOS
(data-assrt :define-caller-pattern add-method (fn form) :lisp2)
(data-assrt :define-caller-pattern call-method (form form) :lisp2)
(data-assrt :define-caller-pattern call-next-method ((:star form)) :lisp2)
(data-assrt :define-caller-pattern change-class (form form) :lisp2)
(data-assrt :define-caller-pattern class-name (form) :lisp2)
(data-assrt :define-caller-pattern class-of (form) :lisp2)
(data-assrt :define-caller-pattern compute-applicable-methods (fn (:star form)) :lisp2)
(data-assrt :define-caller-pattern defclass (name &rest :ignore) :lisp2)
(data-assrt :define-caller-pattern defgeneric (name lambda-list &rest :ignore) :lisp2)
(data-assrt :define-caller-pattern define-method-combination 
  (name lambda-list ((:star :ignore))
	(:optional ((:eq :arguments) :ignore))
	(:optional ((:eq :generic-function) :ignore))
	(:star (:or declaration documentation-string))
	(:star form))
  :lisp2)
(data-assrt :define-caller-pattern defmethod 
  (name (:star symbol) lambda-list
	(:star (:or declaration documentation-string))
	(:star form))
  :lisp2)
(data-assrt :define-caller-pattern ensure-generic-function (name &key (:star form)) :lisp2)
(data-assrt :define-caller-pattern find-class (form &optional form form) :lisp2)
(data-assrt :define-caller-pattern find-method (fn &rest :ignore) :lisp2)
(data-assrt :define-caller-pattern function-keywords (&rest :ignore) :lisp2)
(data-assrt :define-caller-pattern generic-flet (((:star (name lambda-list))) (:star form))
  :lisp2)
(data-assrt :define-caller-pattern generic-labels 
  (((:star (name lambda-list))) (:star form))
  :lisp2)
(data-assrt :define-caller-pattern generic-function (lambda-list) :lisp2)
(data-assrt :define-caller-pattern initialize-instance (form &key (:star form)) :lisp2)
(data-assrt :define-caller-pattern invalid-method-error (fn form (:star form)) :lisp2)
(data-assrt :define-caller-pattern make-instance (fn (:star form)) :lisp2)
(data-assrt :define-caller-pattern make-instances-obsolete (fn) :lisp2)
(data-assrt :define-caller-pattern method-combination-error (form (:star form)) :lisp2)
(data-assrt :define-caller-pattern method-qualifiers (fn) :lisp2)
(data-assrt :define-caller-pattern next-method-p () :lisp2)
(data-assrt :define-caller-pattern no-applicable-method (fn (:star form)) :lisp2)
(data-assrt :define-caller-pattern no-next-method (fn (:star form)) :lisp2)
(data-assrt :define-caller-pattern print-object (form form) :lisp2)
(data-assrt :define-caller-pattern reinitialize-instance (form (:star form)) :lisp2)
(data-assrt :define-caller-pattern remove-method (fn form) :lisp2)
(data-assrt :define-caller-pattern shared-initialize (form form (:star form)) :lisp2)
(data-assrt :define-caller-pattern slot-boundp (form form) :lisp2)
(data-assrt :define-caller-pattern slot-exists-p (form form) :lisp2)
(data-assrt :define-caller-pattern slot-makeunbound (form form) :lisp2)
(data-assrt :define-caller-pattern slot-missing (fn form form form &optional form) :lisp2)
(data-assrt :define-caller-pattern slot-unbound (fn form form) :lisp2)
(data-assrt :define-caller-pattern slot-value (form form) :lisp2)
(data-assrt :define-caller-pattern update-instance-for-different-class 
  (form form (:star form)) :lisp2)
(data-assrt :define-caller-pattern update-instance-for-redefined-class 
  (form form (:star form)) :lisp2)
(data-assrt :define-caller-pattern with-accessors
  (((:star :ignore)) form
   (:star declaration)
   (:star form))
  :lisp2)
(data-assrt :define-caller-pattern with-added-methods
  ((name lambda-list) form
   (:star form))
  :lisp2)
(data-assrt :define-caller-pattern with-slots
  (((:star :ignore)) form
   (:star declaration)
   (:star form))
  :lisp2)

;;; Conditions
(data-assrt :define-caller-pattern signal (form (:star form)) :lisp2)
(data-assrt :define-variable-pattern *break-on-signals* :lisp2)
(data-assrt :define-caller-pattern handler-case (form (:star (form ((:optional var))
						       (:star form))))
  :lisp2)
(data-assrt :define-caller-pattern ignore-errors ((:star form)) :lisp2)
(data-assrt :define-caller-pattern handler-bind (((:star (form form)))
				     (:star form))
  :lisp2)
(data-assrt :define-caller-pattern define-condition (name &rest :ignore) :lisp2)
(data-assrt :define-caller-pattern make-condition (form &rest :ignore) :lisp2)
(data-assrt :define-caller-pattern with-simple-restart
  ((name form (:star form)) (:star form)) :lisp2)
(data-assrt :define-caller-pattern restart-case 
  (form
   (:star (form form (:star form))))
  :lisp2)
(data-assrt :define-caller-pattern restart-bind
  (((:star (name fn &key (:star form))))
   (:star form))
  :lisp2)
(data-assrt :define-caller-pattern with-condition-restarts
  (form form
	(:star declaration)
	(:star form))
  :lisp2)
(data-assrt :define-caller-pattern compute-restarts (&optional form) :lisp2)
(data-assrt :define-caller-pattern restart-name (form) :lisp2)
(data-assrt :define-caller-pattern find-restart (form &optional form) :lisp2)
(data-assrt :define-caller-pattern invoke-restart (form (:star form)) :lisp2)
(data-assrt :define-caller-pattern invoke-restart-interactively (form) :lisp2)
(data-assrt :define-caller-pattern abort (&optional form) :lisp2)
(data-assrt :define-caller-pattern continue (&optional form) :lisp2)
(data-assrt :define-caller-pattern muffle-warning (&optional form) :lisp2)
(data-assrt :define-caller-pattern store-value (form &optional form) :lisp2)
(data-assrt :define-caller-pattern use-value (form &optional form) :lisp2)
(data-assrt :define-caller-pattern invoke-debugger (form) :lisp2)
(data-assrt :define-variable-pattern *debugger-hook* :lisp2)
(data-assrt :define-caller-pattern simple-condition-format-string (form) :lisp2)
(data-assrt :define-caller-pattern simple-condition-format-arguments (form) :lisp2)
(data-assrt :define-caller-pattern type-error-datum (form) :lisp2)
(data-assrt :define-caller-pattern type-error-expected-type (form) :lisp2)
(data-assrt :define-caller-pattern package-error-package (form) :lisp2)
(data-assrt :define-caller-pattern stream-error-stream (form) :lisp2)
(data-assrt :define-caller-pattern file-error-pathname (form) :lisp2)
(data-assrt :define-caller-pattern cell-error-name (form) :lisp2)
(data-assrt :define-caller-pattern arithmetic-error-operation (form) :lisp2)
(data-assrt :define-caller-pattern arithmetic-error-operands (form) :lisp2)

;;; For ZetaLisp Flavors
(data-assrt :define-caller-pattern send (form fn (:star form)) :flavors)

