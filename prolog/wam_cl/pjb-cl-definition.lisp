;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cl-definition.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file gives a description of all the standard symbols in
;;;;    the CL package.  These descriptions can be used to generate
;;;;    automatically various things.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-12 <PJB> Created.
;;;;    2014-08-30 <PJB> Added handling of parameters in define-declare-macro.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.CL-DEFINITIONS"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM")
  (:export "SYMBOL-TYPE-OF-DECLARATIONS"
           "SYMBOL-DECLARATION-INFOS"
           "SYMBOL-INFO"
           "MAP-SYMBOL-INFOS"
           "*SYMBOL-CATEGORIES*")
  (:documentation "
This file gives a description of all the standard symbols in
the CL package.  These descriptions can be used to generate
automatically various things.


Copyright Pascal J. Bourguignon 2012 - 2014

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.
"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.CL-DEFINITIONS")



(defvar *declarations*
  (make-hash-table)
  "Maps symbols to a-list (type-of-declaration) of p-lists (parameter).")


(defun enter (symbol type-of-declaration parameters)
  (let ((sym (gethash symbol *declarations*)))
    (let ((entry (assoc type-of-declaration sym)))
      (if entry
        (setf (cdr entry) parameters)
        (push (cons type-of-declaration parameters) sym)))
    (setf (gethash symbol *declarations*) sym)))


(defun symbol-type-of-declarations (symbol)
  "
SYMBOL:         A symbol exported from the COMMON-LISP package.

RETURN:         a list of keywords denoting the type of declarations
                the symbol has.
"
  (mapcar (function car) (gethash symbol *declarations*)))


(defun symbol-declaration-infos (symbol type-of-declaration)
  "
SYMBOL:         A symbol exported from the COMMON-LISP package.

TYPE-OF-DECLARATION:
                A keyword denoting the type of declaration wanted.

RETURN:         A p-list containing the parameters of the
                TYPE-OF-DECLARATION declaration of the SYMBOL.
"
  (cdr (assoc type-of-declaration (gethash symbol *declarations*))))


(defun symbol-info (symbol)
  "
SYMBOL:         A symbol exported from the COMMON-LISP package.

RETURN:         an a-list (type-of-declaration) of p-lists (parameter)
                describing the specifications of the given CL SYMBOL.
"
  (gethash symbol *declarations*))


(defun map-symbol-infos (fun)
  "
DO:             Calls the function FUN on each symbol exported from
                the COMMON-LISP package, unordered.

FUN:            A function taking a symbol and an a-list
                (type-of-declaration) of p-lists (parameter)
                describing the specifications of the given CL SYMBOL.
"
  (maphash fun *declarations*))


;; (enter 'vector :compound-type-specifier '((:lambda-list . (element-type &optional size)) (:compound-type-kind . :specializing)))
;; (enter 'vector :system-class '(:superclasses  (array sequence)))
;; (com.informatimago.common-lisp.cesarum.utility:print-hashtable *declarations*)
;; (symbol-type-of-declarations 'vector)(:system-class :compound-type-specifier)
;; (symbol-declaration-infos 'vector :system-class)

(lambda-list-parameters  (parse-lambda-list '(name declaration-lambda-list &key documentation) :macro))
;; (#<&mandatory name #x3020022DE0FD> #<&mandatory declaration-lambda-list #x3020022DE0AD> #<&key documentation #x3020022DE03D>)
(make-lambda-list        (parse-lambda-list '(name declaration-lambda-list &key documentation) :macro))
;; (name declaration-lambda-list &key documentation)
(make-argument-list-form (parse-lambda-list '(name declaration-lambda-list &key documentation) :macro))
;;(append (list name declaration-lambda-list) (list :documentation documentation))
(make-argument-list      (parse-lambda-list '(name declaration-lambda-list &key documentation) :macro))
;; (name declaration-lambda-list :documentation documentation nil)
(make-help               (parse-lambda-list '(name declaration-lambda-list &key documentation) :macro))
;; ((:mandatory . "name") (:mandatory . "declaration-lambda-list") (:key . "documentation"))


(defmacro define-declare-macro (type-of-declaration parameter-lambda-list)
  (let* ((ll (parse-lambda-list parameter-lambda-list :macro))
         (parameters (mapcan (lambda (parameter)
                               (list (intern (string (parameter-name parameter)) :keyword)
                                     (list 'list ''quote (parameter-name parameter))))
                             (lambda-list-parameters ll))))
    (print parameters)
    `(defmacro ,(intern (concatenate 'string "DECLARE-" (string type-of-declaration)))
       ,parameter-lambda-list
       `(enter ',,(first parameter-lambda-list)
               ,,(intern (string type-of-declaration) :keyword)
               (list ,,@parameters)))))

(define-declare-macro declaration             (name declaration-lambda-list &key documentation))
(define-declare-macro compound-type-specifier (name type-lambda-list kind &key documentation))
(define-declare-macro type                    (name supertype    &key documentation definition))
(define-declare-macro class                   (name superclasses &key documentation (slots ())))
(define-declare-macro system-class            (name superclasses &key documentation (slots ())))
(define-declare-macro condition-type          (name superclasses &key documentation (slots ())))
(define-declare-macro symbol                  (name &key documentation))
(define-declare-macro lambda-list-keyword     (name &key documentation))
(define-declare-macro variable                (name &key documentation))
(define-declare-macro constant-variable       (name &key documentation))
(define-declare-macro generic-function          (name generic-lambda-list
                                                      &key documentation
                                                      (signals :unspecified)
                                                      (result-type t)))
(define-declare-macro standard-generic-function (name generic-lambda-list
                                                      &key documentation
                                                      (signals :unspecified)
                                                      (result-type t)))
(define-declare-macro accessor         (name accessor-lambda-list   &key documentation (signals :unspecified) (result-type t)))
(define-declare-macro function         (name function-lambda-list   &key documentation (signals :unspecified) (result-type t)))
(define-declare-macro macro            (name macro-lambda-list      &key documentation (signals :unspecified)))
(define-declare-macro local-function   (name function-lambda-list   &key documentation (signals :unspecified) context (result-type t)))
(define-declare-macro local-macro      (name macro-lambda-list      &key documentation (signals :unspecified) context))
(define-declare-macro restart          (name restart-lambda-list    &key documentation))
(define-declare-macro special-operator (name macro-lambda-list      &key documentation (signals :unspecified) context))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMMON-LISP description.
;;;

(defparameter *symbol-categories*
  '(

    (declarations
     DECLARATION DYNAMIC-EXTENT FTYPE IGNORABLE IGNORE INLINE NOTINLINE
     OPTIMIZE SPECIAL TYPE)

    (compound-type-specifier
     and or not eql member array simple-array vector simple-vector
     bit-vector simple-bit-vector string base-string simple-string
     simple-base-string real complex float short-float single-float
     double-float long-float rational integer signed-byte
     unsigned-byte mod function cons values satisfies)

    (type
     EXTENDED-CHAR BASE-CHAR STANDARD-CHAR BASE-STRING SIMPLE-STRING
     SIMPLE-BASE-STRING FIXNUM BIGNUM SIGNED-BYTE UNSIGNED-BYTE BIT
     SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT COMPILED-FUNCTION
     SIMPLE-ARRAY SIMPLE-VECTOR SIMPLE-BIT-VECTOR NIL)

    (class
     STANDARD-OBJECT  STRUCTURE-OBJECT)

    (system-class
     t array method number stream symbol package restart function
     pathname sequence character readtable hash-table random-state
     method-combination vector string bit-vector list cons
     built-in-class standard-class structure-class generic-function
     standard-generic-function standard-method real complex float
     rational ratio integer logical-pathname class echo-stream
     file-stream string-stream synonym-stream two-way-stream
     broadcast-stream concatenated-stream null)

    (condition-type
     condition serious-condition simple-condition warning
     storage-condition error arithmetic-error cell-error control-error
     file-error package-error parse-error print-not-readable
     program-error stream-error type-error division-by-zero
     floating-point-inexact floating-point-invalid-operation
     floating-point-overflow floating-point-underflow unbound-slot
     unbound-variable undefined-function reader-error simple-error
     simple-type-error simple-warning end-of-file style-warning)


    (symbol
     declare lambda)

    (lambda-list-keyword
     &optional &rest &aux &key &allow-other-keys &body &environment
     &whole)

    (variable
     *break-on-signals* *compile-file-pathname*
     *compile-file-truename* *compile-print* *compile-verbose*
     *debugger-hook* *debug-io* *default-pathname-defaults*
     *error-output* *features* *gensym-counter* *load-pathname*
     *load-print* *load-truename* *load-verbose* *macroexpand-hook*
     *modules* *package* *print-array* *print-base* *print-case*
     *print-circle* *print-escape* *print-gensym* *print-length*
     *print-level* *print-lines* *print-miser-width*
     *print-pprint-dispatch* *print-pretty* *print-radix*
     *print-readably* *print-right-margin* *query-io* *random-state*
     *read-base* *read-default-float-format* *read-eval*
     *read-suppress* *readtable* *standard-input* *standard-output*
     *terminal-io* *trace-output* - / // /// * ** *** + ++ +++ )

    (constant-variable
     nil t pi array-dimension-limit array-rank-limit
     array-total-size-limit boole-1 boole-2 boole-andc1 boole-andc2
     boole-and boole-c1 boole-c2 boole-clr boole-eqv boole-ior boole-nand
     boole-nor boole-orc1 boole-orc2 boole-set boole-xor
     call-arguments-limit char-code-limit double-float-epsilon
     double-float-negative-epsilon internal-time-units-per-second
     lambda-list-keywords lambda-parameters-limit
     least-negative-double-float least-negative-long-float
     least-negative-normalized-double-float
     least-negative-normalized-long-float
     least-negative-normalized-short-float
     least-negative-normalized-single-float least-negative-short-float
     least-negative-single-float least-positive-double-float
     least-positive-long-float least-positive-normalized-double-float
     least-positive-normalized-long-float
     least-positive-normalized-short-float
     least-positive-normalized-single-float least-positive-short-float
     least-positive-single-float long-float-epsilon
     long-float-negative-epsilon most-negative-double-float
     most-negative-fixnum most-negative-long-float
     most-negative-short-float most-negative-single-float
     most-positive-double-float most-positive-fixnum
     most-positive-long-float most-positive-short-float
     most-positive-single-float multiple-values-limit short-float-epsilon
     short-float-negative-epsilon single-float-epsilon
     single-float-negative-epsilon)

    (accessor
     caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar
     caddar cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr
     cdar cddaar cddadr cddar cdddar cddddr cdddr cddr cdr first second
     third fourth fifth sixth seventh eighth ninth tenth rest nth elt
     subseq aref row-major-aref bit sbit svref char schar fill-pointer get
     getf gethash ldb mask-field compiler-macro-function macro-function
     fdefinition find-class logical-pathname-translations readtable-case
     symbol-function symbol-plist symbol-value values)

    (function
     1- 1+ abort abs acons acos acosh adjoin adjustable-array-p
     adjust-array alpha-char-p alphanumericp append apply apropos
     apropos-list arithmetic-error-operands arithmetic-error-operation
     array-dimension array-dimensions array-displacement
     array-element-type array-has-fill-pointer-p array-in-bounds-p
     arrayp array-rank array-row-major-index array-total-size ash asin
     asinh assoc assoc-if assoc-if-not atan atanh atom bit-andc1
     bit-andc2 bit-and bit-eqv bit-ior bit-nand bit-nor bit-not
     bit-orc1 bit-orc2 bit-vector-p bit-xor boole both-case-p boundp
     break broadcast-stream-streams butlast byte byte-position
     byte-size ceiling cell-error-name cerror character characterp
     char-code char-downcase char-equal char<= char< char= char>=
     char> char/= char-greaterp char-int char-lessp char-name
     char-not-equal char-not-greaterp char-not-lessp char-upcase cis
     class-of clear-input clear-output close clrhash code-char coerce
     compiled-function-p compile-file compile-file-pathname compile
     complement complex complexp compute-restarts
     concatenated-stream-streams concatenate conjugate cons consp
     constantly constantp continue copy-alist copy-list
     copy-pprint-dispatch copy-readtable copy-seq copy-structure
     copy-symbol copy-tree cos cosh count count-if count-if-not
     decode-float decode-universal-time delete-duplicates delete-file
     delete delete-if delete-if-not delete-package denominator
     deposit-field describe digit-char digit-char-p directory
     directory-namestring disassemble dpb dribble
     echo-stream-input-stream echo-stream-output-stream ed endp
     enough-namestring ensure-directories-exist
     ensure-generic-function eq eql equal equalp error eval evenp
     every exp export expt fboundp fceiling ffloor file-author
     file-error-pathname file-length file-namestring file-position
     file-string-length file-write-date fill find-all-symbols find
     find-if find-if-not find-package find-restart find-symbol
     finish-output float-digits float floatp float-precision
     float-radix float-sign floor fmakunbound force-output format
     fresh-line fround ftruncate funcall <= < = >= > - /= / * +
     function-lambda-expression functionp gcd gensym gentemp
     get-decoded-time get-dispatch-macro-character
     get-internal-real-time get-internal-run-time get-macro-character
     get-output-stream-string get-properties get-setf-expansion
     get-universal-time graphic-char-p hash-table-count hash-table-p
     hash-table-rehash-size hash-table-rehash-threshold
     hash-table-size hash-table-test host-namestring identity imagpart
     import input-stream-p inspect integer-decode-float integer-length
     integerp interactive-stream-p intern intersection
     invalid-method-error invoke-debugger invoke-restart
     invoke-restart-interactively isqrt keywordp last lcm ldb-test
     ldiff length lisp-implementation-type lisp-implementation-version
     list-all-packages listen list list* list-length listp load
     load-logical-pathname-translations logandc1 logandc2 logand
     logbitp logcount logeqv log logical-pathname logior lognand
     lognor lognot logorc1 logorc2 logtest logxor long-site-name
     lower-case-p machine-instance machine-type machine-version
     macroexpand-1 macroexpand make-array make-broadcast-stream
     make-concatenated-stream make-condition
     make-dispatch-macro-character make-echo-stream make-hash-table
     make-list make-load-form-saving-slots make-package make-pathname
     make-random-state make-sequence make-string
     make-string-input-stream make-string-output-stream make-symbol
     make-synonym-stream make-two-way-stream makunbound mapcan mapcar
     mapc mapcon map maphash map-into mapl maplist max member
     member-if member-if-not merge merge-pathnames
     method-combination-error min minusp mismatch mod muffle-warning
     name-char namestring nbutlast nconc nintersection notany notevery
     not nreconc nreverse nset-difference nset-exclusive-or
     nstring-capitalize nstring-downcase nstring-upcase nsublis nsubst
     nsubst-if nsubst-if-not nsubstitute nsubstitute-if
     nsubstitute-if-not nthcdr null numberp numerator nunion oddp open
     open-stream-p output-stream-p package-error-package package-name
     package-nicknames packagep package-shadowing-symbols
     package-used-by-list package-use-list pairlis parse-integer
     parse-namestring pathname-device pathname-directory pathname
     pathname-host pathname-match-p pathname-name pathnamep
     pathname-type pathname-version peek-char phase plusp position
     position-if position-if-not pprint-dispatch pprint-fill pprint
     pprint-indent pprint-linear pprint-newline pprint-tab
     pprint-tabular prin1 prin1-to-string princ princ-to-string print
     print-not-readable-object probe-file proclaim provide random
     random-state-p rassoc rassoc-if rassoc-if-not rational
     rationalize rationalp read-byte read-char read-char-no-hang
     read-delimited-list read-from-string read read-line
     read-preserving-whitespace read-sequence readtablep realpart
     realp reduce rem remhash remove-duplicates remove remove-if
     remove-if-not remprop rename-file rename-package replace require
     restart-name revappend reverse room round rplaca rplacd
     scale-float search set-difference set-dispatch-macro-character
     set-exclusive-or set set-macro-character set-pprint-dispatch
     set-syntax-from-char shadow shadowing-import short-site-name
     signal signum simple-bit-vector-p
     simple-condition-format-arguments simple-condition-format-control
     simple-string-p simple-vector-p sin sinh sleep slot-boundp
     slot-exists-p slot-makunbound slot-value software-type
     software-version some sort special-operator-p sqrt stable-sort
     standard-char-p store-value stream-element-type
     stream-error-stream stream-external-format streamp
     string-capitalize string-downcase string-equal string<= string<
     string= string>= string> string string/= string-greaterp
     string-left-trim string-lessp string-not-equal
     string-not-greaterp string-not-lessp stringp string-right-trim
     string-trim string-upcase sublis subsetp subst subst-if
     subst-if-not substitute substitute-if substitute-if-not subtypep
     sxhash symbol-name symbol-package symbolp synonym-stream-symbol
     tailp tan tanh terpri translate-logical-pathname
     translate-pathname tree-equal truename truncate
     two-way-stream-input-stream two-way-stream-output-stream
     type-error-datum type-error-expected-type type-of typep
     unbound-slot-instance unexport unintern union unread-char
     unuse-package upgraded-array-element-type
     upgraded-complex-part-type upper-case-p use-package
     user-homedir-pathname use-value values-list vector vectorp
     vector-pop vector-push-extend vector-push warn wild-pathname-p
     write-byte write-char write write-line write-sequence
     write-string write-to-string yes-or-no-p y-or-n-p zerop)

    (local-function
     CALL-NEXT-METHOD NEXT-METHOD-P)

    (local-macro
     CALL-METHOD LOOP-FINISH MAKE-METHOD PPRINT-EXIT-IF-LIST-EXHAUSTED
     PPRINT-POP)

    (macro
     and assert case ccase check-type cond ctypecase decf declaim
     defclass defconstant defgeneric define-compiler-macro
     define-condition define-method-combination define-modify-macro
     define-setf-expander define-symbol-macro defmacro defmethod
     defpackage defparameter defsetf defstruct deftype defun defvar
     destructuring-bind do-all-symbols do-external-symbols dolist do
     do* do-symbols dotimes ecase etypecase formatter handler-bind
     handler-case ignore-errors incf in-package lambda loop
     multiple-value-bind multiple-value-list multiple-value-setq
     nth-value or pop pprint-logical-block print-unreadable-object
     prog1 prog2 prog prog* psetf psetq push pushnew remf
     restart-bind restart-case return rotatef setf shiftf step time
     trace typecase unless untrace when with-accessors
     with-compilation-unit with-condition-restarts
     with-hash-table-iterator with-input-from-string with-open-stream
     with-output-to-string with-package-iterator with-simple-restart
     with-slots with-standard-io-syntax)

    (restart
     ABORT CONTINUE MUFFLE-WARNING STORE-VALUE USE-VALUE)

    (special-operator
     block catch eval-when flet function go if labels let let*
     load-time-value locally macrolet multiple-value-call
     multiple-value-prog1 progn progv quote return-from setq
     symbol-macrolet tagbody the throw unwind-protect)

    (standard-generic-fucntion
     add-method allocate-instance change-class class-name
     compute-applicable-methods describe-object documentation
     find-method function-keywords initialize-instance
     make-instances-obsolete make-instance make-load-form
     method-qualifiers no-applicable-method no-next-method print-object
     reinitialize-instance remove-method shared-initialize slot-missing
     slot-unbound update-instance-for-different-class
     update-instance-for-redefined-class)))



(declare-declaration DECLARATION    (&rest names))
(declare-declaration DYNAMIC-EXTENT (&rest var-or-functions))
(declare-declaration FTYPE          (type &rest function-names))
(declare-declaration IGNORABLE      (&rest var-or-functions))
(declare-declaration IGNORE         (&rest var-or-functions))
(declare-declaration INLINE         (&rest function-names))
(declare-declaration NOTINLINE      (&rest function-names))
(declare-declaration OPTIMIZE       (&rest qualities))
(declare-declaration SPECIAL        (&rest vars))
(declare-declaration TYPE           (type &rest vars))



(declare-compound-type-specifier AND  (&rest typespecs)
                                 :combining)

(declare-compound-type-specifier OR   (&rest typespecs)
                                 :combining)

(declare-compound-type-specifier NOT (typespec)
                                 :combining)

(declare-compound-type-specifier EQL  (object)
                                 :combining)

(declare-compound-type-specifier MEMBER (&rest objects)
                                 :combining)

(declare-compound-type-specifier array  (element-type &optional dimension-spec)
                                 :specializing)

(declare-compound-type-specifier simple-array  (element-type &optional dimension-spec)
                                 :specializing)

(declare-compound-type-specifier vector (element-type &optional size)
                                 :specializing)

(declare-compound-type-specifier simple-vector (&optional size)
                                 :specializing)

(declare-compound-type-specifier bit-vector (&optional size)
                                 :abbreviating)

(declare-compound-type-specifier simple-bit-vector (&optional size)
                                 :abbreviating)

(declare-compound-type-specifier string (&optional size)
                                 :abbreviating)

(declare-compound-type-specifier base-string (&optional size)
                                 :abbreviating)

(declare-compound-type-specifier simple-string (&optional size)
                                 :abbreviating)

(declare-compound-type-specifier simple-base-string (&optional size)
                                 :abbreviating)

(declare-compound-type-specifier real (&optional lower-limit upper-limit)
                                 :abbreviating)

(declare-compound-type-specifier complex (&optional typespec)
                                 :specializing)

(declare-compound-type-specifier float (&optional lower-limit upper-limit)
                                 :abbreviating)

(declare-compound-type-specifier short-float (&optional lower-limit upper-limit)
                                 :abbreviating)

(declare-compound-type-specifier single-float (&optional lower-limit upper-limit)
                                 :abbreviating)

(declare-compound-type-specifier double-float (&optional lower-limit upper-limit)
                                 :abbreviating)

(declare-compound-type-specifier long-float (&optional lower-limit upper-limit)
                                 :abbreviating)

(declare-compound-type-specifier rational (&optional lower-limit upper-limit)
                                 :abbreviating)

(declare-compound-type-specifier integer (&optional lower-limit upper-limit)
                                 :abbreviating)

(declare-compound-type-specifier signed-byte (&optional size)
                                 :abbreviating)

(declare-compound-type-specifier unsigned-byte (&optional size)
                                 :abbreviating)

(declare-compound-type-specifier mod (n)
                                 :abbreviating)

(declare-compound-type-specifier function (&optional arg-typespec value-typespec)
                                 :specializing)

(declare-compound-type-specifier cons (&optional car-typespec cdr-typespec)
                                 :specializing)

(declare-compound-type-specifier values (&rest value-typespecs)
                                 :specializing)

(declare-compound-type-specifier satisfies (predicate-name)
                                 :predicating)



(declare-type ATOM                  (t))
(declare-type BOOLEAN               (symbol)
              :definition (member t nil))
(declare-type KEYWORD               (symbol)
              :documentation "
Note: we cannot say :definition (satisfies keywordp)
      because keywordp is whether (symbol-package k) = (find-package :keyword)
      while KEYWORD includes all symbols interned in the :keyword package.
")

(declare-type EXTENDED-CHAR         (character))
(declare-type BASE-CHAR             (character))
(declare-type STANDARD-CHAR         (base-char))

(declare-type BASE-STRING           (string))
(declare-type SIMPLE-STRING         (string))
(declare-type SIMPLE-BASE-STRING    (base-string simple-string))

(declare-type FIXNUM                (integer))
(declare-type BIGNUM                (integer)
              :definition (and integer (not fixnum)))
(declare-type SIGNED-BYTE           (integer))
(declare-type UNSIGNED-BYTE         (signed-byte))
(declare-type BIT                   (unsigned-byte)
              :definition (unsigned-byte 0 1))

(declare-type SHORT-FLOAT           (float))
(declare-type SINGLE-FLOAT          (float))
(declare-type DOUBLE-FLOAT          (float))
(declare-type LONG-FLOAT            (float))

(declare-type COMPILED-FUNCTION     (function))

(declare-type SIMPLE-ARRAY          (array))
(declare-type SIMPLE-VECTOR         (vector))
(declare-type SIMPLE-BIT-VECTOR     (bit-vector simple-vector))

(declare-type NIL                  :all-types)



(declare-class STANDARD-OBJECT      (t))
(declare-class STRUCTURE-OBJECT     (t))

(declare-system-class T                          ())
(declare-system-class ARRAY                      (t))
(declare-system-class METHOD                     (t))
(declare-system-class NUMBER                     (t))
(declare-system-class STREAM                     (t))
(declare-system-class SYMBOL                     (t))
(declare-system-class PACKAGE                    (t))
(declare-system-class RESTART                    (t))
(declare-system-class FUNCTION                   (t))
(declare-system-class PATHNAME                   (t))
(declare-system-class SEQUENCE                   (t))
(declare-system-class CHARACTER                  (t))
(declare-system-class READTABLE                  (t))
(declare-system-class HASH-TABLE                 (t))
(declare-system-class RANDOM-STATE               (t))
(declare-system-class METHOD-COMBINATION         (t))
(declare-system-class VECTOR                     (array sequence))
(declare-system-class STRING                     (vector))
(declare-system-class BIT-VECTOR                 (vector))
(declare-system-class LIST                       (sequence))
(declare-system-class CONS                       (list))
(declare-system-class BUILT-IN-CLASS             (class))
(declare-system-class STANDARD-CLASS             (class))
(declare-system-class STRUCTURE-CLASS            (class))
(declare-system-class GENERIC-FUNCTION           (function))
(declare-system-class STANDARD-GENERIC-FUNCTION  (generic-function))
(declare-system-class STANDARD-METHOD            (method))
(declare-system-class REAL                       (number))
(declare-system-class COMPLEX                    (number))
(declare-system-class FLOAT                      (real))
(declare-system-class RATIONAL                   (real))
(declare-system-class RATIO                      (rational))
(declare-system-class INTEGER                    (rational))
(declare-system-class LOGICAL-PATHNAME           (pathname))
(declare-system-class CLASS                      (standard-object))
(declare-system-class ECHO-STREAM                (stream))
(declare-system-class FILE-STREAM                (stream))
(declare-system-class STRING-STREAM              (stream))
(declare-system-class SYNONYM-STREAM             (stream))
(declare-system-class TWO-WAY-STREAM             (stream))
(declare-system-class BROADCAST-STREAM           (stream))
(declare-system-class CONCATENATED-STREAM        (stream))
(declare-system-class NULL                       (symbol))


(declare-condition-type CONDITION          (t))
(declare-condition-type SERIOUS-CONDITION  (condition))
(declare-condition-type SIMPLE-CONDITION   (condition))
(declare-condition-type WARNING            (condition))
(declare-condition-type STORAGE-CONDITION  (serious-condition))
(declare-condition-type ERROR              (serious-condition))
(declare-condition-type ARITHMETIC-ERROR   (error))
(declare-condition-type CELL-ERROR         (error))
(declare-condition-type CONTROL-ERROR      (error))
(declare-condition-type FILE-ERROR         (error))
(declare-condition-type PACKAGE-ERROR      (error))
(declare-condition-type PARSE-ERROR        (error))
(declare-condition-type PRINT-NOT-READABLE (error))
(declare-condition-type PROGRAM-ERROR      (error))
(declare-condition-type STREAM-ERROR       (error))
(declare-condition-type TYPE-ERROR         (error))
(declare-condition-type DIVISION-BY-ZERO                 (arithmetic-error))
(declare-condition-type FLOATING-POINT-INEXACT           (arithmetic-error))
(declare-condition-type FLOATING-POINT-INVALID-OPERATION (arithmetic-error))
(declare-condition-type FLOATING-POINT-OVERFLOW          (arithmetic-error))
(declare-condition-type FLOATING-POINT-UNDERFLOW         (arithmetic-error))
(declare-condition-type UNBOUND-SLOT       (cell-error))
(declare-condition-type UNBOUND-VARIABLE   (cell-error))
(declare-condition-type UNDEFINED-FUNCTION (cell-error))
(declare-condition-type READER-ERROR       (parse-error))
(declare-condition-type SIMPLE-ERROR       (simple-condition error))
(declare-condition-type SIMPLE-TYPE-ERROR  (simple-condition type-error))
(declare-condition-type SIMPLE-WARNING     (simple-condition warning))
(declare-condition-type END-OF-FILE        (stream-error))
(declare-condition-type STYLE-WARNING      (warning))


(declare-symbol DECLARE)
(declare-symbol LAMBDA)

(declare-lambda-list-keyword &OPTIONAL)
(declare-lambda-list-keyword &REST)
(declare-lambda-list-keyword &AUX)
(declare-lambda-list-keyword &KEY)
(declare-lambda-list-keyword &ALLOW-OTHER-KEYS)
(declare-lambda-list-keyword &BODY)
(declare-lambda-list-keyword &ENVIRONMENT)
(declare-lambda-list-keyword &WHOLE)



(declare-variable *BREAK-ON-SIGNALS*)
(declare-variable *COMPILE-FILE-PATHNAME*)
(declare-variable *COMPILE-FILE-TRUENAME*)
(declare-variable *COMPILE-PRINT*)
(declare-variable *COMPILE-VERBOSE*)
(declare-variable *DEBUGGER-HOOK*)
(declare-variable *DEBUG-IO*)
(declare-variable *DEFAULT-PATHNAME-DEFAULTS*)
(declare-variable *ERROR-OUTPUT*)
(declare-variable *FEATURES*)
(declare-variable *GENSYM-COUNTER*)
(declare-variable *LOAD-PATHNAME*)
(declare-variable *LOAD-PRINT*)
(declare-variable *LOAD-TRUENAME*)
(declare-variable *LOAD-VERBOSE*)
(declare-variable *MACROEXPAND-HOOK*)
(declare-variable *MODULES*)
(declare-variable *PACKAGE*)
(declare-variable *PRINT-ARRAY*)
(declare-variable *PRINT-BASE*)
(declare-variable *PRINT-CASE*)
(declare-variable *PRINT-CIRCLE*)
(declare-variable *PRINT-ESCAPE*)
(declare-variable *PRINT-GENSYM*)
(declare-variable *PRINT-LENGTH*)
(declare-variable *PRINT-LEVEL*)
(declare-variable *PRINT-LINES*)
(declare-variable *PRINT-MISER-WIDTH*)
(declare-variable *PRINT-PPRINT-DISPATCH*)
(declare-variable *PRINT-PRETTY*)
(declare-variable *PRINT-RADIX*)
(declare-variable *PRINT-READABLY*)
(declare-variable *PRINT-RIGHT-MARGIN*)
(declare-variable *QUERY-IO*)
(declare-variable *RANDOM-STATE*)
(declare-variable *READ-BASE*)
(declare-variable *READ-DEFAULT-FLOAT-FORMAT*)
(declare-variable *READ-EVAL*)
(declare-variable *READ-SUPPRESS*)
(declare-variable *READTABLE*)
(declare-variable *STANDARD-INPUT*)
(declare-variable *STANDARD-OUTPUT*)
(declare-variable *TERMINAL-IO*)
(declare-variable *TRACE-OUTPUT*)
(declare-variable -)
(declare-variable /)
(declare-variable //)
(declare-variable ///)
(declare-variable *)
(declare-variable **)
(declare-variable ***)
(declare-variable +)
(declare-variable ++)
(declare-variable +++)


(declare-constant-variable NIL)
(declare-constant-variable T)
(declare-constant-variable PI)
(declare-constant-variable ARRAY-DIMENSION-LIMIT)
(declare-constant-variable ARRAY-RANK-LIMIT)
(declare-constant-variable ARRAY-TOTAL-SIZE-LIMIT)
(declare-constant-variable BOOLE-1)
(declare-constant-variable BOOLE-2)
(declare-constant-variable BOOLE-ANDC1)
(declare-constant-variable BOOLE-ANDC2)
(declare-constant-variable BOOLE-AND)
(declare-constant-variable BOOLE-C1)
(declare-constant-variable BOOLE-C2)
(declare-constant-variable BOOLE-CLR)
(declare-constant-variable BOOLE-EQV)
(declare-constant-variable BOOLE-IOR)
(declare-constant-variable BOOLE-NAND)
(declare-constant-variable BOOLE-NOR)
(declare-constant-variable BOOLE-ORC1)
(declare-constant-variable BOOLE-ORC2)
(declare-constant-variable BOOLE-SET)
(declare-constant-variable BOOLE-XOR)
(declare-constant-variable CALL-ARGUMENTS-LIMIT)
(declare-constant-variable CHAR-CODE-LIMIT)
(declare-constant-variable DOUBLE-FLOAT-EPSILON)
(declare-constant-variable DOUBLE-FLOAT-NEGATIVE-EPSILON)
(declare-constant-variable INTERNAL-TIME-UNITS-PER-SECOND)
(declare-constant-variable LAMBDA-LIST-KEYWORDS)
(declare-constant-variable LAMBDA-PARAMETERS-LIMIT)
(declare-constant-variable LEAST-NEGATIVE-DOUBLE-FLOAT)
(declare-constant-variable LEAST-NEGATIVE-LONG-FLOAT)
(declare-constant-variable LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT)
(declare-constant-variable LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT)
(declare-constant-variable LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT)
(declare-constant-variable LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT)
(declare-constant-variable LEAST-NEGATIVE-SHORT-FLOAT)
(declare-constant-variable LEAST-NEGATIVE-SINGLE-FLOAT)
(declare-constant-variable LEAST-POSITIVE-DOUBLE-FLOAT)
(declare-constant-variable LEAST-POSITIVE-LONG-FLOAT)
(declare-constant-variable LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT)
(declare-constant-variable LEAST-POSITIVE-NORMALIZED-LONG-FLOAT)
(declare-constant-variable LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT)
(declare-constant-variable LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT)
(declare-constant-variable LEAST-POSITIVE-SHORT-FLOAT)
(declare-constant-variable LEAST-POSITIVE-SINGLE-FLOAT)
(declare-constant-variable LONG-FLOAT-EPSILON)
(declare-constant-variable LONG-FLOAT-NEGATIVE-EPSILON)
(declare-constant-variable MOST-NEGATIVE-DOUBLE-FLOAT)
(declare-constant-variable MOST-NEGATIVE-FIXNUM)
(declare-constant-variable MOST-NEGATIVE-LONG-FLOAT)
(declare-constant-variable MOST-NEGATIVE-SHORT-FLOAT)
(declare-constant-variable MOST-NEGATIVE-SINGLE-FLOAT)
(declare-constant-variable MOST-POSITIVE-DOUBLE-FLOAT)
(declare-constant-variable MOST-POSITIVE-FIXNUM)
(declare-constant-variable MOST-POSITIVE-LONG-FLOAT)
(declare-constant-variable MOST-POSITIVE-SHORT-FLOAT)
(declare-constant-variable MOST-POSITIVE-SINGLE-FLOAT)
(declare-constant-variable MULTIPLE-VALUES-LIMIT)
(declare-constant-variable SHORT-FLOAT-EPSILON)
(declare-constant-variable SHORT-FLOAT-NEGATIVE-EPSILON)
(declare-constant-variable SINGLE-FLOAT-EPSILON)
(declare-constant-variable SINGLE-FLOAT-NEGATIVE-EPSILON)


(declare-accessor CAAAAR (list))
(declare-accessor CAAADR (list))
(declare-accessor CAAAR  (list))
(declare-accessor CAADAR (list))
(declare-accessor CAADDR (list))
(declare-accessor CAADR  (list))
(declare-accessor CAAR   (list))
(declare-accessor CADAAR (list))
(declare-accessor CADADR (list))
(declare-accessor CADAR  (list))
(declare-accessor CADDAR (list))
(declare-accessor CADDDR (list))
(declare-accessor CADDR  (list))
(declare-accessor CADR   (list))
(declare-accessor CAR    (list))
(declare-accessor CDAAAR (list))
(declare-accessor CDAADR (list))
(declare-accessor CDAAR  (list))
(declare-accessor CDADAR (list))
(declare-accessor CDADDR (list))
(declare-accessor CDADR  (list))
(declare-accessor CDAR   (list))
(declare-accessor CDDAAR (list))
(declare-accessor CDDADR (list))
(declare-accessor CDDAR  (list))
(declare-accessor CDDDAR (list))
(declare-accessor CDDDDR (list))
(declare-accessor CDDDR  (list))
(declare-accessor CDDR   (list))
(declare-accessor CDR    (list))

(declare-accessor FIRST   (list))
(declare-accessor SECOND  (list))
(declare-accessor THIRD   (list))
(declare-accessor FOURTH  (list))
(declare-accessor FIFTH   (list))
(declare-accessor SIXTH   (list))
(declare-accessor SEVENTH (list))
(declare-accessor EIGHTH  (list))
(declare-accessor NINTH   (list))
(declare-accessor TENTH   (list))
(declare-accessor REST    (list))

(declare-accessor NTH     (index list))

(declare-accessor ELT            (sequence index))
(declare-accessor SUBSEQ         (sequence start &optional end))

(declare-accessor AREF           (array &rest subscripts))
(declare-accessor ROW-MAJOR-AREF (array index))
(declare-accessor BIT            (bit-array &rest subscripts))
(declare-accessor SBIT           (bit-array &rest subscripts))
(declare-accessor SVREF          (simple-vector index))
(declare-accessor CHAR           (string index))
(declare-accessor SCHAR          (string index))
(declare-accessor FILL-POINTER   (vector))


(declare-accessor GET     (symbol indicator &optional default))

(declare-accessor GETF    (plist indicator &optional default))

(declare-accessor GETHASH (key hash-table &optional default)
                  :result-type (values t t))


(declare-accessor LDB (ldb bytespec integer)
                  :result-type byte)

(declare-accessor MASK-FIELD (bytespec integer))


(declare-accessor COMPILER-MACRO-FUNCTION (function-name &optional environment)
                  :result-type (or function null))

(declare-accessor MACRO-FUNCTION (symbol &optional environment)
                  :result-type (or function null))

(declare-accessor FDEFINITION (function-name)
                  :result-type function
                  :signals (type-error))

(declare-accessor FIND-CLASS (symbol &optional errorp environment)
                  :result-type (or class null))

(declare-accessor LOGICAL-PATHNAME-TRANSLATIONS (host)
                  :result-type list)

(declare-accessor READTABLE-CASE (readtable))

(declare-accessor SYMBOL-FUNCTION (symbol))
(declare-accessor SYMBOL-PLIST    (symbol))
(declare-accessor SYMBOL-VALUE    (symbol))

(declare-accessor VALUES (&rest object))

;;; functions --  3. Evaluation and Compilation

(declare-function COMPILE (name/function-name-or-nil &optional definition/lambda-expression-or-function)
                  :result-type (values (or function-name compiled-function) t t))
(declare-function EVAL ())
(declare-function MACROEXPAND-1 ())
(declare-function MACROEXPAND ())
(declare-function PROCLAIM ())
(declare-function SPECIAL-OPERATOR-P ())
(declare-function CONSTANTP ())

;;; functions --  4. Types and Classes
;;; functions --  5. Data and Control Flow
;;; functions --  6. Iteration
;;; functions --  7. Objects
;;; functions --  8. Structures
;;; functions --  9. Conditions
;;; functions -- 10. Symbols
;;; functions -- 11. Packages
;;; functions -- 12. Numbers
;;; functions -- 13. Characters
;;; functions -- 14. Conses
;;; functions -- 15. Arrays
;;; functions -- 16. Strings
;;; functions -- 17. Sequences
;;; functions -- 18. Hash Tables
;;; functions -- 19. Filenames
;;; functions -- 20. Files
;;; functions -- 21. Streams
;;; functions -- 22. Printer
;;; functions -- 23. Reader
;;; functions -- 24. System Construction
;;; functions -- 25. Environment

(declare-function 1- (argument/number) :result-type number :signals (type-error arithmetic-error)
                  :documentation "1- returns a number that is one less than its argument number.")

(declare-function 1+ (argument/number) :result-type number :signals (type-error arithmetic-error)
                  :documentation "1+ returns a number that is one more than its argument number.")


(declare-function ABORT (&optional condition/condition) :result-type (values) :signals (control-error)
                  :documentation "
Transfers control to the most recently established applicable restart
named ABORT.  If no such restart exists, signals an error of type
CONTROL-ERROR.

When CONDITION is non-nil, only those restarts are considered that are
either explicitly associated with that condition, or not associated
with any condition; that is, the excluded restarts are those that are
associated with a non-empty set of conditions of which the given
condition is not an element. If CONDITION is nil, all restarts are
considered.
")

(declare-function MUFFLE-WARNING (&optional condition/condition) :result-type (values)
                  :documentation "
Transfers control to the most recently established applicable restart
named MUFFLE-WARNING.  If no such restart exists, signals an error of
type CONTROL-ERROR.

When CONDITION is non-nil, only those restarts are considered that are
either explicitly associated with that condition, or not associated
with any condition; that is, the excluded restarts are those that are
associated with a non-empty set of conditions of which the given
condition is not an element. If CONDITION is nil, all restarts are
considered.
")

(declare-function CONTINUE (&optional condition/condition) :result-type null
                  :documentation "
Transfers control to the most recently established applicable restart
named CONTINUE.  If no such restart exists, returns NIL.

When CONDITION is non-nil, only those restarts are considered that are
either explicitly associated with that condition, or not associated
with any condition; that is, the excluded restarts are those that are
associated with a non-empty set of conditions of which the given
condition is not an element. If CONDITION is nil, all restarts are
considered.
")

(declare-function STORE-VALUE (value &optional condition/condition) :result-type null
                  :documentation "
Transfers control to the most recently established applicable restart
named STORE-VALUE.  If no such restart exists, returns NIL.

When CONDITION is non-nil, only those restarts are considered that are
either explicitly associated with that condition, or not associated
with any condition; that is, the excluded restarts are those that are
associated with a non-empty set of conditions of which the given
condition is not an element. If CONDITION is nil, all restarts are
considered.
")

(declare-function USE-VALUE (value  &optional condition/condition) :result-type null
                  :documentation "
Transfers control to the most recently established applicable restart
named USE-VALUE.  If no such restart exists, returns NIL.

When CONDITION is non-nil, only those restarts are considered that are
either explicitly associated with that condition, or not associated
with any condition; that is, the excluded restarts are those that are
associated with a non-empty set of conditions of which the given
condition is not an element. If CONDITION is nil, all restarts are
considered.
")


(declare-function ABS (number) :result-type real :signals (type-error))
(declare-function ACONS (key datum alist) :result-type list)
(declare-function ACOS (number) :result-type real)
(declare-function ACOSH (number) :result-type number)
(declare-function ADJOIN (item list &key (key (function identity)) (test (function eql)) test-not) :result-type list)
(declare-function ADJUSTABLE-ARRAY-P (array) :result-type generalized-boolean)
(declare-function ADJUST-ARRAY (array new-dimensions &key element-type initial-element initial-contents fill-pointer displaced-to displaced-index-offset) :result-type array)
(declare-function ALPHA-CHAR-P (character) :result-type generalized-boolean :signals (type-error))
(declare-function ALPHANUMERICP (character) :result-type generalized-boolean :signals (type-error))
(declare-function APPEND (&rest lists) :result-type lists :signals ())
(declare-function APPLY (function &rest args+))
(declare-function APROPOS (string &optional package) :result-type (values))
(declare-function APROPOS-LIST (string &optional package) :result-type list)
(declare-function ARITHMETIC-ERROR-OPERANDS ())
(declare-function ARITHMETIC-ERROR-OPERATION ())
(declare-function ARRAY-DIMENSION (array integer) :result-type (integer 0))
(declare-function ARRAY-DIMENSIONS (array) :result-type (list (integer 0)))
(declare-function ARRAY-DISPLACEMENT ())
(declare-function ARRAY-ELEMENT-TYPE ())
(declare-function ARRAY-HAS-FILL-POINTER-P ())
(declare-function ARRAY-IN-BOUNDS-P ())
(declare-function ARRAYP ())
(declare-function ARRAY-RANK ())
(declare-function ARRAY-ROW-MAJOR-INDEX ())
(declare-function ARRAY-TOTAL-SIZE ())
(declare-function ASH ())
(declare-function ASIN (number) :result-type real)
(declare-function ASINH (number) :result-type number)
(declare-function ASSOC ())
(declare-function ASSOC-IF ())
(declare-function ASSOC-IF-NOT ())
(declare-function ATAN (number1 &optional number2) :result-type real)
(declare-function ATANH (number) :result-type number)
(declare-function ATOM ())
(declare-function BIT-ANDC1 ())
(declare-function BIT-ANDC2 ())
(declare-function BIT-AND ())
(declare-function BIT-EQV ())
(declare-function BIT-IOR ())
(declare-function BIT-NAND ())
(declare-function BIT-NOR ())
(declare-function BIT-NOT ())
(declare-function BIT-ORC1 ())
(declare-function BIT-ORC2 ())
(declare-function BIT-VECTOR-P ())
(declare-function BIT-XOR ())
(declare-function BOOLE ())
(declare-function BOTH-CASE-P ())
(declare-function BOUNDP ())
(declare-function BREAK ())
(declare-function BROADCAST-STREAM-STREAMS ())
(declare-function BUTLAST ())
(declare-function BYTE ())
(declare-function BYTE-POSITION ())
(declare-function BYTE-SIZE ())
(declare-function CEILING ())
(declare-function CELL-ERROR-NAME ())
(declare-function CERROR ())
(declare-function CHARACTER ())
(declare-function CHARACTERP ())
(declare-function CHAR-CODE ())
(declare-function CHAR-DOWNCASE ())
(declare-function CHAR-EQUAL ())
(declare-function CHAR<= ())
(declare-function CHAR< ())
(declare-function CHAR= ())
(declare-function CHAR>= ())
(declare-function CHAR> ())
(declare-function CHAR/= ())
(declare-function CHAR-GREATERP ())
(declare-function CHAR-INT ())
(declare-function CHAR-LESSP ())
(declare-function CHAR-NAME ())
(declare-function CHAR-NOT-EQUAL ())
(declare-function CHAR-NOT-GREATERP ())
(declare-function CHAR-NOT-LESSP ())
(declare-function CHAR-UPCASE ())
(declare-function CIS ())
(declare-function CLASS-OF ())
(declare-function CLEAR-INPUT ())
(declare-function CLEAR-OUTPUT ())
(declare-function CLOSE ())
(declare-function CLRHASH ())
(declare-function CODE-CHAR ())
(declare-function COERCE ())
(declare-function COMPILED-FUNCTION-P ())
(declare-function COMPILE-FILE ())
(declare-function COMPILE-FILE-PATHNAME ())
(declare-function COMPLEMENT ())
(declare-function COMPLEX ())
(declare-function COMPLEXP ())
(declare-function COMPUTE-RESTARTS ())
(declare-function CONCATENATED-STREAM-STREAMS ())
(declare-function CONCATENATE ())
(declare-function CONJUGATE ())
(declare-function CONS ())
(declare-function CONSP ())
(declare-function CONSTANTLY ())
(declare-function COPY-ALIST ())
(declare-function COPY-LIST ())
(declare-function COPY-PPRINT-DISPATCH ())
(declare-function COPY-READTABLE ())
(declare-function COPY-SEQ ())
(declare-function COPY-STRUCTURE ())
(declare-function COPY-SYMBOL ())
(declare-function COPY-TREE ())
(declare-function COS (number) :result-type number)
(declare-function COSH (number) :result-type number)
(declare-function COUNT ())
(declare-function COUNT-IF ())
(declare-function COUNT-IF-NOT ())
(declare-function DECODE-FLOAT ())
(declare-function DECODE-UNIVERSAL-TIME ())
(declare-function DELETE-DUPLICATES ())
(declare-function DELETE-FILE ())
(declare-function DELETE ())
(declare-function DELETE-IF ())
(declare-function DELETE-IF-NOT ())
(declare-function DELETE-PACKAGE ())
(declare-function DENOMINATOR ())
(declare-function DEPOSIT-FIELD ())
(declare-function DESCRIBE ())
(declare-function DIGIT-CHAR ())
(declare-function DIGIT-CHAR-P ())
(declare-function DIRECTORY ())
(declare-function DIRECTORY-NAMESTRING ())
(declare-function DISASSEMBLE ())
(declare-function DPB ())
(declare-function DRIBBLE ())
(declare-function ECHO-STREAM-INPUT-STREAM ())
(declare-function ECHO-STREAM-OUTPUT-STREAM ())
(declare-function ED ())
(declare-function ENDP ())
(declare-function ENOUGH-NAMESTRING ())
(declare-function ENSURE-DIRECTORIES-EXIST ())
(declare-function ENSURE-GENERIC-FUNCTION ())
(declare-function EQ ())
(declare-function EQL ())
(declare-function EQUAL ())
(declare-function EQUALP ())
(declare-function ERROR ())
(declare-function EVENP ())
(declare-function EVERY ())
(declare-function EXP ())
(declare-function EXPORT ())
(declare-function EXPT ())
(declare-function FBOUNDP ())
(declare-function FCEILING ())
(declare-function FFLOOR ())
(declare-function FILE-AUTHOR ())
(declare-function FILE-ERROR-PATHNAME ())
(declare-function FILE-LENGTH ())
(declare-function FILE-NAMESTRING ())
(declare-function FILE-POSITION ())
(declare-function FILE-STRING-LENGTH ())
(declare-function FILE-WRITE-DATE ())
(declare-function FILL ())
(declare-function FIND-ALL-SYMBOLS ())
(declare-function FIND ())
(declare-function FIND-IF ())
(declare-function FIND-IF-NOT ())
(declare-function FIND-PACKAGE ())
(declare-function FIND-RESTART ())
(declare-function FIND-SYMBOL ())
(declare-function FINISH-OUTPUT ())
(declare-function FLOAT-DIGITS ())
(declare-function FLOAT ())
(declare-function FLOATP ())
(declare-function FLOAT-PRECISION ())
(declare-function FLOAT-RADIX ())
(declare-function FLOAT-SIGN ())
(declare-function FLOOR ())
(declare-function FMAKUNBOUND ())
(declare-function FORCE-OUTPUT ())
(declare-function FORMAT ())
(declare-function FRESH-LINE ())
(declare-function FROUND ())
(declare-function FTRUNCATE ())
(declare-function FUNCALL ())
(declare-function <= ())
(declare-function < ())
(declare-function = ())
(declare-function >= ())
(declare-function > ())
(declare-function - ())
(declare-function /= ())
(declare-function / ())
(declare-function * ())
(declare-function + ())
(declare-function FUNCTION-LAMBDA-EXPRESSION ())
(declare-function FUNCTIONP ())
(declare-function GCD ())
(declare-function GENSYM ())
(declare-function GENTEMP ())
(declare-function GET-DECODED-TIME ())
(declare-function GET-DISPATCH-MACRO-CHARACTER ())
(declare-function GET-INTERNAL-REAL-TIME ())
(declare-function GET-INTERNAL-RUN-TIME ())
(declare-function GET-MACRO-CHARACTER ())
(declare-function GET-OUTPUT-STREAM-STRING ())
(declare-function GET-PROPERTIES ())
(declare-function GET-SETF-EXPANSION ())
(declare-function GET-UNIVERSAL-TIME ())
(declare-function GRAPHIC-CHAR-P ())
(declare-function HASH-TABLE-COUNT ())
(declare-function HASH-TABLE-P ())
(declare-function HASH-TABLE-REHASH-SIZE ())
(declare-function HASH-TABLE-REHASH-THRESHOLD ())
(declare-function HASH-TABLE-SIZE ())
(declare-function HASH-TABLE-TEST ())
(declare-function HOST-NAMESTRING ())
(declare-function IDENTITY ())
(declare-function IMAGPART ())
(declare-function IMPORT ())
(declare-function INPUT-STREAM-P ())
(declare-function INSPECT ())
(declare-function INTEGER-DECODE-FLOAT ())
(declare-function INTEGER-LENGTH ())
(declare-function INTEGERP ())
(declare-function INTERACTIVE-STREAM-P ())
(declare-function INTERN ())
(declare-function INTERSECTION ())
(declare-function INVALID-METHOD-ERROR ())
(declare-function INVOKE-DEBUGGER ())
(declare-function INVOKE-RESTART ())
(declare-function INVOKE-RESTART-INTERACTIVELY ())
(declare-function ISQRT ())
(declare-function KEYWORDP ())
(declare-function LAST ())
(declare-function LCM ())
(declare-function LDB-TEST ())
(declare-function LDIFF ())
(declare-function LENGTH ())
(declare-function LISP-IMPLEMENTATION-TYPE ())
(declare-function LISP-IMPLEMENTATION-VERSION ())
(declare-function LIST-ALL-PACKAGES ())
(declare-function LISTEN ())
(declare-function LIST ())
(declare-function LIST* ())
(declare-function LIST-LENGTH ())
(declare-function LISTP ())
(declare-function LOAD ())
(declare-function LOAD-LOGICAL-PATHNAME-TRANSLATIONS ())
(declare-function LOGANDC1 ())
(declare-function LOGANDC2 ())
(declare-function LOGAND ())
(declare-function LOGBITP ())
(declare-function LOGCOUNT ())
(declare-function LOGEQV ())
(declare-function LOG ())
(declare-function LOGICAL-PATHNAME ())
(declare-function LOGIOR ())
(declare-function LOGNAND ())
(declare-function LOGNOR ())
(declare-function LOGNOT ())
(declare-function LOGORC1 ())
(declare-function LOGORC2 ())
(declare-function LOGTEST ())
(declare-function LOGXOR ())
(declare-function LONG-SITE-NAME ())
(declare-function LOWER-CASE-P ())
(declare-function MACHINE-INSTANCE ())
(declare-function MACHINE-TYPE ())
(declare-function MACHINE-VERSION ())
(declare-function MAKE-ARRAY ())
(declare-function MAKE-BROADCAST-STREAM ())
(declare-function MAKE-CONCATENATED-STREAM ())
(declare-function MAKE-CONDITION ())
(declare-function MAKE-DISPATCH-MACRO-CHARACTER ())
(declare-function MAKE-ECHO-STREAM ())
(declare-function MAKE-HASH-TABLE ())
(declare-function MAKE-LIST ())
(declare-function MAKE-LOAD-FORM-SAVING-SLOTS ())
(declare-function MAKE-PACKAGE ())
(declare-function MAKE-PATHNAME ())
(declare-function MAKE-RANDOM-STATE ())
(declare-function MAKE-SEQUENCE ())
(declare-function MAKE-STRING ())
(declare-function MAKE-STRING-INPUT-STREAM ())
(declare-function MAKE-STRING-OUTPUT-STREAM ())
(declare-function MAKE-SYMBOL ())
(declare-function MAKE-SYNONYM-STREAM ())
(declare-function MAKE-TWO-WAY-STREAM ())
(declare-function MAKUNBOUND ())
(declare-function MAPCAN ())
(declare-function MAPCAR ())
(declare-function MAPC ())
(declare-function MAPCON ())
(declare-function MAP ())
(declare-function MAPHASH ())
(declare-function MAP-INTO ())
(declare-function MAPL ())
(declare-function MAPLIST ())
(declare-function MAX ())
(declare-function MEMBER ())
(declare-function MEMBER-IF ())
(declare-function MEMBER-IF-NOT ())
(declare-function MERGE ())
(declare-function MERGE-PATHNAMES ())
(declare-function METHOD-COMBINATION-ERROR ())
(declare-function MIN ())
(declare-function MINUSP ())
(declare-function MISMATCH ())
(declare-function MOD ())
(declare-function NAME-CHAR ())
(declare-function NAMESTRING ())
(declare-function NBUTLAST ())
(declare-function NCONC ())
(declare-function NINTERSECTION ())
(declare-function NOTANY ())
(declare-function NOTEVERY ())
(declare-function NOT ())
(declare-function NRECONC ())
(declare-function NREVERSE ())
(declare-function NSET-DIFFERENCE ())
(declare-function NSET-EXCLUSIVE-OR ())
(declare-function NSTRING-CAPITALIZE ())
(declare-function NSTRING-DOWNCASE ())
(declare-function NSTRING-UPCASE ())
(declare-function NSUBLIS ())
(declare-function NSUBST ())
(declare-function NSUBST-IF ())
(declare-function NSUBST-IF-NOT ())
(declare-function NSUBSTITUTE ())
(declare-function NSUBSTITUTE-IF ())
(declare-function NSUBSTITUTE-IF-NOT ())
(declare-function NTHCDR ())
(declare-function NULL ())
(declare-function NUMBERP ())
(declare-function NUMERATOR ())
(declare-function NUNION ())
(declare-function ODDP ())
(declare-function OPEN ())
(declare-function OPEN-STREAM-P ())
(declare-function OUTPUT-STREAM-P ())
(declare-function PACKAGE-ERROR-PACKAGE ())
(declare-function PACKAGE-NAME ())
(declare-function PACKAGE-NICKNAMES ())
(declare-function PACKAGEP ())
(declare-function PACKAGE-SHADOWING-SYMBOLS ())
(declare-function PACKAGE-USED-BY-LIST ())
(declare-function PACKAGE-USE-LIST ())
(declare-function PAIRLIS ())
(declare-function PARSE-INTEGER ())
(declare-function PARSE-NAMESTRING ())
(declare-function PATHNAME-DEVICE ())
(declare-function PATHNAME-DIRECTORY ())
(declare-function PATHNAME ())
(declare-function PATHNAME-HOST ())
(declare-function PATHNAME-MATCH-P ())
(declare-function PATHNAME-NAME ())
(declare-function PATHNAMEP ())
(declare-function PATHNAME-TYPE ())
(declare-function PATHNAME-VERSION ())
(declare-function PEEK-CHAR ())
(declare-function PHASE ())
(declare-function PLUSP ())
(declare-function POSITION ())
(declare-function POSITION-IF ())
(declare-function POSITION-IF-NOT ())
(declare-function PPRINT-DISPATCH ())
(declare-function PPRINT-FILL ())
(declare-function PPRINT ())
(declare-function PPRINT-INDENT ())
(declare-function PPRINT-LINEAR ())
(declare-function PPRINT-NEWLINE ())
(declare-function PPRINT-TAB ())
(declare-function PPRINT-TABULAR ())
(declare-function PRIN1 ())
(declare-function PRIN1-TO-STRING ())
(declare-function PRINC ())
(declare-function PRINC-TO-STRING ())
(declare-function PRINT ())
(declare-function PRINT-NOT-READABLE-OBJECT ())
(declare-function PROBE-FILE ())
(declare-function PROVIDE ())
(declare-function RANDOM ())
(declare-function RANDOM-STATE-P ())
(declare-function RASSOC ())
(declare-function RASSOC-IF ())
(declare-function RASSOC-IF-NOT ())
(declare-function RATIONAL ())
(declare-function RATIONALIZE ())
(declare-function RATIONALP ())
(declare-function READ-BYTE ())
(declare-function READ-CHAR ())
(declare-function READ-CHAR-NO-HANG ())
(declare-function READ-DELIMITED-LIST ())
(declare-function READ-FROM-STRING ())
(declare-function READ ())
(declare-function READ-LINE ())
(declare-function READ-PRESERVING-WHITESPACE ())
(declare-function READ-SEQUENCE ())
(declare-function READTABLEP ())
(declare-function REALPART ())
(declare-function REALP ())
(declare-function REDUCE ())
(declare-function REM ())
(declare-function REMHASH ())
(declare-function REMOVE-DUPLICATES ())
(declare-function REMOVE ())
(declare-function REMOVE-IF ())
(declare-function REMOVE-IF-NOT ())
(declare-function REMPROP ())
(declare-function RENAME-FILE ())
(declare-function RENAME-PACKAGE ())
(declare-function REPLACE ())
(declare-function REQUIRE ())
(declare-function RESTART-NAME ())
(declare-function REVAPPEND ())
(declare-function REVERSE ())
(declare-function ROOM ())
(declare-function ROUND ())
(declare-function RPLACA ())
(declare-function RPLACD ())
(declare-function SCALE-FLOAT ())
(declare-function SEARCH ())
(declare-function SET-DIFFERENCE ())
(declare-function SET-DISPATCH-MACRO-CHARACTER ())
(declare-function SET-EXCLUSIVE-OR ())
(declare-function SET ())
(declare-function SET-MACRO-CHARACTER ())
(declare-function SET-PPRINT-DISPATCH ())
(declare-function SET-SYNTAX-FROM-CHAR ())
(declare-function SHADOW ())
(declare-function SHADOWING-IMPORT ())
(declare-function SHORT-SITE-NAME ())
(declare-function SIGNAL ())
(declare-function SIGNUM ())
(declare-function SIMPLE-BIT-VECTOR-P ())
(declare-function SIMPLE-CONDITION-FORMAT-ARGUMENTS ())
(declare-function SIMPLE-CONDITION-FORMAT-CONTROL ())
(declare-function SIMPLE-STRING-P ())
(declare-function SIMPLE-VECTOR-P ())
(declare-function SIN (number) :result-type number)
(declare-function SINH (number) :result-type number)
(declare-function SLEEP ())
(declare-function SLOT-BOUNDP ())
(declare-function SLOT-EXISTS-P ())
(declare-function SLOT-MAKUNBOUND ())
(declare-function SLOT-VALUE ())
(declare-function SOFTWARE-TYPE ())
(declare-function SOFTWARE-VERSION ())
(declare-function SOME ())
(declare-function SORT ())
(declare-function SQRT ())
(declare-function STABLE-SORT ())
(declare-function STANDARD-CHAR-P ())
(declare-function STREAM-ELEMENT-TYPE ())
(declare-function STREAM-ERROR-STREAM ())
(declare-function STREAM-EXTERNAL-FORMAT ())
(declare-function STREAMP ())
(declare-function STRING-CAPITALIZE ())
(declare-function STRING-DOWNCASE ())
(declare-function STRING-EQUAL ())
(declare-function STRING<= ())
(declare-function STRING< ())
(declare-function STRING= ())
(declare-function STRING>= ())
(declare-function STRING> ())
(declare-function STRING ())
(declare-function STRING/= ())
(declare-function STRING-GREATERP ())
(declare-function STRING-LEFT-TRIM ())
(declare-function STRING-LESSP ())
(declare-function STRING-NOT-EQUAL ())
(declare-function STRING-NOT-GREATERP ())
(declare-function STRING-NOT-LESSP ())
(declare-function STRINGP ())
(declare-function STRING-RIGHT-TRIM ())
(declare-function STRING-TRIM ())
(declare-function STRING-UPCASE ())
(declare-function SUBLIS ())
(declare-function SUBSETP ())
(declare-function SUBST ())
(declare-function SUBST-IF ())
(declare-function SUBST-IF-NOT ())
(declare-function SUBSTITUTE ())
(declare-function SUBSTITUTE-IF ())
(declare-function SUBSTITUTE-IF-NOT ())
(declare-function SUBTYPEP ())
(declare-function SXHASH ())
(declare-function SYMBOL-NAME ())
(declare-function SYMBOL-PACKAGE ())
(declare-function SYMBOLP ())
(declare-function SYNONYM-STREAM-SYMBOL ())
(declare-function TAILP ())
(declare-function TAN (number) :result-type number)
(declare-function TANH (number) :result-type number)
(declare-function TERPRI ())
(declare-function TRANSLATE-LOGICAL-PATHNAME ())
(declare-function TRANSLATE-PATHNAME ())
(declare-function TREE-EQUAL ())
(declare-function TRUENAME ())
(declare-function TRUNCATE ())
(declare-function TWO-WAY-STREAM-INPUT-STREAM ())
(declare-function TWO-WAY-STREAM-OUTPUT-STREAM ())
(declare-function TYPE-ERROR-DATUM ())
(declare-function TYPE-ERROR-EXPECTED-TYPE ())
(declare-function TYPE-OF ())
(declare-function TYPEP ())
(declare-function UNBOUND-SLOT-INSTANCE ())
(declare-function UNEXPORT ())
(declare-function UNINTERN ())
(declare-function UNION ())
(declare-function UNREAD-CHAR ())
(declare-function UNUSE-PACKAGE ())
(declare-function UPGRADED-ARRAY-ELEMENT-TYPE ())
(declare-function UPGRADED-COMPLEX-PART-TYPE ())
(declare-function UPPER-CASE-P ())
(declare-function USE-PACKAGE ())
(declare-function USER-HOMEDIR-PATHNAME ())
(declare-function VALUES-LIST ())
(declare-function VECTOR ())
(declare-function VECTORP ())
(declare-function VECTOR-POP ())
(declare-function VECTOR-PUSH-EXTEND ())
(declare-function VECTOR-PUSH ())
(declare-function WARN ())
(declare-function WILD-PATHNAME-P ())
(declare-function WRITE-BYTE ())
(declare-function WRITE-CHAR ())
(declare-function WRITE ())
(declare-function WRITE-LINE ())
(declare-function WRITE-SEQUENCE ())
(declare-function WRITE-STRING ())
(declare-function WRITE-TO-STRING ())
(declare-function YES-OR-NO-P ())
(declare-function Y-OR-N-P ())
(declare-function ZEROP ())

(declare-local-function CALL-NEXT-METHOD (&rest arguments))
(declare-local-function NEXT-METHOD-P    ())


(declare-local-macro CALL-METHOD                             ())
(declare-local-macro LOOP-FINISH                             ())
(declare-local-macro MAKE-METHOD                             ())
(declare-local-macro PPRINT-EXIT-IF-LIST-EXHAUSTED           ())
(declare-local-macro PPRINT-POP                              ())

;;; macros --  3. Evaluation and Compilation
;;; macros --  4. Types and Classes
;;; macros --  5. Data and Control Flow
;;; macros --  6. Iteration
;;; macros --  7. Objects
;;; macros --  8. Structures
;;; macros --  9. Conditions
;;; macros -- 10. Symbols
;;; macros -- 11. Packages
;;; macros -- 12. Numbers
;;; macros -- 13. Characters
;;; macros -- 14. Conses
;;; macros -- 15. Arrays
;;; macros -- 16. Strings
;;; macros -- 17. Sequences
;;; macros -- 18. Hash Tables
;;; macros -- 19. Filenames
;;; macros -- 20. Files
;;; macros -- 21. Streams
;;; macros -- 22. Printer
;;; macros -- 23. Reader
;;; macros -- 24. System Construction
;;; macros -- 25. Environment

(declare-macro AND                                      ())
(declare-macro ASSERT                                   ())
(declare-macro CASE                                     ())
(declare-macro CCASE                                    ())
(declare-macro CHECK-TYPE                               ())
(declare-macro COND                                     ())
(declare-macro CTYPECASE                                ())
(declare-macro DECF                                     ())
(declare-macro DECLAIM                                  ())
(declare-macro DEFCLASS                                 ())
(declare-macro DEFCONSTANT                              ())
(declare-macro DEFGENERIC                               ())
(declare-macro DEFINE-COMPILER-MACRO                    ())
(declare-macro DEFINE-CONDITION                         ())
(declare-macro DEFINE-METHOD-COMBINATION                ())
(declare-macro DEFINE-MODIFY-MACRO                      ())
(declare-macro DEFINE-SETF-EXPANDER                     ())
(declare-macro DEFINE-SYMBOL-MACRO                      ())
(declare-macro DEFMACRO                                 ())
(declare-macro DEFMETHOD                                ())
(declare-macro DEFPACKAGE                               ())
(declare-macro DEFPARAMETER                             ())
(declare-macro DEFSETF                                  ())
(declare-macro DEFSTRUCT                                ())
(declare-macro DEFTYPE                                  ())
(declare-macro DEFUN                                    ())
(declare-macro DEFVAR                                   ())
(declare-macro DESTRUCTURING-BIND                       ())
(declare-macro DO-ALL-SYMBOLS                           ())
(declare-macro DO-EXTERNAL-SYMBOLS                      ())
(declare-macro DOLIST                                   ())
(declare-macro DO                                       ())
(declare-macro DO*                                      ())
(declare-macro DO-SYMBOLS                               ())
(declare-macro DOTIMES                                  ())
(declare-macro ECASE                                    ())
(declare-macro ETYPECASE                                ())
(declare-macro FORMATTER                                ())
(declare-macro HANDLER-BIND                             ())
(declare-macro HANDLER-CASE                             ())
(declare-macro IGNORE-ERRORS                            ())
(declare-macro INCF                                     ())
(declare-macro IN-PACKAGE                               ())
(declare-macro LAMBDA                                   ())
(declare-macro LOOP                                     ())
(declare-macro MULTIPLE-VALUE-BIND                      ())
(declare-macro MULTIPLE-VALUE-LIST                      ())
(declare-macro MULTIPLE-VALUE-SETQ                      ())
(declare-macro NTH-VALUE                                ())
(declare-macro OR                                       ())
(declare-macro POP                                      ())
(declare-macro PPRINT-LOGICAL-BLOCK                     ())
(declare-macro PRINT-UNREADABLE-OBJECT                  ())
(declare-macro PROG1                                    ())
(declare-macro PROG2                                    ())
(declare-macro PROG                                     ())
(declare-macro PROG*                                    ())
(declare-macro PSETF                                    ())
(declare-macro PSETQ                                    ())
(declare-macro PUSH                                     ())
(declare-macro PUSHNEW                                  ())
(declare-macro REMF                                     ())
(declare-macro RESTART-BIND                             ())
(declare-macro RESTART-CASE                             ())
(declare-macro RETURN                                   ())
(declare-macro ROTATEF                                  ())
(declare-macro SETF                                     ())
(declare-macro SHIFTF                                   ())
(declare-macro STEP                                     ())
(declare-macro TIME                                     ())
(declare-macro TRACE                                    ())
(declare-macro TYPECASE                                 ())
(declare-macro UNLESS                                   ())
(declare-macro UNTRACE                                  ())
(declare-macro WHEN                                     ())
(declare-macro WITH-ACCESSORS                           ())
(declare-macro WITH-COMPILATION-UNIT                    ())
(declare-macro WITH-CONDITION-RESTARTS                  ())
(declare-macro WITH-HASH-TABLE-ITERATOR                 ())
(declare-macro WITH-INPUT-FROM-STRING                   ())
(declare-macro WITH-OPEN-STREAM                         ())
(declare-macro WITH-OUTPUT-TO-STRING                    ())
(declare-macro WITH-PACKAGE-ITERATOR                    ())
(declare-macro WITH-SIMPLE-RESTART                      ())
(declare-macro WITH-SLOTS                               ())
(declare-macro WITH-STANDARD-IO-SYNTAX                  ())

(declare-restart ABORT                                    ())
(declare-restart CONTINUE                                 ())
(declare-restart MUFFLE-WARNING                           ())
(declare-restart STORE-VALUE                              ())
(declare-restart USE-VALUE                                ())

;;; special operators --  3. Evaluation and Compilation
;;; special operators --  4. Types and Classes
;;; special operators --  5. Data and Control Flow
;;; special operators --  6. Iteration
;;; special operators --  7. Objects
;;; special operators --  8. Structures
;;; special operators --  9. Conditions
;;; special operators -- 10. Symbols
;;; special operators -- 11. Packages
;;; special operators -- 12. Numbers
;;; special operators -- 13. Characters
;;; special operators -- 14. Conses
;;; special operators -- 15. Arrays
;;; special operators -- 16. Strings
;;; special operators -- 17. Sequences
;;; special operators -- 18. Hash Tables
;;; special operators -- 19. Filenames
;;; special operators -- 20. Files
;;; special operators -- 21. Streams
;;; special operators -- 22. Printer
;;; special operators -- 23. Reader
;;; special operators -- 24. System Construction
;;; special operators -- 25. Environment

(declare-special-operator BLOCK                                    ())
(declare-special-operator CATCH                                    ())
(declare-special-operator EVAL-WHEN                                ())
(declare-special-operator FLET                                     ())
(declare-special-operator FUNCTION                                 ())
(declare-special-operator GO                                       ())
(declare-special-operator IF                                       ())
(declare-special-operator LABELS                                   ())
(declare-special-operator LET                                      ())
(declare-special-operator LET*                                     ())
(declare-special-operator LOAD-TIME-VALUE                          ())
(declare-special-operator LOCALLY                                  ())
(declare-special-operator MACROLET                                 ())
(declare-special-operator MULTIPLE-VALUE-CALL                      ())
(declare-special-operator MULTIPLE-VALUE-PROG1                     ())
(declare-special-operator PROGN                                    ())
(declare-special-operator PROGV                                    ())
(declare-special-operator QUOTE                                    ())
(declare-special-operator RETURN-FROM                              ())
(declare-special-operator SETQ                                     ())
(declare-special-operator SYMBOL-MACROLET                          ())
(declare-special-operator TAGBODY                                  ())
(declare-special-operator THE                                      ())
(declare-special-operator THROW                                    ())
(declare-special-operator UNWIND-PROTECT                           ())

;;; generic functions --  3. Evaluation and Compilation
;;; generic functions --  4. Types and Classes
;;; generic functions --  5. Data and Control Flow
;;; generic functions --  6. Iteration
;;; generic functions --  7. Objects
;;; generic functions --  8. Structures
;;; generic functions --  9. Conditions
;;; generic functions -- 10. Symbols
;;; generic functions -- 11. Packages
;;; generic functions -- 12. Numbers
;;; generic functions -- 13. Characters
;;; generic functions -- 14. Conses
;;; generic functions -- 15. Arrays
;;; generic functions -- 16. Strings
;;; generic functions -- 17. Sequences
;;; generic functions -- 18. Hash Tables
;;; generic functions -- 19. Filenames
;;; generic functions -- 20. Files
;;; generic functions -- 21. Streams
;;; generic functions -- 22. Printer
;;; generic functions -- 23. Reader
;;; generic functions -- 24. System Construction
;;; generic functions -- 25. Environment

(declare-standard-generic-function ADD-METHOD                               ())
(declare-standard-generic-function ALLOCATE-INSTANCE                        ())
(declare-standard-generic-function CHANGE-CLASS                             ())
(declare-standard-generic-function CLASS-NAME                               ())
(declare-standard-generic-function COMPUTE-APPLICABLE-METHODS               ())
(declare-standard-generic-function DESCRIBE-OBJECT                          ())
(declare-standard-generic-function DOCUMENTATION                            ())
(declare-standard-generic-function FIND-METHOD                              ())
(declare-standard-generic-function FUNCTION-KEYWORDS                        ())
(declare-standard-generic-function INITIALIZE-INSTANCE                      ())
(declare-standard-generic-function MAKE-INSTANCES-OBSOLETE                  ())
(declare-standard-generic-function MAKE-INSTANCE                            ())
(declare-standard-generic-function MAKE-LOAD-FORM                           ())
(declare-standard-generic-function METHOD-QUALIFIERS                        ())
(declare-standard-generic-function NO-APPLICABLE-METHOD                     ())
(declare-standard-generic-function NO-NEXT-METHOD                           ())
(declare-standard-generic-function PRINT-OBJECT                             ())
(declare-standard-generic-function REINITIALIZE-INSTANCE                    ())
(declare-standard-generic-function REMOVE-METHOD                            ())
(declare-standard-generic-function SHARED-INITIALIZE                        ())
(declare-standard-generic-function SLOT-MISSING                             ())
(declare-standard-generic-function SLOT-UNBOUND                             ())
(declare-standard-generic-function UPDATE-INSTANCE-FOR-DIFFERENT-CLASS      ())
(declare-standard-generic-function UPDATE-INSTANCE-FOR-REDEFINED-CLASS      ())

;;;; THE END ;;;;
