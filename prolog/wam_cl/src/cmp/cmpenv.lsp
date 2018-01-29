;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPENV  Environments of the Compiler.

(in-package 'compiler)

(defvar *safe-compile* nil)
(defvar *compiler-check-args* nil)
(defvar *compiler-push-events* nil)
(defvar *speed* 3)
(defvar *space* 0)

;;; Only these flags are set by the user.
;;; If *safe-compile* is ON, some kind of run-time checks are not
;;; included in the compiled code.  The default value is OFF.

(defun init-env ()
  (setq *max-temp* 0)
  (setq *temp* 0)
  (setq *next-cmacro* 0)
  (setq *next-vv* -1)
  (setq *next-cfun* 0)
  (setq *last-label* 0)
  (setq *objects* nil)
  (setq *constants* nil)
  (setq *local-funs* nil)
  (setq *global-funs* nil)
  (setq *linking-calls* nil)
  (setq *global-entries* nil)
  (setq *undefined-vars* nil)
  (setq *reservations* nil)
  (setq *closures* nil)
  (setq *top-level-forms* nil)
  (setq *compile-time-too* nil)
  (setq *non-package-operation* nil)
  (setq *function-declarations* nil)
  (setq *inline-functions* nil)
  (setq *inline-blocks* 0)
  (setq *notinline* nil)
  )

;;; Compiled code uses the following kinds of variables:
;;; 1. Vi, declared explicitely, either unboxed or register (*lcl*, next-lcl)
;;; 2. Ti, declared collectively, of type object, may be reused (*temp*, next-temp)
;;; 3. Ui, declared collectively, of type unboxed (*unboxed*, next-unboxed)
;;; 4. lexi[j], for lexical variables in local functions
;;; 5. CLVi, for lexical variables in closures

;;; The following counters are used:

(defvar *lcl* 0)		; number of local variables

(defvar *temp* 0)		; number of temporary variables
(defvar *max-temp* 0)		; maximum *temp* reached

(defvar *unboxed*)		; list of unboxed variables
(defvar *next-unboxed* 0)	; number of *unboxed* used.

(defvar *level* 0)		; nesting level for local functions

(defvar *lex* 0)		; number of lexical variables in local functions
(defvar *max-lex* 0)		; maximum *lex* reached

(defvar *env* 0)		; number of variables in current form
(defvar *max-env* 0)		; maximum *env* in whole function
(defvar *env-lvl* 0)		; number of levels of environments

(defvar *next-cmacro* 0)	; holds the last cmacro number used.
(defvar *next-vv* -1)		; holds the last VV index used.
(defvar *next-cfun* 0)		; holds the last cfun used.

(defun next-lcl () (incf *lcl*))

(defun next-temp ()
  (prog1 *temp*
         (incf *temp*)
         (setq *max-temp* (max *temp* *max-temp*))))

(defun next-unboxed (type)
  (let ((tem (incf *next-unboxed*)))
    (push (list (rep-type type) tem) *unboxed*)
    tem))

(defun next-lex ()
  (prog1 (cons *level* *lex*)
         (incf *lex*)
         (setq *max-lex* (max *lex* *max-lex*))))

(defun next-env () (prog1 *env*
		     (incf *env*)
		     (setq *max-env* (max *env* *max-env*))))

(defun add-symbol (symbol)
  (let ((x (assoc symbol *objects*)))
       (cond (x (second x))
             (t (incf *next-vv*)
                (push (list symbol *next-vv*) *objects*)
                (wt-data symbol)
                *next-vv*))))

(defun add-object (object &aux x)
  ;;; Used only during Pass 1.
  (cond ((sys:contains-sharp-comma object)
         ;;; SYS:CONTAINS-SHARP-COMMA returns T iff OBJECT
         ;;; contains a sharp comma OR a structure.
         (incf *next-vv*)
         (push *next-vv* *sharp-commas*)
         (wt-data (prin1-to-string object))
         *next-vv*)
        ((setq x (assoc object *objects*))
         (second x))
        (t (incf *next-vv*)
           (push (list object *next-vv*) *objects*)
           (wt-data object)
           *next-vv*)))

(defun add-constant (symbol &aux x)
  ;;; Used only during Pass 1.
  (cond ((setq x (assoc symbol *constants*))
         (second x))
        (t (incf *next-vv*)
           (push *next-vv* *sharp-commas*)
           (wt-data (prin1-to-string (cons 'sys:|#,| symbol)))
           (push (list symbol *next-vv*) *constants*)
           *next-vv*)))

;;; Tail recursion information.
(defvar *tail-recursion-info* nil)
;;; *tail-recursion-info* holds NIL, if tail recursion is impossible.
;;; If possible, *tail-recursion-info* holds
;;;	( fname  required-arg .... required-arg ),
;;; where each required-arg is a var-object.


(defvar *function-declarations* nil)
;;; *function-declarations* holds :
;;;	(... ( { function-name | fun-object } arg-types return-type ) ...)
;;; Function declarations for global functions are ASSOCed by function names,
;;; whereas those for local functions are ASSOCed by function objects.
;;;
;;; The valid argment type declaration is:
;;;	( {type}* [ &optional {type}* ] [ &rest type ] [ &key {type}* ] )
;;; though &optional, &rest, and &key return types are simply ignored.

(defun function-arg-types (arg-types &aux (types nil))
  (do ((al arg-types (cdr al)))
      ((or (endp al)
           (member (car al) '(&optional &rest &key)))
       (nreverse types))
      (declare (object al))
      (push (type-filter (car al)) types)))

;;; The valid return type declaration is:
;;;	(( VALUES {type}* )) or ( {type}* ).

(defun function-return-type (return-types)
  (cond ((endp return-types) t)
        ((and (consp (car return-types))
              (eq (caar return-types) 'VALUES))
         (cond ((not (endp (cdr return-types)))
                (warn "The function return types ~s is illegal." return-types)
                t)
               ((or (endp (cdar return-types))
                    (member (cadar return-types) '(&optional &rest &key)))
                t)
               (t (type-filter (cadar return-types)))))
        (t (type-filter (car return-types)))))

(defun add-function-proclamation (fname decl &aux
         arg-types return-types)
  (cond ((and (symbolp fname)
	      (listp decl) (listp (cdr decl)))
	 (cond ((or (null decl)(eq (car decl) '*)) (setq arg-types '*)
		(remprop fname 'PROCLAIMED-ARG-TYPES))
	       (t (setq arg-types (function-arg-types (car decl)))
		  (setf (get fname 'PROCLAIMED-ARG-TYPES) arg-types)))
	 (cond ((or (null (cdr decl))(eq (second decl) '*))
		(setq return-types '*))
	       (t (setq return-types (function-return-type (cdr decl)))))
         (setf (get fname 'PROCLAIMED-RETURN-TYPE) return-types)
	 (cond((eql return-types '*))
	      (t(setq return-types (cdr decl))))
	 ;;; A non-local function may have local entry only if it returns
	 ;;; a single value.
         (if (and (not (endp return-types))
                  (endp (cdr return-types))
                  (not (and (consp (car return-types))
                            (eq (caar return-types) 'VALUES)
                            (or (endp (cdar return-types))
                                (not (endp (cddar return-types)))))))
             (setf (get fname 'PROCLAIMED-FUNCTION) t)
	   (remprop fname 'PROCLAIMED-FUNCTION)))
        (t (warn "The function procl ~s ~s is not valid." fname decl))))

(defun add-function-declaration (fname arg-types return-types)
  (cond ((symbolp fname)
         (push (list (sch-local-fun fname)
                     (function-arg-types arg-types)
                     (function-return-type return-types))
               *function-declarations*))
        (t (warn "The function name ~s is not a symbol." fname))))

(defun get-arg-types (fname &aux x)
  (if (setq x (assoc fname *function-declarations*))
      (second x)
      (get fname 'PROCLAIMED-ARG-TYPES)))

(defun get-return-type (fname)
  (let* ((x (assoc fname *function-declarations*))
         (type1 (if x (caddr x) (get fname 'PROCLAIMED-RETURN-TYPE))))
        (cond (type1
               (let ((type (get fname 'RETURN-TYPE)))
                    (cond (type
                           (cond ((setq type (type-and type type1)) type)
                                 (t
                                  (cmpwarn
                                   "The return type of ~s was badly declared."
                                   fname))))
                          (t type1))))
              (t (get fname 'RETURN-TYPE)))
        ))

(defun get-local-arg-types (fun &aux x)
  (if (setq x (assoc fun *function-declarations*))
      (second x)
      nil))

(defun get-local-return-type (fun &aux x)
  (if (setq x (assoc fun *function-declarations*))
      (caddr x)
      nil))

;;; Proclamation and declaration handling.

(defvar *alien-declarations* nil)
(defvar *notinline* nil)

(defun inline-possible (fname)
       (not (or ; *compiler-push-events*
                (member fname *notinline*)
                (get fname 'CMP-NOTINLINE))))

#-:CCL
(defun proclaim (decl)
  (unless (listp decl)
	  (error "The proclamation specification ~s is not a list" decl))
  (case (car decl)
    (SPECIAL
     (dolist (var (cdr decl))
       (if (symbolp var)
           (sys:*make-special var)
           (warn "The variable name ~s is not a symbol." var))))
    (OPTIMIZE
     (dolist (x (cdr decl))
       (when (symbolp x) (setq x (list x 3)))
       (if (or (not (consp x))
               (not (consp (cdr x)))
               (not (numberp (second x)))
               (not (<= 0 (second x) 3)))
           (warn "The OPTIMIZE proclamation ~s is illegal." x)
           (case (car x)
                 (SAFETY (setq *compiler-check-args* (>= (second x) 1))
                         (setq *safe-compile* (>= (second x) 2))
                         (setq *compiler-push-events* (>= (second x) 3)))
                 (SPACE (setq *space* (second x)))
                 (SPEED (setq *speed* (second x)))
                 (COMPILATION-SPEED (setq *speed* (- 3 (second x))))
                 (t (warn "The OPTIMIZE quality ~s is unknown." (car x)))))))
    (TYPE
     (if (consp (cdr decl))
         (proclaim-var (second decl) (cddr decl))
         (warn "The type declaration ~s is illegal." decl)))
    ((FIXNUM CHARACTER SHORT-FLOAT LONG-FLOAT)
     (proclaim-var (car decl) (cdr decl)))
    (FTYPE
     (cond ((and (consp (cdr decl))
		 (consp (second decl)))
	    (eq (caadr decl) 'FUNCTION)
	    (dolist (v (cddr decl))
		    (add-function-proclamation v (cdr (second decl)))
		    ))
	   (t (cmpwarn "Bad function proclamation ~a" decl))))
    (FUNCTION
     (cond ((and (consp (cdr decl)))
	    (add-function-proclamation (second decl) (cddr decl)))
	   (t (cmpwarn "Bad function proclamation ~a" decl))))
    (INLINE
     (dolist (fun (cdr decl))
               (if (symbolp fun)
                   (remprop fun 'CMP-NOTINLINE)
                   (warn "The function name ~s is not a symbol." fun))))
    (NOTINLINE
     (dolist (fun (cdr decl))
               (if (symbolp fun)
                   (setf (get fun 'CMP-NOTINLINE) t)
                   (warn "The function name ~s is not a symbol." fun))))
    ((OBJECT IGNORE)
     (dolist (var (cdr decl))
       (unless (symbolp var)
               (warn "The variable name ~s is not a symbol." var))))
    (DECLARATION
     (dolist (x (cdr decl))
       (if (symbolp x)
           (pushnew x *alien-declarations*)
           (warn "The declaration specifier ~s is not a symbol." x))))
    ((ARRAY ATOM BIGNUM BIT BIT-VECTOR CHARACTER COMMON COMPILED-FUNCTION
      COMPLEX CONS DOUBLE-FLOAT FIXNUM FLOAT HASH-TABLE INTEGER KEYWORD LIST
      LONG-FLOAT NIL NULL NUMBER PACKAGE PATHNAME RANDOM-STATE RATIO RATIONAL
      READTABLE SEQUENCE SHORT-FLOAT SIMPLE-ARRAY SIMPLE-BIT-VECTOR
      SIMPLE-STRING SIMPLE-VECTOR SINGLE-FLOAT STANDARD-CHAR STREAM STRING
      STRING-CHAR SYMBOL T VECTOR SIGNED-BYTE UNSIGNED-BYTE)
     (proclaim-var (car decl) (cdr decl)))
    (otherwise
     (unless (member (car decl) *alien-declarations*)
             (warn "The declaration specifier ~s is unknown." (car decl)))
     (and (functionp (get (car decl) :proclaim))
	  (dolist (v (cdr decl))
		    (funcall (get (car decl) :proclaim) v))))
    )
  nil
  )

(defun proclaim-var (type vl)
  (setq type (type-filter type))
  (dolist (var vl)
    (if (symbolp var)
	(let ((type1 (get var 'CMP-TYPE))
	      (v (sch-global var)))
	  (setq type1 (if type1 (type-and type1 type) type))
	  (when v (setq type1 (type-and type1 (var-type v))))
	  (unless type1
	    (warn
	     "Inconsistent type declaration was found for the variable ~s."
	     var)
	    (setq type1 T))
	  (setf (get var 'CMP-TYPE) type1)
	  (when v (setf (var-type v) type1)))
	(warn "The variable name ~s is not a symbol." var))))

(defun c1body (body doc-p &aux
		    (ss nil)		; special vars
		    (is nil)		; ignored vars
		    (ts nil)		; typed vars (var . type)
		    (others nil)	; all other vars
                    doc form)
  (loop
    (when (endp body) (return))
    (setq form (cmp-macroexpand (car body)))
    (cond
     ((stringp form)
      (when (or (null doc-p) (endp (cdr body)) doc) (return))
      (setq doc form))
     ((and (consp form) (eq (car form) 'DECLARE))
      (dolist (decl (cdr form))
        (cmpck (or (not (consp decl)) (not (symbolp (car decl))))
               "The declaration ~s is illegal." decl)
        (case (car decl)
          (SPECIAL
           (dolist (var (cdr decl))
             (cmpck (not (symbolp var))
                    "The special declaration ~s contains a non-symbol ~s."
                    decl var)
             (push var ss)))
          (IGNORE
           (dolist (var (cdr decl))
             (cmpck (not (symbolp var))
                    "The ignore declaration ~s contains a non-symbol ~s."
                    decl var)
             (push var is)))
          (TYPE
           (cmpck (endp (cdr decl))
                  "The type declaration ~s is illegal." decl)
           (let ((type (type-filter (second decl))))
                (when type
                      (dolist (var (cddr decl))
                        (cmpck (not (symbolp var))
                          "The type declaration ~s contains a non-symbol ~s."
                          decl var)
                        (push (cons var type) ts)))))
          (OBJECT
           (dolist (var (cdr decl))
             (cmpck (not (symbolp var))
                    "The object declaration ~s contains a non-symbol ~s."
                    decl var)
             (push (cons var 'OBJECT) ts)))
	  (:REGISTER
           (dolist (var (cdr decl))
	      (cmpck (not (symbolp var))
		     "The register declaration ~s contains a non-symbol ~s."
		     decl var)
	      (push (cons var 'REGISTER) ts)
	      ))
	  ;; read-only variable treatment. Beppe
	  (:READ-ONLY
#| obsolete
           (dolist (var (cdr decl))
	      (cmpck (not (symbolp var))
		     "In the :read-only declaration ~s, ~s is not a symbol."
		     decl var)
	      (push (cons var 'READ-ONLY) ts))
|#
	      )
          ((FIXNUM CHARACTER DOUBLE-FLOAT SHORT-FLOAT ARRAY ATOM BIGNUM BIT
            BIT-VECTOR COMMON COMPILED-FUNCTION COMPLEX CONS FLOAT HASH-TABLE
            INTEGER KEYWORD LIST LONG-FLOAT NIL NULL NUMBER PACKAGE PATHNAME
            RANDOM-STATE RATIO RATIONAL READTABLE SEQUENCE SIMPLE-ARRAY
            SIMPLE-BIT-VECTOR SIMPLE-STRING SIMPLE-VECTOR SINGLE-FLOAT
            STANDARD-CHAR STREAM STRING STRING-CHAR SYMBOL T VECTOR
            SIGNED-BYTE UNSIGNED-BYTE)
           (let ((type (type-filter (car decl))))
                (when type
                      (dolist (var (cdr decl))
                        (cmpck (not (symbolp var))
                          "The type declaration ~s contains a non-symbol ~s."
                          decl var)
                        (push (cons var type) ts)))))
          (otherwise (push decl others))
          )))
     (t (return)))
    (pop body)
    )
  (values body ss ts is others doc)
  )

(defun c1decl-body (decls body &aux (dl nil))
  (if (null decls)
      (c1progn body)
      (let ((*function-declarations* *function-declarations*)
            (*alien-declarations* *alien-declarations*)
            (*notinline* *notinline*)
            (*space* *space*))
           (dolist (decl decls dl)
             (case (car decl)
              (OPTIMIZE
               (dolist (x (cdr decl))
                 (when (symbolp x) (setq x (list x 3)))
                 (if (or (not (consp x))
                         (not (consp (cdr x)))
                         (not (numberp (second x)))
                         (not (<= 0 (second x) 3)))
                     (warn "The OPTIMIZE proclamation ~s is illegal." x)
                     (case (car x)
                           (SAFETY (push (list 'SAFETY (second x)) dl))
                           (SPACE (setq *space* (second x))
                                  (push (list 'SPACE (second x)) dl))
                           ((SPEED COMPILATION-SPEED))
                           (t (warn "The OPTIMIZE quality ~s is unknown."
                                    (car x)))))))
              (FTYPE
               (if (or (endp (cdr decl))
                       (not (consp (second decl)))
                       (not (eq (caadr decl) 'FUNCTION))
                       (endp (cdadr decl)))
                   (warn "The function declaration ~s is illegal." decl)
                   (dolist (fname (cddr decl))
                     (add-function-declaration
                      fname (cadadr decl) (cddadr decl)))))
              (FUNCTION
               (if (or (endp (cdr decl))
                       (endp (cddr decl))
                       (not (symbolp (second decl))))
                   (warn "The function declaration ~s is illegal." decl)
                   (add-function-declaration
                    (second decl) (caddr decl) (cdddr decl))))
              (INLINE
               (dolist (fun (cdr decl))
                 (if (symbolp fun)
                     (progn (push (list 'INLINE fun) dl)
                            (setq *notinline* (remove fun *notinline*)))
                     (warn "The function name ~s is not a symbol." fun))))
              (NOTINLINE
               (dolist (fun (cdr decl))
                 (if (symbolp fun)
                     (progn (push (list 'NOTINLINE fun) dl)
                            (push fun *notinline*))
                     (warn "The function name ~s is not a symbol." fun))))
              (DECLARATION
               (dolist (x (cdr decl))
                 (if (symbolp x)
                     (pushnew x *alien-declarations*)
                     (warn "The declaration specifier ~s is not a symbol."
                           x))))
              (otherwise
               (unless (member (car decl) *alien-declarations*)
                 (warn "The declaration specifier ~s is unknown."
                       (car decl))))
              ))
           (setq body (c1progn body))
           (list 'DECL-BODY (second body) dl body)
           )
      )
  )

(setf (get 'decl-body 'c2) 'c2decl-body)

(defun c2decl-body (decls body)
  (let ((*compiler-check-args* *compiler-check-args*)
        (*safe-compile* *safe-compile*)
        (*compiler-push-events* *compiler-push-events*)
        (*notinline* *notinline*)
        (*space* *space*))
       (dolist (decl decls)
         (case (car decl)
               (SAFETY
                (let ((level (second decl)))
                     (declare (fixnum level))
                     (setq *compiler-check-args* (>= level 1)
                           *safe-compile* (>= level 2)
                           *compiler-push-events* (>= level 3))))
               (SPACE (setq *space* (second decl)))
               (NOTINLINE (push (second decl) *notinline*))
               (INLINE
                (setq *notinline* (remove (second decl) *notinline*)))
               (otherwise (baboon))))
       (c2expr body))
  )

(defun check-vdecl (vnames ts is)
  (dolist (x ts)
    (unless (member (car x) vnames)
      (cmpwarn "Type declaration was found for not bound variable ~s."
               (car x))))
  (dolist (x is)
    (unless (member x vnames)
      (cmpwarn "Ignore declaration was found for not bound variable ~s." x)))
  )

(defun proclamation (decl)
  (unless (listp decl)
	  (error "The proclamation specification ~s is not a list" decl))
  (case (car decl)
    (SPECIAL
     (dolist (var (cdr decl) t)
       (if (symbolp var)
;;; Beppe
;;;           (unless (sys:specialp var) (return nil))

           (unless (or (sys:specialp var) (check-global var)) (return nil))
           (warn "The variable name ~s is not a symbol." var))))
    (OPTIMIZE
     (dolist (x (cdr decl) t)
       (when (symbolp x) (setq x (list x 3)))
       (if (or (not (consp x))
               (not (consp (cdr x)))
               (not (numberp (second x)))
               (not (<= 0 (second x) 3)))
           (warn "The OPTIMIZE proclamation ~s is illegal." x)
           (case (car x)
                 (SAFETY
                  (unless (= (second x)
                             (cond ((null *compiler-check-args*) 0)
                                   ((null *safe-compile*) 1)
                                   ((null *compiler-push-events*) 2)
                                   (t 3)))
                          (return nil)))
                 (SPACE (unless (= (second x) *space*) (return nil)))
                 (SPEED (unless (= (second x) *speed*) (return nil)))
                 (COMPILATION-SPEED
                  (unless (= (- 3 (second x)) *speed*) (return nil)))
                 (t (warn "The OPTIMIZE quality ~s is unknown."
                          (car x)))))))
    (TYPE
     (if (consp (cdr decl))
         (let ((type (type-filter (second decl)))
               x)
              (dolist (var (cddr decl) t)
                (if (symbolp var)
                    (unless (and (setq x (get var 'CMP-TYPE))
                                 (equal x type))
                            (return nil))
                    (warn "The variable name ~s is not a symbol." var))))
         (warn "The type declaration ~s is illegal." decl)))
    ((FIXNUM CHARACTER SHORT-FLOAT LONG-FLOAT)
     (let ((type (type-filter (car decl)))
           x)
          (dolist (var (cdr decl) t)
            (if (symbolp var)
                (unless (and (setq x (get var 'CMP-TYPE)) (equal x type))
                        (return nil))
                (warn "The variable name ~s is not a symbol." var)))))
    (FTYPE
     (if (or (endp (cdr decl))
             (not (consp (second decl)))
             (not (eq (caadr decl) 'FUNCTION))
             (endp (cdadr decl)))
         (warn "The function declaration ~s is illegal." decl)
         (dolist (fname (cddr decl) t)
           (unless (and (get fname 'PROCLAIMED-FUNCTION)
                        (equal (function-arg-types (cadadr decl))
                               (get fname 'PROCLAIMED-ARG-TYPES))
                        (equal (function-return-type (cddadr decl))
                               (get fname 'PROCLAIMED-RETURN-TYPE)))
                   (return nil)))))
    (FUNCTION
     (if (or (endp (cdr decl)) (endp (cddr decl)))
         (warn "The function declaration ~s is illegal." decl)
         (and (get (second decl) 'PROCLAIMED-FUNCTION)
              (equal (function-arg-types (caddr decl))
                     (get (second decl) 'PROCLAIMED-ARG-TYPES))
              (equal (function-return-type (cdddr decl))
                     (get (second decl) 'PROCLAIMED-RETURN-TYPE)))))
    (INLINE (dolist (fun (cdr decl) t)
              (if (symbolp fun)
                  (when (get fun 'CMP-NOTINLINE) (return nil))
                  (warn "The function name ~s is not a symbol." fun))))
    (NOTINLINE (dolist (fun (cdr decl) t)
                 (if (symbolp fun)
                     (unless (get fun 'CMP-NOTINLINE) (return nil))
                     (warn "The function name ~s is not a symbol." fun))))
    ((OBJECT IGNORE)
     (dolist (var (cdr decl) t)
               (unless (symbolp var)
                       (warn "The variable name ~s is not a symbol." var))))
    (DECLARATION (dolist (x (cdr decl) t)
                   (if (symbolp x)
                       (unless (member x *alien-declarations*) (return nil))
                       (warn "The declaration specifier ~s is not a symbol."
                             x))))
    ((ARRAY ATOM BIGNUM BIT BIT-VECTOR CHARACTER COMMON COMPILED-FUNCTION
      COMPLEX CONS DOUBLE-FLOAT FIXNUM FLOAT HASH-TABLE INTEGER KEYWORD LIST
      LONG-FLOAT NIL NULL NUMBER PACKAGE PATHNAME RANDOM-STATE RATIO RATIONAL
      READTABLE SEQUENCE SHORT-FLOAT SIMPLE-ARRAY SIMPLE-BIT-VECTOR
      SIMPLE-STRING SIMPLE-VECTOR SINGLE-FLOAT STANDARD-CHAR STREAM STRING
      STRING-CHAR SYMBOL T VECTOR SIGNED-BYTE UNSIGNED-BYTE)
     (dolist (var (cdr decl) t)
       (if (symbolp var)
	   (unless (equal (get var 'CMP-TYPE) (type-filter (car decl)))
	     (return nil))
	   (warn "The variable name ~s is not a symbol." var))))
    (otherwise
     (unless (member (car decl) *alien-declarations*)
             (warn "The declaration specifier ~s is unknown." (car decl))))
    )
  )
