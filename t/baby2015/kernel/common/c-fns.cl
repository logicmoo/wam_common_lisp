;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

(unless (fboundp 'neq)
  (defun neq (o1 o2)
    (not (eq o1 o2))))

;;  AUTHOR:  Juergen Walther, Erich Rome, Franco di Primio


;;-----------------------------------------------------------------
;;        all the string-handling stuff
;;-----------------------------------------------------------------

; Defined in file global-variables
;(defvar *language* 'engl)

(defmacro defbabylon-table (name lang &rest options)
  `(unless (typep (get ',name ',lang) 'hash-table)
     (setf (get ',name ',lang) (make-hash-table ,@options))))

(defmacro defbabylon-entry (key table lang value)
  `(let ((*language* ',lang))
     (setf (gethash ',key (get ',table ',lang)) ,value)))  

(defmacro getentry (key table)
  `(values (gethash ',key (get ',table *language*))))


(defmacro getentry2 (key table)
  `(quote ,(gethash key (get table *language*))))

(defmacro is-entry (key table)
  `(multiple-value-bind (value sw z)
       (gethash ',key (get ',table *language*))
     (declare (ignore value z))
     sw))

;(defmacro get-string (key table)
;  `(gethash ',key ,table))

;;-----------------------------------------------------------------

(defun send-kb (selector &rest args)
  (lexpr-$send *current-knowledge-base* selector args))

(defun send-current-knowledge-base (selector &rest args)
  (lexpr-$send *current-knowledge-base* selector args))

(defun send-bab (selector &rest args)
  (lexpr-$send *babylon* selector args))

(defun send-babylon (selector &rest args)
  (lexpr-$send *babylon* selector args))

(defun current-p (kb)
  (eq kb *current-knowledge-base*))

(defun current-kb-typep (flavor-type &optional string)
  (cond ((flavor-typep *current-knowledge-base* flavor-type))
	(*current-knowledge-base* 
	 (format *default-dialog-stream* "~%=> ~A"
		 (or string (getentry kb-of-wrong-type-str babylon-io-table))))
	(t (format *default-dialog-stream* "~%=>~A"
		   (getentry none-kb-current-str babylon-io-table)))))


;;-----------------------------------------------------------------
;;
;;-----------------------------------------------------------------

;;(defrequest frame-reference :recall :eval-frame-reference ....)

(defmacro defrequest (name &rest plist)
  "Defines methods to be used for a request type."
  `(do ((rlist ',plist (rest (rest rlist))))
       ((null rlist) ',name)
     (setf (get ',name (first rlist)) (second rlist))))

(defmacro assign-typefkt (fkt mixin)
  "Assigns a type predicate to the processor mixin."
  `(setf (get ,mixin :typefkt) ,fkt))


;;-----------------------------------------------------------------
;;
;;-----------------------------------------------------------------


(defun make-blanks (nr)
  (if (< nr 1)
      ""
      (make-sequence 'string nr :initial-element #\space)))


(defun make-string-of-length  (nr &optional (z #\space))
  (declare (fixnum nr))
  (if (< nr 1)
      ""
      (make-sequence 'string nr :initial-element (character z))))


(defun complete-to-n (str nr)
  "verlaengert str um nr blanks bzw. kuerzt str um nr+3"
  (declare (fixnum nr) (simple-string str))
  (if (< nr 0)
      (concatenate 'string (subseq str 0 (+ (length str) nr -3)) "...")
      (concatenate 'string str (make-sequence 'string nr :initial-element #\space))))



;;-----------------------------------------------------------------

(defun is-simple-list (l)
  (and (listp l)
       (every 'atom l)))


(defun is-true-list (x)			
  (and (not (atom x))
       (null (cdr (last x)))))


(defun from-list-to-string (list)
  (let ((*print-pretty* nil))
    (format nil "~S" list)))


(defun remove-doubles (list &optional result)
  (cond ((null list) (nreverse result))
	((member (first list) result :test 'equal)
	 (remove-doubles (rest list) result))
	(t (remove-doubles (rest list) (cons (first list) result)))))


;;-----------------------------------------------------------------

;
;(defmacro declare-lisp-fns (&rest fns)
;  `(mapc #'(lambda (fn)
;	     (setf (get fn 'LISP) t))
;	 ',fns))
;
;(defmacro undeclare-lisp-fns (&rest fns)
;  `(mapc #'(lambda (fn)
;	     (remprop fn 'LISP))
;	 ',fns))
;
;(declare-lisp-fns stop-execution stop-kb-execution ask-for
;		  say find-implications test-hypotheses obtain)

(defmacro lisp (&rest formlist)
  `(progn . ,formlist))

;;-----------------------------------------------------------------

(defun make-multiple-value (x)
  (cond ((atom x) x)
	((null (cdr x)) (first x))
	(t `(:MULTIPLE-VALUE . ,x))))

(defun make-multiple-answer (x)
  (cond ((atom x) x)
	((null (cdr x)) (first x))
	(t `(:MULTIPLE-VALUE . ,x))))

(defun is-multiple-value (x)
  (and (listp x) (eq (first x) :MULTIPLE-VALUE)))

(defun is-multiple-answer (x)
  (and (listp x) (eq (first x) :MULTIPLE-VALUE))) 

;;-----------------------------------------------------------------

(defun compute-term (term)
  (if (is-negated-term term)
      (get-positive-term term)
      term))

(defun is-negated-term (term)
  (and (listp term) (eq (first term) 'NOT)))

(defun get-positive-term (negated-term)
  (second negated-term))

(defun get-negation (term)
  (first term))

;;-----------------------------------------------------------------

(defun undetermined ()
  "initial content of :value facet (localstate)"
  '-)

(defun undetermined-2 () 
  "for use in rules"
  '(UNDETERMINED UNBESTIMMT))


(defun is-undetermined (x)
  (or (eq x (undetermined))
      (member x (undetermined-2))))

(defun unknown () 
  "standard possible answer of USER"
  'UNKNOWN)

(defun unknown-2 ()
  '(UNBEKANNT))

(defun is-unknown (x)
  (or (eq x (UNKNOWN))
      (member x (unknown-2))))

(defun is-help (x)
  (eq x 'HELP))

;;-------------------------------------------------------------------------------

(defun is-variable (x)
  (and (symbolp x)
       (char-equal (aref (string x) 0) #\_)))

(defun contains-vars (exp)		
  "Yields true if <exp> is resp. contains a prolog variable." 
  (cond ((is-variable exp) t)
	((atom exp) nil)
	((contains-vars (car exp)) t)
	((contains-vars (cdr exp)) t)
	(t nil)))

;;--------------------------------------------------------------------------------

(defun say (string &rest args)  
  (lexpr-$send *current-knowledge-base* :babylon-format string args))

;;--------------------------------------------------------------------------------

;;  normalize-answer 
;;  ---------------

(defun normalize-answer (answer)
  (or (cdr (assoc answer (getentry possible-answers babylon-io-table)))
      answer))

(defun translate-answer (answer)
  (or (car (rassoc answer (getentry possible-answers babylon-io-table)))
      answer))

;;; eof

