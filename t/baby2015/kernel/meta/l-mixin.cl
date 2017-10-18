;;; -*- Mode: LISP; Package: BABYLON; Syntax: Common-Lisp; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHORS:  Franco di Primio, Eckehard Gross, Juergen Walther


(def$flavor lisp-mixin
	   ()
	   ()
  (:required-instance-variables active-proc)
  (:documentation "This is the lisp-mixin flavor.
It provides all the facilities to use Lisp functions in the context
of a knowledge base."))


(defrequest lisp-form			
	    :remember :eval-lisp
	    :store    :eval-lisp
	    :recall   :eval-lisp
	    :prolog   :eval-lisp-form-for-prolog)

(defmacro lisp-type (request)
  `(cond ((atom ,request) 'lisp-form)
	 ((and (symbolp (first ,request))
	       (or (fboundp (first ,request))
		   (macro-function (first ,request)))
	       'lisp-form))))

(assign-typefkt 'lisp-type 'lisp-mixin)

(def$method (lisp-mixin :eval-lisp) (form mode)
  "Method to evaluate lisp functions in the context of a kb."
  (declare (ignore mode))
  (setf active-proc self)
  (eval form))

(def$method (lisp-mixin :eval-lisp-form-for-prolog) (form mode)
  "This implements the gentle method to evaluate lisp forms in Prolog!
  method :EVAL-LISP represents the crude mode.
  get an error, if (first form) is a MACRO or a special function!!!"
  (declare (ignore mode))
  (setf active-proc self)
  (let ((fn (first form)) ;; a function, not a macro!!!!
	(args (butlast (cdr form)))
	(result (first (last form))))
    (if (not (CONTAINS-VARS args))
	(let ((call-result ;; ist eine Liste von einem oder mehreren Werten.
		(multiple-value-list ;; Das, um auch multiple values zu behandeln
		  (apply fn args)))
	      (clauses nil))
	  ;; Die Moeglichkeit, Funktionen zu benutzen, die multiple values zurueckgeben,
	  ;; macht Lisp aus Prolog-Sicht sehr interessant!!! (Franco)
	  (if (is-variable result) ;; gives a list of clauses as result
	      (dolist (a-value call-result (nreverse clauses))
		(setf clauses (cons `((,fn ,@args ,a-value)) clauses)))
	      ;; else T or NIL 
	      (not (null (member result call-result))))))))