;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.



;;; Program Development Environment

(in-package 'system)
(setq *record-source-pathname-p* nil)
(defun record-source-pathname (symbol type)
  ;; type is either:
  ;; 1. a symbol, for single entry definitions (defun, defvar, defclass ..)
  ;; 2. a list (type . spec), for multiple entries (defmethod)
  (when (and *record-source-pathname-p*
	     *source-pathname*)
    (when (sys::setf-namep symbol)
       (setq symbol (get (second symbol) 'setf-symbol)))
    (if (symbolp type)
	(putprop symbol *source-pathname* type)
	(let ((alist (get symbol (car type)))
	      (spec (cdr type)))
	  (if alist
	      (let ((entry (assoc spec alist :test #'equal)))
		(if entry
		    (setf (cdr entry) *source-pathname*)
		    (push (cons spec *source-pathname*) alist)))
	      (setq alist (list (cons spec *source-pathname*))))
	  (putprop symbol alist (car type))))))

;;; Go into LISP.
(in-package 'lisp)

(defun lisp-implementation-type () "ECoLisp")

;;; Compiler functions.

(defun proclaim (d)
       (when (eq (car d) 'SPECIAL) (mapc #'sys::*make-special (cdr d))))

(defun proclamation (d)
  (and (eq (car d) 'SPECIAL)
       (dolist (var (cdr d) t)
               (unless (sys::specialp var) (return nil)))))

(defun compile-file (&rest args)
  (load (merge-pathnames sys:*system-directory* "compiler"))
  (apply 'compile-file args))
(defun compile (&rest args)
  (load (merge-pathnames sys:*system-directory* "compiler"))
  (apply 'compile args))
(defun disassemble (&rest args)
  (load (merge-pathnames sys:*system-directory* "compiler"))
  (apply 'disassemble args))


(defun get-decoded-time ()
  (decode-universal-time (get-universal-time)))

;;; Editor.

(defun ed (&optional filename)
  (system (format nil "emacs ~A" filename)))


;;; Allocator.

(import 'sys::allocate)
(export '(allocate cfun cclosure structure))

(defvar *type-list*
        '(cons
          ;; fixnum Beppe
	  bignum ratio short-float long-float complex
          symbol package hash-table
          array vector string bit-vector
          stream random-state readtable pathname
          cfun cclosure
	  #-clos structure #+clos instance #+clos generic-function
	  #+threads cont #+threads thread))

(defun room (&optional x)
  (let (npage info-list link-alist)
    (multiple-value-bind
	  (maxpage leftpage ncbpage maxcbpage ncb cbgbccount
		   holepage rbused rbfree nrbpage rbgbccount l)
	(sys::room-report)

      (do ((l l (nthcdr 5 l))
	   (tl *type-list* (cdr tl))
	   (i 0 (+ i (if (nth 2 l) (nth 2 l) 0))))
	  ((null l) (setq npage i))
	(let ((typename (car tl))
	      (nused (nth 0 l))
	      (nfree (nth 1 l))
	      (npage (nth 2 l))
	      (maxpage (nth 3 l))
	      (gbccount (nth 4 l)))
	  (if nused
	      (push (list typename npage maxpage
			  (if (zerop (+ nused nfree))
			      0
			      (/ nused 0.01 (+ nused nfree)))
			  (if (zerop gbccount) nil gbccount))
		    info-list)
	      (let ((a (assoc (nth nfree *type-list*) link-alist)))
		(if a
		    (nconc a (list typename))
		    (push (list (nth nfree *type-list*) typename)
			  link-alist))))))
      (dolist (info (nreverse info-list))
	(apply #'format t "~4D/~D~10T~5,1F%~@[~3D~]~20T~{~A~^ ~}"
	       (append (cdr info)
		       (if  (assoc (car info) link-alist)
			    (list (assoc (car info) link-alist))
			    (list (list (car info))))))
	(terpri)
	)
      (terpri)
      (format t "~4D/~D~16T~@[~3D~]~20Tcontiguous (~D blocks)~%"
	      ncbpage maxcbpage (if (zerop cbgbccount) nil cbgbccount) ncb)
      (format t "~5T~D~20Thole~%" holepage)
      (format t "~5T~D~10T~5,1F%~@[~3D~]~20Trelocatable~%~%"
	      nrbpage (/ rbused 0.01 (+ rbused rbfree))
	      (if (zerop rbgbccount) nil rbgbccount))
      (format t "~5D pages for cells~%" npage)
      (format t "~5D total pages~%" (+ npage ncbpage holepage nrbpage))
      (format t "~5D pages available~%" leftpage)
      (format t "~5D pages in heap but not gc'd + pages needed for gc marking~%"
	      (- maxpage (+ npage ncbpage holepage nrbpage leftpage)))
      (format t "~5D maximum pages~%" maxpage)
      (values)
      )))


;;; C Interface.

(defmacro Clines (&rest r) nil)
(defmacro defCfun (&rest r) nil)
(defmacro defentry (&rest r) nil)
(defmacro defCbody (&rest args) nil)	; Beppe
(defmacro defunC (&rest args) nil)	; Beppe

(defmacro definline (fun arg-types type code)
  `(eval-when (compile load eval)
	      ;; defCbody must go first, because it clears symbol-plist of fun
	      (defCbody ,fun ,arg-types ,type ,code)
	      (proclaim '(function ,fun ,arg-types ,type))
	      (setf (get ',fun ':inline-always)
		    '((,arg-types ,type
		       t		; side-effect-p
		       nil ,code)))))

(defmacro defla (&rest r) (cons 'defun r))

;;; Help.

(export '(help help*))

(defun help (&optional (symbol nil s))
  (if s (sys::print-doc symbol)
      (progn
        (princ "
Welcome to Ecological Common Lisp (ECL for short).
Here are the few functions you should learn first.

	(HELP symbol) prints the online documentation associated with the
	symbol.  For example, (HELP 'CONS) will print the useful information
	about the CONS function, the CONS data type, and so on.

	(HELP* string) prints the online documentation associated with those
	symbols whose print-names have the string as substring.  For example,
	(HELP* \"PROG\") will print the documentation of the symbols such as
	PROG, PROGN, and MULTIPLE-VALUE-PROG1.

	(BYE) ends the current ECL session.

For the precise language specification, refere to Guy Steele's \"Common Lisp,
the Language\" and our \"ECL Manual\".  \"ECL Dictionary\", the hard-copied
version of ECL online documentation, will be useful as a handbook.

Good luck!
")
        (values))))

(defun help* (string &optional (package (find-package "LISP")))
  (sys::apropos-doc string package))

;;; Pretty-print-formats.
;;;
;;;	The number N as the property of a symbol SYMBOL indicates that,
;;;	in the form (SYMBOL f1 ... fN fN+1 ... fM), the subforms fN+1,...,fM
;;;	are the 'body' of the form and thus are treated in a special way by
;;;	the ECL pretty-printer.

;;; (At boot we don't have setf yet)

(sys::putprop 'lambda 1 'sys::pretty-print-format)
(sys::putprop 'lambda-block 2 'sys::pretty-print-format)
(sys::putprop 'lambda-closure 4 'sys::pretty-print-format)
(sys::putprop 'lambda-block-closure 5 'sys::pretty-print-format)

(sys::putprop 'block 1 'sys::pretty-print-format)
(sys::putprop 'case 1 'sys::pretty-print-format)
(sys::putprop 'catch 1 'sys::pretty-print-format)
(sys::putprop 'ccase 1 'sys::pretty-print-format)
(sys::putprop 'clines 0 'sys::pretty-print-format)
(sys::putprop 'compiler-let 1 'sys::pretty-print-format)
(sys::putprop 'cond 0 'sys::pretty-print-format)
(sys::putprop 'ctypecase 1 'sys::pretty-print-format)
(sys::putprop 'defcfun 2 'sys::pretty-print-format)
(sys::putprop 'define-setf-method 2 'sys::pretty-print-format)
(sys::putprop 'defla 2 'sys::pretty-print-format)
(sys::putprop 'defmacro 2 'sys::pretty-print-format)
(sys::putprop 'defsetf 3 'sys::pretty-print-format)
(sys::putprop 'defstruct 1 'sys::pretty-print-format)
(sys::putprop 'deftype 2 'sys::pretty-print-format)
(sys::putprop 'defun 2 'sys::pretty-print-format)
(sys::putprop 'defunC 2 'sys::pretty-print-format) ; Beppe
(sys::putprop 'do 2 'sys::pretty-print-format)
(sys::putprop 'do* 2 'sys::pretty-print-format)
(sys::putprop 'do-symbols 1 'sys::pretty-print-format)
(sys::putprop 'do-all-symbols 1 'sys::pretty-print-format)
(sys::putprop 'do-external-symbols 1 'sys::pretty-print-format)
(sys::putprop 'dolist 1 'sys::pretty-print-format)
(sys::putprop 'dotimes 1 'sys::pretty-print-format)
(sys::putprop 'ecase 1 'sys::pretty-print-format)
(sys::putprop 'etypecase 1 'sys::pretty-print-format)
(sys::putprop 'eval-when 1 'sys::pretty-print-format)
(sys::putprop 'flet 1 'sys::pretty-print-format)
(sys::putprop 'labels 1 'sys::pretty-print-format)
(sys::putprop 'let 1 'sys::pretty-print-format)
(sys::putprop 'let* 1 'sys::pretty-print-format)
(sys::putprop 'locally 0 'sys::pretty-print-format)
(sys::putprop 'loop 0 'sys::pretty-print-format)
(sys::putprop 'macrolet 1 'sys::pretty-print-format)
(sys::putprop 'multiple-value-bind 2 'sys::pretty-print-format)
(sys::putprop 'multiple-value-prog1 1 'sys::pretty-print-format)
(sys::putprop 'prog 1 'sys::pretty-print-format)
(sys::putprop 'prog* 1 'sys::pretty-print-format)
(sys::putprop 'prog1 1 'sys::pretty-print-format)
(sys::putprop 'prog2 2 'sys::pretty-print-format)
(sys::putprop 'progn 0 'sys::pretty-print-format)
(sys::putprop 'progv 2 'sys::pretty-print-format)
(sys::putprop 'return 0 'sys::pretty-print-format)
(sys::putprop 'return-from 1 'sys::pretty-print-format)
(sys::putprop 'tagbody 0 'sys::pretty-print-format)
(sys::putprop 'the 1 'sys::pretty-print-format)
(sys::putprop 'throw 1 'sys::pretty-print-format)
(sys::putprop 'typecase 1 'sys::pretty-print-format)
(sys::putprop 'unless 1 'sys::pretty-print-format)
(sys::putprop 'unwind-protect 0 'sys::pretty-print-format)
(sys::putprop 'when 1 'sys::pretty-print-format)
(sys::putprop 'with-input-from-string 1 'sys::pretty-print-format)
(sys::putprop 'with-open-file 1 'sys::pretty-print-format)
(sys::putprop 'with-open-stream 1 'sys::pretty-print-format)
(sys::putprop 'with-output-to-string 1 'sys::pretty-print-format)
#+CLOS
(progn
(sys::putprop 'defclass 2 'sys::pretty-print-format)
(sys::putprop 'defmethod 2 'sys::pretty-print-format)
(sys::putprop 'symbol-macrolet 2 'sys::pretty-print-format)
(sys::putprop 'with-accessors 2 'sys::pretty-print-format)
(sys::putprop 'with-slots 2 'sys::pretty-print-format)
)
