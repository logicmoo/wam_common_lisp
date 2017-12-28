;;; #+BUILTIN Means to ignore since it should already be defined
;;; #+WAM-CL Means we want it
;;; #+LISP500 Means probably we dont want it
;;; #+ALT Alternative definition
;;; #+ABCL From ABCL
;;; #+SBCL From SBCL
;;; #+SICL From SICL
;;; #+SICL From SICL


(defmacro apropos-defmacro (name ll &rest body)
   `(putprop ',name 'apropos-defmacro '(defmacro ,name ,ll ,@body)))

(defmacro apropos-defun (name ll &rest body)
   `(putprop ',name 'apropos-defun '(defun ,name ,ll ,@body)))


;;; define-modify-macro.lisp
;;;
;;; Copyright (C) 2003-2005 Peter Graves
;;; $Id$
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;;
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module.  An independent module is a module which is not derived from
;;; or based on this library.  If you modify this library, you may extend
;;; this exception to your version of the library, but you are not
;;; obligated to do so.  If you do not wish to do so, delete this
;;; exception statement from your version.

;;; Adapted from SBCL.

(in-package #:system)

;; FIXME See section 5.1.3.
#+(or (and ABCL ALT) WAM-CL)
(defmacro define-modify-macro (name lambda-list function &optional doc-string)
  "Creates a new read-modify-write macro like PUSH or INCF."
  (let ((other-args nil)
	(rest-arg nil)
	(env (gensym))
	(reference (gensym)))
    ;; Parse out the variable names and &REST arg from the lambda list.
    (do ((ll lambda-list (cdr ll))
	 (arg nil))
	((null ll))
      (setq arg (car ll))
      (cond ((eq arg '&optional))
	    ((eq arg '&rest)
	     (if (symbolp (cadr ll))
		 (setq rest-arg (cadr ll))
		 (error "Non-symbol &REST arg in definition of ~S." name))
	     (if (null (cddr ll))
		 (return nil)
		 (error "Illegal stuff after &REST argument in DEFINE-MODIFY-MACRO.")))
	    ((memq arg '(&key &allow-other-keys &aux))
	     (error "~S not allowed in DEFINE-MODIFY-MACRO lambda list." arg))
	    ((symbolp arg)
	     (push arg other-args))
	    ((and (listp arg) (symbolp (car arg)))
	     (push (car arg) other-args))
	    (t (error "Illegal stuff in DEFINE-MODIFY-MACRO lambda list."))))
    (setq other-args (nreverse other-args))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (defmacro ,name (,reference ,@lambda-list &environment ,env)
        ,doc-string
        (multiple-value-bind (dummies vals newval setter getter)
            (get-setf-expansion ,reference ,env)
          (do ((d dummies (cdr d))
               (v vals (cdr v))
               (let-list nil (cons (list (car d) (car v)) let-list)))
              ((null d)
               (push (list (car newval)
                           ,(if rest-arg
                                `(list* ',function getter ,@other-args ,rest-arg)
                                `(list ',function getter ,@other-args)))
                     let-list)
               `(let* ,(nreverse let-list)
                 ,setter))))))))

#+ALT #+ABCL
(define-modify-macro incf-complex (&optional (delta 1)) +
  "The first argument is some location holding a number.  This number is
   incremented by the second argument, DELTA, which defaults to 1.")

#+ALT #+ABCL
(define-modify-macro decf-complex (&optional (delta 1)) -
  "The first argument is some location holding a number.  This number is
   decremented by the second argument, DELTA, which defaults to 1.")

#+ALT #+ABCL
(defmacro incf (place &optional (delta 1))
  (cond ((symbolp place)
         (cond ((constantp delta)
                `(setq ,place (+ ,place ,delta)))
               (t
                ;; See section 5.1.3.
                (let ((temp (gensym)))
                  `(let ((,temp ,delta))
                     (setq ,place (+ ,place ,temp)))))))
        ((and (consp place) (eq (car place) 'THE))
         (let ((res (gensym)))
           `(let ((,res (the ,(second place) (+ ,place ,delta))))
              (setf ,(third place) ,res))))
        (t
         `(incf-complex ,place ,delta))))

#+WAM-CL
(defmacro incf (place &optional (delta 1) &environment env)
  "The first argument is some location holding a number.  This number is
incremented by the second argument, DELTA, which defaults to 1."
  (if (and (symbolp (setq place (%symbol-macroexpand place env)))
           (or (constantp delta)
               (and (symbolp delta)
                    (not (nth-value 1 (%symbol-macroexpand delta env))))))
    `(setq ,place (+ ,place ,delta))
    (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-method place env)
      (let ((d (gensym)))
        `(let* (,@(mapcar #'list dummies vals)
                (,d ,delta)
                (,(car newval) (+ ,getter ,d)))
           ,setter)))))


#+WAM-CL
(defmacro decf (place &optional (delta 1))
  `(incf ,place (- 0 ,delta)))

#+ALT #+ABCL
(defmacro decf (place &optional (delta 1))
  (cond ((symbolp place)
         (cond ((constantp delta)
                `(setq ,place (- ,place ,delta)))
               (t
                ;; See section 5.1.3.
                (let ((temp (gensym)))
                  `(let ((,temp ,delta))
                     (setq ,place (- ,place ,temp)))))))
        ((and (consp place) (eq (car place) 'THE))
         (let ((res (gensym)))
           `(let ((,res (the ,(second place) (- ,place ,delta))))
              (setf ,(third place) ,res))))
        (t
         `(decf-complex ,place ,delta))))

;;; setf.lisp
;;;
;;; Copyright (C) 2003-2006 Peter Graves
;;; $Id$
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;;
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module.  An independent module is a module which is not derived from
;;; or based on this library.  If you modify this library, you may extend
;;; this exception to your version of the library, but you are not
;;; obligated to do so.  If you do not wish to do so, delete this
;;; exception statement from your version.

(in-package "SYSTEM")

#+(or ABCL WAM-CL)
(defun get-setf-method-inverse (form inverse setf-function)
  (let ((new-var (gensym))
        (vars nil)
        (vals nil))
    (dolist (x (cdr form))
      (push (gensym) vars)
      (push x vals))
    (setq vals (nreverse vals))
    (values vars vals (list new-var)
            (if setf-function
                `(,@inverse ,new-var ,@vars)
                (if (functionp (car inverse))
                    `(funcall ,@inverse ,@vars ,new-var)
                    `(,@inverse ,@vars ,new-var)))
            `(,(car form) ,@vars))))

;;; If a macro, expand one level and try again.  If not, go for the
;;; SETF function.
#+(or ABCL WAM-CL)
(defun expand-or-get-setf-inverse (form environment)
  (multiple-value-bind (expansion expanded)
      (macroexpand-1 form environment)
    (if expanded
        (get-setf-expansion expansion environment)
        (get-setf-method-inverse form `(funcall #'(setf ,(car form)))
                                 t))))

#+(or ABCL WAM-CL)
(defun get-setf-expansion (form &optional environment)
  (let (temp)
    (cond ((symbolp form)
           (multiple-value-bind (expansion expanded)
               (macroexpand-1 form environment)
             (if expanded
                 (get-setf-expansion expansion environment)
                 (let ((new-var (gensym)))
                   (values nil nil (list new-var)
                           `(setq ,form ,new-var) form)))))
          ((setq temp (get (car form) 'setf-inverse))
           (get-setf-method-inverse form `(,temp) nil))
          ((setq temp (get (car form) 'setf-expander))
           (funcall temp form environment))
          (t
           (expand-or-get-setf-inverse form environment)))))

#+(or ABCL WAM-CL)
(defmacro setf (&rest args &environment environment)
  (let ((numargs (length args)))
    (cond
     ((= numargs 2)
      (let ((place (first args))
            (value-form (second args)))
        (if (atom place)
            `(setq ,place ,value-form)
            (progn
              (multiple-value-bind (dummies vals store-vars setter getter)
                  (get-setf-expansion place environment)
                (let ((inverse (get (car place) 'setf-inverse)))
                  (if (and inverse (eq inverse (car setter)))
                      (if (functionp inverse)
                          `(funcall ,inverse ,@(cdr place) ,value-form)
                          `(,inverse ,@(cdr place) ,value-form))
                      (if (or (null store-vars) (cdr store-vars))
                          `(let* (,@(mapcar #'list dummies vals))
                             (multiple-value-bind ,store-vars ,value-form
                               ,setter))
                          `(let* (,@(mapcar #'list dummies vals)
                                    ,(list (car store-vars) value-form))
                               ,setter)))))))))
     ((oddp numargs)
      (error "Odd number of arguments to SETF."))
     (t
      (do ((a args (cddr a)) (l nil))
          ((null a) `(progn ,@(nreverse l)))
        (setq l (cons (list 'setf (car a) (cadr a)) l)))))))


;; (defsetf subseq (sequence start &optional (end nil)) (v)
;;   `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
;;      ,v))
#+(or ABCL WAM-CL)
(defun %set-subseq (sequence start &rest rest)
  (let ((end nil) v)
    (ecase (length rest)
      (1
       (setq v (car rest)))
      (2
       (setq end (car rest)
             v (cadr rest))))
    (progn
      (replace sequence v :start1 start :end1 end)
      v)))

#+(or ABCL WAM-CL)
(defun %define-setf-macro (name expander inverse doc)
  (declare (ignore doc)) ; FIXME
  (when inverse
    (put-sysprop name 'setf-inverse inverse))
  (when expander
    (put-sysprop name 'setf-expander expander))
  name)


#+(or ABCL WAM-CL)
(defmacro defsetf (access-function update-function)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (put-sysprop ',access-function 'setf-inverse ',update-function)))


#+(or ABCL WAM-CL) (flet () ;; FLET BEGIN

(defun %set-caar (x v) (set-car (car x) v))
(defun %set-cadr (x v) (set-car (cdr x) v))
(defun %set-cdar (x v) (set-cdr (car x) v))
(defun %set-cddr (x v) (set-cdr (cdr x) v))
(defun %set-caaar (x v) (set-car (caar x) v))
(defun %set-cadar (x v) (set-car (cdar x) v))
(defun %set-cdaar (x v) (set-cdr (caar x) v))
(defun %set-cddar (x v) (set-cdr (cdar x) v))
(defun %set-caadr (x v) (set-car (cadr x) v))
(defun %set-caddr (x v) (set-car (cddr x) v))
(defun %set-cdadr (x v) (set-cdr (cadr x) v))
(defun %set-cdddr (x v) (set-cdr (cddr x) v))
(defun %set-caaaar (x v) (set-car (caaar x) v))
(defun %set-cadaar (x v) (set-car (cdaar x) v))
(defun %set-cdaaar (x v) (set-cdr (caaar x) v))
(defun %set-cddaar (x v) (set-cdr (cdaar x) v))
(defun %set-caadar (x v) (set-car (cadar x) v))
(defun %set-caddar (x v) (set-car (cddar x) v))
(defun %set-cdadar (x v) (set-cdr (cadar x) v))
(defun %set-cdddar (x v) (set-cdr (cddar x) v))
(defun %set-caaadr (x v) (set-car (caadr x) v))
(defun %set-cadadr (x v) (set-car (cdadr x) v))
(defun %set-cdaadr (x v) (set-cdr (caadr x) v))
(defun %set-cddadr (x v) (set-cdr (cdadr x) v))
(defun %set-caaddr (x v) (set-car (caddr x) v))
(defun %set-cadddr (x v) (set-car (cdddr x) v))
(defun %set-cdaddr (x v) (set-cdr (caddr x) v))
(defun %set-cddddr (x v) (set-cdr (cdddr x) v))

(defsetf car set-car)
(defsetf cdr set-cdr)
(defsetf caar %set-caar)
(defsetf cadr %set-cadr)
(defsetf cdar %set-cdar)
(defsetf cddr %set-cddr)
(defsetf caaar %set-caaar)
(defsetf cadar %set-cadar)
(defsetf cdaar %set-cdaar)
(defsetf cddar %set-cddar)
(defsetf caadr %set-caadr)
(defsetf caddr %set-caddr)
(defsetf cdadr %set-cdadr)
(defsetf cdddr %set-cdddr)
(defsetf caaaar %set-caaaar)
(defsetf cadaar %set-cadaar)
(defsetf cdaaar %set-cdaaar)
(defsetf cddaar %set-cddaar)
(defsetf caadar %set-caadar)
(defsetf caddar %set-caddar)
(defsetf cdadar %set-cdadar)
(defsetf cdddar %set-cdddar)
(defsetf caaadr %set-caaadr)
(defsetf cadadr %set-cadadr)
(defsetf cdaadr %set-cdaadr)
(defsetf cddadr %set-cddadr)
(defsetf caaddr %set-caaddr)
(defsetf cadddr %set-cadddr)
(defsetf cdaddr %set-cdaddr)
(defsetf cddddr %set-cddddr)

(defsetf first set-car)
(defsetf second %set-cadr)
(defsetf third %set-caddr)
(defsetf fourth %set-cadddr)
(defun %set-fifth (x v) (set-car (cddddr x) v))
(defsetf fifth %set-fifth)
(defun %set-sixth (x v) (set-car (cdr (cddddr x)) v))
(defsetf sixth %set-sixth)
(defun %set-seventh (x v) (set-car (cddr (cddddr x)) v))
(defsetf seventh %set-seventh)
(defun %set-eighth (x v) (set-car (cdddr (cddddr x)) v))
(defsetf eighth %set-eighth)
(defun %set-ninth (x v) (set-car (cddddr (cddddr x)) v))
(defsetf ninth %set-ninth)
(defun %set-tenth (x v) (set-car (cdr (cddddr (cddddr x))) v))
(defsetf tenth %set-tenth)

(defsetf rest set-cdr)
;;Redefined in extensible-sequences-base.lisp
(defsetf elt %set-elt)
(defsetf nth %set-nth)
(defsetf svref svset)
(defsetf fill-pointer %set-fill-pointer)
(defsetf subseq %set-subseq)
(defsetf symbol-value set)
(defsetf symbol-function %set-symbol-function)
(defsetf symbol-plist %set-symbol-plist)
(defsetf get put)
(defsetf gethash puthash)
(defsetf char set-char)
(defsetf schar set-schar)
(defsetf logical-pathname-translations %set-logical-pathname-translations)
(defsetf readtable-case %set-readtable-case)

(defsetf function-info %set-function-info)

(defsetf stream-external-format %set-stream-external-format)

(defsetf structure-ref structure-set)

) ;; FLET END


;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                           sequence routines

(in-package "SYSTEM")

#+ecl-min
(eval-when (:execute)
  (load (merge-pathnames "seqmacros.lsp" *load-truename*)))


#+(or WAM-CL ECL) 
(defun error-not-a-sequence (value)
  ; (declare (c-local))
  (signal-type-error value 'sequence))

#+(or WAM-CL ECL) 
(defun error-sequence-index (sequence index)
  ; (declare (c-local))
  (error 'simple-type-error
         :datum index
         :expected-type 'unsigned-byte
         :format-control "Not a valid index ~A into sequence ~A"
         :format-arguments (list index sequence)))

#+(or WAM-CL ECL) 
(defun error-sequence-type (type)
  ; (declare (c-local))
  (error 'simple-type-error
         :datum (vector) ;; Any sequence object will do, because it does not belong to TYPE
         :expected-type type
         :format-control "~S does not specify a sequence type"
         :format-arguments (list type)))

#+(or WAM-CL ECL) 
(defun error-sequence-length (object type size)
  ; (declare (c-local))
  (error 'simple-type-error
         :format-control
         "Cannot create a sequence of size ~S which matches type ~S."
         :format-arguments (list size type)
         :expected-type type
         :datum object))

				 
#+(or WAM-CL ECL) 
(defun make-sequence (type size &key (initial-element nil iesp) &aux sequence)
  "Args: (type length &key initial-element)
Creates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-
ELEMENT is given, then it becomes the elements of the created sequence.  The
default value of INITIAL-ELEMENT depends on TYPE."
  (multiple-value-bind (element-type length)
      (closest-sequence-type type)
    (cond ((eq element-type 'LIST)
           (setq sequence (make-list size :initial-element initial-element))
           (unless (subtypep 'LIST type)
             (when (or (and (subtypep type 'NULL) (plusp size))
                       (and (subtypep type 'CONS) (zerop size)))
               (error-sequence-length (make-list size :initial-element initial-element) type 0))))
          (t
           (setq sequence (sys:make-vector (if (eq element-type '*) T element-type)
                                           size nil nil nil nil))
           (when iesp
             (fill-array-with-elt sequence initial-element 0 nil))
           (unless (or (eql length '*) (eql length size))
             (error-sequence-length sequence type size))))
    sequence))

#+(or WAM-CL ECL) 
(defun make-seq-iterator (sequence &optional (start 0))
  (declare (optimize (safety 0)))
  (cond ((fixnump start)
         (let ((aux start))
           (declare (fixnum aux))
           (cond ((minusp aux)
                  (error-sequence-index sequence start))
                 ((listp sequence)
                  (nthcdr aux sequence))
                 ((vectorp sequence)
                  (and (< start (length (truly-the vector sequence)))
                       start))
                 (t
                  (error-not-a-sequence sequence)))))
        ((not (or (listp sequence) (vectorp sequence)))
         (error-not-a-sequence sequence))
        ((integerp start)
         nil)
        (t
         (error-sequence-index sequence start))))



#|
TODO PORT

#+(or WAM-CL ECL) 
(defun closest-sequence-type (type)
  (let (elt-type length name args)
    (cond ((consp type)
           (setq name (first type) args (cdr type)))
          ((si::instancep type)
           (setf name (class-name (truly-the class type)) args nil))
          (t
           (setq name type args nil)))
    (case name
      ((LIST)
       ;; This is the only descriptor that does not match a real
       ;; array type.
       (setq elt-type 'LIST length '*))
      ((VECTOR)
       (setq elt-type (if (endp args) 'T (first args))
             length (if (endp (rest args)) '* (second args))))
      ((SIMPLE-VECTOR)
       (setq elt-type 'T
             length (if (endp args) '* (first args))))
      #-unicode
      ((STRING SIMPLE-STRING)
       (setq elt-type 'BASE-CHAR
             length (if (endp args) '* (first args))))
      #+unicode
      ((BASE-STRING SIMPLE-BASE-STRING)
       (setq elt-type 'BASE-CHAR
             length (if (endp args) '* (first args))))
      #+unicode
      ((STRING SIMPLE-STRING)
       (setq elt-type 'CHARACTER
             length (if (endp args) '* (first args))))
      ((BIT-VECTOR SIMPLE-BIT-VECTOR)
       (setq elt-type 'BIT
             length (if (endp args) '* (first args))))
      ((ARRAY SIMPLE-ARRAY)
       (let ((dimension-spec (second args)))
         (cond
           ((eql dimension-spec 1)
            (setf length '*))
           ((and (consp dimension-spec)
                 (null (cdr dimension-spec)))
            (setf length (car dimension-spec)))
           (T (error-sequence-type type))))
       (setq elt-type (upgraded-array-element-type (first args))))
      (t
       ;; We arrive here when the sequence type is not easy to parse.
       ;; We give up trying to guess the length of the sequence.
       ;; Furthermore, we also give up trying to find if the element
       ;; type is *. Instead we just compare with some specialized
       ;; types and otherwise fail.
       (dolist (i '((NIL . NIL)
                    (LIST . LIST)
                    (STRING . CHARACTER)
                    . #.(mapcar #'(lambda (i) `((VECTOR ,i) . ,i)) +upgraded-array-element-types+)
		  ))
                (if (subtypep type 'vector)
                    ;; Does this have to be a type-error?
                    ;; 17.3 for MAKE-SEQUENCE says it should be an error,
                    ;; but does not specialize what kind.
                    (error "Cannot find the element type in vector type ~S" type)
                    (error-sequence-type type))
          (when (subtypep type (car i))
            (setq elt-type (cdr i) length '*)
            ;; The (NIL . NIL) case above
            (unless elt-type
              (error-sequence-type type))
            (return)))))
    (values elt-type length)))
|#

#+(or WAM-CL ECL) 
(defun seq-iterator-ref (sequence iterator)
  (declare (optimize (safety 0)))
  (if (fixnump iterator)
      (aref (truly-the vector sequence) iterator)
      (car (truly-the cons iterator))))

#+(or WAM-CL ECL) 
(defun seq-iterator-set (sequence iterator value)
  (declare (optimize (safety 0)))
  (if (fixnump iterator)
      (setf (aref (truly-the vector sequence) iterator) value)
      (setf (car (truly-the cons iterator)) value)))

#+(or WAM-CL ECL) 
(defun seq-iterator-next (sequence iterator)
  (declare (optimize (safety 0)))
  (cond ((fixnump iterator)
         (let ((aux (1+ iterator)))
           (declare (fixnum aux))
           (and (< aux (length (truly-the vector sequence)))
                aux)))
        ((atom iterator)
         (error-not-a-sequence iterator))
        (t
         (setf iterator (cdr (truly-the cons iterator)))
         (unless (listp iterator)
           (error-not-a-sequence iterator))
         iterator)))

#+(or WAM-CL ECL) 
(defun seq-iterator-list-pop (values-list seq-list iterator-list)
  (declare (optimize (safety 0)))
  (do* ((it-list iterator-list)
        (v-list values-list))
       ((null v-list)
        values-list)
    (let* ((it (cons-car it-list))
           (sequence (cons-car seq-list)))
      (cond ((null it)
             (return nil))
            ((fixnump it)
             (let* ((n it) (s sequence))
               (declare (fixnum n) (vector s))
               (rplaca v-list (aref s n))
               (rplaca it-list (and (< (incf n) (length s)) n))))
            ((atom it)
             (error-not-a-sequence it))
            (t
             (rplaca v-list (cons-car it))
             (unless (listp (setf it (cons-cdr it)))
               (error-not-a-sequence it))
             (rplaca it-list it)))
      (setf v-list (cons-cdr v-list)
            it-list (cons-cdr it-list)
            seq-list (cons-cdr seq-list)))))

#+(or WAM-CL ECL) 
(defun coerce-to-list (object)
  (if (listp object)
      object
      (do ((it (make-seq-iterator object) (seq-iterator-next object it))
           (output nil))
          ((null it) (nreverse output))
        (push (seq-iterator-ref object it) output))))

#+(or WAM-CL ECL) 
(defun coerce-to-vector (object elt-type length simple-array-p)
  (let ((output object))
    (unless (and (vectorp object)
                 (or (null simple-array-p) (simple-array-p object))
                 (eq (array-element-type object) elt-type))
      (let* ((final-length (if (eq length '*) (length object) length)))
        (setf output (make-vector elt-type final-length nil nil nil 0))
        (do ((i (make-seq-iterator object) (seq-iterator-next output i))
             (j 0 (truly-the index (1+ j))))
            ((= j final-length)
             (setf object output))
          (declare (index j))
          (setf (aref output j) (seq-iterator-ref object i)))))
    (unless (eq length '*)
      (unless (= length (length output))
        (check-type output `(vector ,elt-type (,length)) "coerced object")))
    output))

#+(or WAM-CL ECL) 
(defun concatenate (result-type &rest sequences)
  "Args: (type &rest sequences)
Returns a new sequence of the specified type, consisting of all elements of
SEQUENCEs."
  (do* ((length-list (mapcar #'length sequences) (rest length-list))
        (output (make-sequence result-type (apply #'+ length-list)))
        (sequences sequences (rest sequences))
        (i (make-seq-iterator output)))
      ((null sequences) output)
    (do* ((s (first sequences))
          (j (make-seq-iterator s) (seq-iterator-next s j)))
         ((null j))
      (seq-iterator-set output i (seq-iterator-ref s j))
      (setq i (seq-iterator-next output i)))))


#+(or WAM-CL ECL) 
(defun map (result-type function sequence &rest more-sequences)
  "Args: (type function sequence &rest more-sequences)
Creates and returns a sequence of TYPE with K elements, with the N-th element
being the value of applying FUNCTION to the N-th elements of the given
SEQUENCEs, where K is the minimum length of the given SEQUENCEs."
  (let* ((sequences (list* sequence more-sequences))
         (function (coerce-to-function function))
         output
         it)
    (when result-type
      (let ((l (length sequence)))
        (when more-sequences
          (setf l (reduce #'min more-sequences
                          :initial-value l
                          :key #'length)))
        (setf output (make-sequence result-type l)
              it (make-seq-iterator output))))
    (do-sequences (elt-list sequences :output output)
      (let ((value (apply function elt-list)))
        (when result-type
          (seq-iterator-set output it value)
          (setf it (seq-iterator-next output it)))))))

#+(or WAM-CL ECL) 
(defun some (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;
NIL otherwise."
  (reckless
   (do-sequences (elt-list (cons sequence more-sequences) :output nil)
     (let ((x (apply predicate elt-list)))
       (when x (return x))))))

#+(or WAM-CL ECL) 
(defun every (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise."
  (reckless
   (do-sequences (elt-list (cons sequence more-sequences) :output t)
     (unless (apply predicate elt-list)
       (return nil)))))



#-WAM-CL #+ECL
(def-seq-bool-parser notany
  "Args: (predicate sequence &rest more-sequences)
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise."
  (when that-value (return nil))
  t)

#-WAM-CL #+ECL
(def-seq-bool-parser notevery
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise."
  (unless that-value (return t))
  nil)
 
#+(or WAM-CL ECL) 
(defun every* (predicate &rest sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE and all sequences
have the same length; NIL otherwise."
  (and (apply #'= (mapcar #'length sequences))
       (apply #'every predicate sequences)))

#+(or WAM-CL ECL) 
(defun notany (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise."
  (not (apply #'some predicate sequence more-sequences)))

#+(or WAM-CL ECL) 
(defun notevery (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise."
  (not (apply #'every predicate sequence more-sequences)))

#+(or WAM-CL ECL) 
(defun map-into (result-sequence function &rest sequences)
"Fills the output sequence with the values returned by applying FUNCTION to the
elements of the given sequences. The i-th element of RESULT-SEQUENCE is the output
of applying FUNCTION to the i-th element of each of the sequences. The map routine
stops when it reaches the end of one of the given sequences."
  (let ((nel (apply #'min (if (vectorp result-sequence)
                              (array-dimension result-sequence 0)
                              (length result-sequence))
                    (mapcar #'length sequences))))
    (declare (fixnum nel))
    ;; Set the fill pointer to the number of iterations
    (when (and (vectorp result-sequence)
               (array-has-fill-pointer-p result-sequence))
      (setf (fill-pointer result-sequence) nel))
    ;; Perform mapping
    (do ((ir (make-seq-iterator result-sequence) (seq-iterator-next result-sequence ir))
         (it (mapcar #'make-seq-iterator sequences))
         (val (make-sequence 'list (length sequences))))
        ((null ir) result-sequence)
      (do ((i it (cdr i))
           (v val (cdr v))
           (s sequences (cdr s)))
          ((null i))
        (unless (car i) (return-from map-into result-sequence))
        (rplaca v (seq-iterator-ref (car s) (car i)))
        (rplaca i (seq-iterator-next (car s) (car i))))
      (seq-iterator-set result-sequence ir (apply function val)))))


;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1995, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;                        list manipulating routines

(in-package "SYSTEM")


#+(or WAM-CL ECL)
(defun union (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns, as a list, the union of elements in LIST1 and in LIST2."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last list2))
       (or first list2))
    (unless (member1 (car x) list2 test test-not key)
      (if last
          (progn (rplacd last (cons (car x) nil))
                 (setq last (cdr last)))
          (progn (setq first (cons (car x) nil))
                 (setq last first))))))


#+(or WAM-CL ECL) 
(defun nunion (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive UNION.  Both LIST1 and LIST2 may be destroyed."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last list2))
       (or first list2))
    (unless (member1 (car x) list2 test test-not key)
      (if last
          (rplacd last x)
          (setq first x))
      (setq last x))))


#+(or WAM-CL ECL) 
(defun intersection (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns a list consisting of those objects that are elements of both LIST1 and
LIST2."
  (do ((x list1 (cdr x))
       (ans))
      ((null x)
       (nreverse ans)) ; optional nreverse: not required by CLtL
    (when (member1 (car x) list2 test test-not key)
        (push (car x) ans))))


#+(or WAM-CL ECL) 
(defun nintersection (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive INTERSECTION.  Only LIST1 may be destroyed."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last nil))
       first)
    (when (member1 (car x) list2 test test-not key)
      (if last
          (rplacd last x)
          (setq first x))
      (setq last x))))


#+(or WAM-CL ECL) 
(defun set-difference (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns, as a list, those elements of LIST1 that are not elements of LIST2."
  (do ((x list1 (cdr x))
       (ans))
      ((null x) (nreverse ans))
    (unless (member1 (car x) list2 test test-not key)
      (push (car x) ans))))


#+(or WAM-CL ECL) 
(defun nset-difference (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive SET-DIFFERENCE.  Only LIST1 may be destroyed."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last nil))
       first)
    (unless (member1 (car x) list2 test test-not key)
      (if last
          (rplacd last x)
          (setq first x))
      (setq last x))))


#+(or WAM-CL ECL) 
(defun swap-args (f)
  ; (declare (c-local))
  (and f #'(lambda (x y) (funcall f y x))))


#+(or WAM-CL ECL) 
(defun set-exclusive-or (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns, as a list, those elements of LIST1 that are not elements of LIST2 and
those elements of LIST2 that are not elements of LIST1."
  (nconc (set-difference list1 list2 :test test :test-not test-not :key key)
         (set-difference list2 list1 :test (swap-args test) :test-not (swap-args test-not) :key key)))


#+(or WAM-CL ECL) 
(defun nset-exclusive-or (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive SET-EXCLUSIVE-OR.  Both LIST1 and LIST2 may be destroyed."
  (nconc (set-difference list1 list2 :test test :test-not test-not :key key)
         (nset-difference list2 list1 :test (swap-args test) :test-not (swap-args test-not) :key key)))


#+(or WAM-CL ECL) 
(defun subsetp (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns T if every element of LIST1 is also an element of LIST2.  Returns NIL
otherwise."
  (do ((l list1 (cdr l)))
      ((null l) t)
    (unless (member1 (car l) list2 test test-not key)
      (return nil))))


#+(or WAM-CL ECL) 
(defun rassoc-if (test alist &key key)
  "Returns the first pair in ALIST whose cdr satisfies TEST. Returns NIL if no
such pair exists."
  (rassoc test alist :test #'funcall :key key))

#+(or WAM-CL ECL) 
(defun rassoc-if-not (test alist &key key)
  "Returns the first pair in ALIST whose cdr does not satisfy TEST.  Returns NIL
if no such pair exists."
  (rassoc test alist :test-not #'funcall :key key))


#+(or WAM-CL ECL) 
(defun assoc-if (test alist &key key)
  "Returns the first pair in ALIST whose car satisfies TEST.  Returns NIL if no
such pair exists."
  (assoc test alist :test #'funcall :key key))

#+(or WAM-CL ECL) 
(defun assoc-if-not (test alist &key key)
  "Returns the first pair in ALIST whose car does not satisfy TEST.  Returns NIL
if no such pair exists."
  (assoc test alist :test-not #'funcall :key key))


#+(or WAM-CL ECL) 
(defun member-if (test list &key key)
  "Searches LIST for an element that satisfies TEST.  If found, returns the
sublist of LIST that begins with the element.  If not found, returns NIL."
  (member test list :test #'funcall :key key))

#+(or WAM-CL ECL) 
(defun member-if-not (test list &key key)
  "Searches LIST for an element that does not satisfy TEST.  If found, returns
the sublist of LIST that begins with the element.  If not found, returns NIL."
  (member test list :test-not #'funcall :key key))


#+(or WAM-CL ECL) 
(defun subst-if (new test tree &key key)
  "Substitutes NEW for subtrees of TREE that satisfy TEST and returns the result.
The original TREE is not destroyed."
  (subst new test tree :test #'funcall :key key))

#+(or WAM-CL ECL) 
(defun subst-if-not (new test tree &key key)
  "Substitutes NEW for subtrees of TREE that do not satisfy TEST and returns the
result.  The original TREE is not destroyed."
  (subst new test tree :test-not #'funcall :key key))


#+(or WAM-CL ECL) 
(defun nsubst-if (new test tree &key key)
  "Destructive SUBST-IF. TREE may be modified."
  (nsubst new test tree :test #'funcall :key key))

#+(or WAM-CL ECL) 
(defun nsubst-if-not (new test tree &key key)
  "Destructive SUBST-IF-NOT. TREE may be modified."
  (nsubst new test tree :test-not #'funcall :key key))




#+BUILTIN 
#+(or WAM-CL ECL) 
(funcall #'(setf macro-function)
	 #'(lambda (name lambda-list &rest body)
	     (list 'progn
		   (list 'funcall '#'(setf macro-function)
			 (list 'function
			       (cons 'lambda (cons lambda-list body)))
			 (list 'quote name))
		   (list 'quote name)))
	 'defmacro)



#+(or WAM-CL ECL) 
(defmacro defun500 (name lambda-list &rest body)
  (list 'progn
	(list 'funcall '#'(setf fdefinition)
	      (list 'function
		    (list 'lambda lambda-list
			  (cons 'block (cons (if (consp name)
						 (car (cdr name))
						 name)
					     body))))
	      (list 'quote name))
	(list 'quote name)))

#+(or WAM-CL ECL) 
#+BUILTIN
(defmacro setf (place new-value)
  (if (consp place)
      (cons 'funcall (cons (list 'function (list 'setf (car place)))
			   (cons new-value (cdr place))))
      (list 'setq place new-value)))


#+WAM-CL
#+(or WAM-CL ECL) 
(defun append (&rest lists)
  (if (cdr lists)
      (let ((list (car lists))
	    (result nil)
	    (end nil))
	(if list
	    (tagbody
	     start
	       (if list
		   (progn
		     (setf end (if end
				   (setf (cdr end) (list (car list)))
				   (setf result (list (car list)))))
		     (setf list (cdr list))
		     (go start)))
	       (setf (cdr end) (apply #'append (cdr lists)))
	       (return-from append result))
	    (apply #'append (cdr lists))))
      (car lists)))


#+(or WAM-CL ECL) 
(defparameter *type-expanders* nil)

#+(or WAM-CL ECL) 
(defconstant call-arguments-limit 65536)

#+(or WAM-CL ECL) 
(defconstant lambda-parameters-limit 65536)

#+(or WAM-CL ECL) 
(defconstant multiple-values-limit 65536)

#+(or WAM-CL ECL)
(defconstant lambda-list-keywords
  '(&allow-other-keys &aux &body &environment &key &optional &rest &whole))

(apropos-defmacro psetq (&rest rest)
  (let ((inits nil)
	(sets nil)
	(list rest))
    (tagbody
     start
       (when (cddr list)
	 (push (list (gensym) (cadr list)) inits)
	 (setq list (cddr list))
	 (go start)))
    (setq list inits)
    (tagbody
     start
       (when (cddr rest)
	 (push (caar list) sets)
	 (push (car rest) sets)
	 (setq list (cdr list))
	 (setq rest (cddr rest))
	 (go start)))
    `(let ,(reverse inits)
      (setq ,@sets ,@rest))))


(apropos-defmacro return (&optional result)
  `(return-from nil ,result))

(apropos-defmacro when (test-form &rest forms)
  `(if ,test-form (progn ,@forms)))

(apropos-defmacro unless (test-form &rest forms)
  `(if (not ,test-form) (progn ,@forms)))

(apropos-defmacro and (&rest forms)
  (if forms
      (if (cdr forms)
	  `(when ,(car forms) (and ,@(cdr forms)))
	(car forms))
    `t))

(apropos-defmacro or (&rest forms)
  (if forms
      (if (cdr forms)
	  (let ((temp (gensym)))
	    `(let ((,temp ,(car forms)))
	      (if ,temp
		  ,temp
		(or ,@(cdr forms)))))
	(car forms))
    `nil))

(apropos-defmacro cond (&rest clauses)
  (when clauses
    (if (cdar clauses)
	`(if ,(caar clauses)
	     (progn ,@(cdar clauses))
	     (cond ,@(cdr clauses)))
	`(or ,(caar clauses)
	     (cond ,@(cdr clauses))))))


(apropos-defmacro case (keyform &rest clauses)
  (let ((temp (gensym)))
    (labels ((recur (clauses)
	       (when clauses
		 (if (member (caar clauses) '(otherwise t))
		     `(progn ,@(cdar clauses))
		     `(if ,(if (listp (caar clauses))
			       `(member ,temp ',(caar clauses))
			       `(eql ,temp ',(caar clauses)))
		          (progn ,@(cdar clauses))
		          ,(recur (cdr clauses)))))))
      `(let ((,temp ,keyform))
	,(recur clauses)))))


(apropos-defmacro ecase (keyform &rest clauses)
  (let ((temp (gensym)))
    `(let ((,temp ,keyform))
      (case ,temp ,@clauses
	    (error 'type-error :datum ,temp
		   :expected-type `(member ,@(mapcan #'(lambda (x)
							 (if (listp (car x))
							     (car x)
							     (list (car x))))
						     clauses)))))))

(apropos-defmacromultiple-value-bind (vars values-form &rest forms)
  `(multiple-value-call #'(lambda (&optional ,@vars &rest ,(gensym))
			    ,@forms)
                        ,values-form))

(apropos-defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))

(apropos-defun values-list (list) (apply #'values list))

#+(or WAM-CL LISP500)
(defmacro nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))

(apropos-defmacro prog (inits &rest forms)
  `(block nil
    (let ,inits
      (tagbody ,@forms))))
(apropos-defmacro prog* (inits &rest forms)
  `(block nil
    (let* ,inits
      (tagbody ,@forms))))
(apropos-defmacro prog1 (first-form &rest forms)
  (let ((temp (gensym)))
    `(let ((,temp ,first-form))
      ,@forms
      ,temp)))
(apropos-defmacro prog2 (first-form second-form &rest forms)
  (let ((temp (gensym)))
    `(progn
      ,first-form
      (let ((,temp ,second-form))
	,@forms
	,temp))))

#+BUILTIN
#+(or WAM-CL LISP500)
(defun equal (a b)
  (or (eql a b)
      (cond
	((not a) nil)
	((consp a) (and (consp b)
			(equal (car a) (car b))
			(equal (cdr a) (cdr b))))
	((stringp a) (and (stringp b)
			  (string= a b)))
	((bit-vector-p a) (and (bit-vector-p b)
			       (= (length a) (length b))
			       (dotimes (i (length a) t)
				 (when (/= (aref a i) (aref b i))
				   (return))))))))

#+(or WAM-CL LISP500)
(defun identity (object) object)

#+(or WAM-CL LISP500)
(defun complement (function)
  #'(lambda (&rest rest) (not (apply function rest))))

#+(or WAM-CL LISP500)
(defun constantly (value) #'(lambda (&rest rest) value))



(apropos-defmacro dotimes ((var count-form &optional result-form) &rest forms)
  (let ((start (gensym))
	(count (gensym)))
    `(block nil
      (let ((,var 0)
	    (,count ,count-form))
	(tagbody
	   ,start
	   (when (< ,var ,count)
	     ,@forms
	     (incf ,var)
	     (go ,start)))
	,result-form))))



(apropos-defmacro do (vars (end-test-form &rest result-forms) &rest forms)
  (let ((start (gensym))
	(inits nil)
	(steps nil))
  `(block nil
    (let ,(dolist (var vars (reverse inits))
	    (push (if (consp var)
		      (list (car var) (cadr var))
		      (list var)) inits))
      (tagbody
	 ,start
	 (if ,end-test-form (return (progn ,@result-forms)))
	 ,@forms
	 ,@(dolist (var vars (when steps `((psetq ,@(reverse steps)))))
	     (when (and (consp var) (cddr var))
	       (push (car var) steps)
	       (push (caddr var) steps)))
	 (go ,start))))))
(apropos-defmacro do* (vars (end-test-form &rest result-forms) &rest forms)
  (let ((start (gensym))
	(inits nil)
	(steps nil))
  `(block nil
    (let* ,(dolist (var vars (reverse inits))
	     (push (if (consp var)
		       (list (car var) (cadr var))
		       (list var)) inits))
      (tagbody
	 ,start
	 (if ,end-test-form (return (progn ,@result-forms)))
	 ,@forms
	 ,@(dolist (var vars (when steps `((setq ,@(reverse steps)))))
	     (when (and (consp var) (cddr var))
	       (push (car var) steps)
	       (push (caddr var) steps)))
	 (go ,start))))))



(apropos-defmacro dolist ((var list-form &optional result-form) &rest forms)
  (let ((start (gensym))
	(list (gensym)))
    `(block nil
      (let ((,list ,list-form)
	    (,var nil))
	(tagbody
	   ,start
	   (unless ,list
	     (setf ,var nil)
	     (return-from nil ,result-form))
	   (setf ,var (car ,list))
	   (setf ,list (cdr ,list))
	   ,@forms
	   (go ,start))))))





#+(or WAM-CL LISP500) 
(defmacro check-type (place typespec &optional string)
  `(tagbody
    start
    (unless (typep ,place ',typespec)
      (restart-case
	  (error 'type-error :datum ,place :expected-type ',typespec)
	(store-value (value)
	  (setf ,place value)))
      (go start))))


#+(or WAM-CL LISP500)
(defun designator-condition (default-type datum arguments)
  (if (symbolp datum)
      (apply #'make-condition datum arguments)
      (if (or (stringp datum) (functionp datum))
	  (make-condition default-type
			  :format-control datum
			  :format-arguments arguments)
	  datum)))

#+(or WAM-CL LISP500) 
(defun error (datum &rest arguments)
  (let ((condition (designator-condition 'simple-error datum arguments)))
    (when (typep condition *break-on-signals*)
      (invoke-debugger condition))
    (invoke-handler condition)
    (invoke-debugger condition)))

#+(or WAM-CL LISP500) 
(defun cerror (continue-format-control datum &rest arguments)
  `(with-simple-restart (continue continue-format-control)
    (apply #'error datum arguments)))

#+(or WAM-CL LISP500) 
(defun signal (datum &rest arguments)
  (let ((condition (designator-condition 'simple-condition datum arguments)))
    (when (typep condition *break-on-signals*)
      (invoke-debugger condition))
    (invoke-handler condition)
    nil))

#+(or WAM-CL LISP500) 
(defun warn (datum &rest arguments)
  (restart-case
      (let ((warning (if (symbolp datum)
			 (apply #'make-condition 'warning datum arguments)
			 datum)))
	(signal warning)
	(print-object warning *error-output*))
    (muffle-warning () nil))
  nil)



#+(or WAM-CL LISP500) 
(defun invoke-debugger (condition)
  (let ((debugger-hook *debugger-hook*)
	(*debugger-hook* nil))
    (when debugger-hook
      (funcall debugger-hook condition debugger-hook))
    (format *debug-io* "Entering debugger.~%")
    (princ condition *debug-io*)
    (terpri *debug-io*)
    (let ((restarts (compute-restarts condition))
	  (stack (makef))
	  (frame-depth 0)
	  (active-frame nil))
      (let ((count 0))
	(dolist (restart restarts)
	  (format *debug-io* "~A: " count)
	  (princ restart *debug-io*)
	  (terpri *debug-io*)
	  (incf count)))
      (setq active-frame (next-function-frame (- stack 20)))
      (show-frame active-frame 0)
      (tagbody
       start
	 (format *debug-io* ";~A> " frame-depth)
	 (let ((form (read)))
	   (case form
	     (:help (format *debug-io* "Type :help to get help.~%")
		    (format *debug-io* "Type :continue <index> to invoke the indexed restart.~%"))
	     (:back (do ((frame (next-function-frame (- stack 20))
				(next-function-frame frame))
			 (index 0 (+ 1 index)))
			((not frame))
		      (show-frame frame index)))
	     (:up (if (plusp frame-depth)
		      (progn
			(decf frame-depth)
			(do ((frame (next-function-frame (- stack 20))
				    (next-function-frame frame))
			     (index 0 (+ 1 index)))
			    ((= index frame-depth) (setq active-frame frame)))
			(show-frame active-frame frame-depth))
		      (format *debug-io* "Top of stack.~%")))
	     (:down (let ((frame (next-function-frame active-frame)))
		      (if frame
			  (progn
			    (incf frame-depth)
			    (setq active-frame frame)
			    (show-frame active-frame frame-depth))
			  (format *debug-io* "Bottom of stack.~%"))))
	     (:locals (do ((env (fref (- active-frame 1)) (cdr env)))
			  ((not env))
			(when (symbolp (caar env))
			  (format *debug-io* "~A~%" (caar env)))))
	     (:continue (let ((index (read)))
			  (invoke-restart-interactively (nth index restarts))))
	     (t (let ((values (multiple-value-list
			       (eval form (fref (- active-frame 1)))))
		      (count 0))
		  (if values
		      (dolist (value values)
			(format *debug-io* ";~A: ~S~%" count value)
			(incf count))
		      (format *debug-io* ";No values.~%")))))
	   (go start))))))




#+(or WAM-CL LISP500) 
(defun break (&optional format-control &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (let ((*debugger-hook* nil))
      (invoke-debugger (make-condition 'simple-condition
				       :format-control format-control
				       :format-arguments format-arguments))))
  nil)



#+(or WAM-CL LISP500) 
(defparameter *debugger-hook* nil)

#+(or WAM-CL LISP500) 
(defparameter *break-on-signals* nil)

#+(or WAM-CL LISP500) 
(defparameter *handlers* nil)

#+(or WAM-CL LISP500) 
(defun invoke-handler (condition)
  (dolist (handler *handlers*)
    (when (typep condition (car handler))
      (setq *handlers* (caddr handler))
      (funcall (cadr handler) condition))))


#+(or WAM-CL LISP500) 
(defmacro handler-bind (bindings &rest forms)
  (let ((form '*handlers*)
	(handlers (gensym)))
    (dolist (binding (reverse bindings))
      (setq form
	    `(cons (list ',(car binding) ,(cadr binding) ',handlers) ,form)))
    `(let ((handlers *handlers*)
	   (*handlers* ,form))
      ,@forms)))

#+(or WAM-CL LISP500) 
(defmacro handler-case (expression &rest clauses)
  (let ((tag (gensym))
	(bindings nil))
    `(handler-bind
      ,(dolist (clause clauses (reverse bindings))
	 (let ((typespec (car clause))
	       (var-list (cadr clause))
	       (forms (cddr clauses)))
	   (push `(typespec #'(lambda (,(if var-list (car var-list) (gensym)))
				(return-from tag (progn ,@forms))))
		 bindings)))
      ,expression)))

#+(or WAM-CL LISP500) 
(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
    (error (condition) (values nil condition))))

#+(or WAM-CL LISP500) 
(defparameter *restarts* nil)

#+(or WAM-CL LISP500) 
(defun compute-restarts (&optional condition)
  "FIXME restarts associated with conditions"
  (if condition
      *restarts*
      *restarts*))

#+(or WAM-CL LISP500) 
(defun find-restart (identifier &optional condition)
  (dolist (restart *restarts*)
    (when (eq restart identifier)
      (return restart))
    (when (eq (restart-name restart) identifier)
      (return restart))))



#+(or WAM-CL LISP500) 
(defun designator-restart (designator)
  (if (restartp designator)
      designator
      (dolist (restart *restarts* (error 'type-error :datum designator
					 :expected-type 'restart))
	(when (eq (restart-name restart) designator)
	  (return restart)))))


#+(or WAM-CL LISP500) 
(defun invoke-restart (restart &rest arguments)
  (setq restart (designator-restart restart))
  (apply (restart-function restart) arguments))

#+(or WAM-CL LISP500) 
(defun invoke-restart-interactively (restart)
  (setq restart (designator-restart restart))
  (apply (restart-function restart)
	 (funcall (restart-interactive-function restart))))

#+(or WAM-CL LISP500) 
(defmacro restart-bind (restart-bindings &rest forms)
  (let ((form '*restarts*))
    (dolist (binding (reverse restart-bindings))
      (setq form
	    `(cons (make-restart ',(car binding) ,@(cdr binding)) ,form)))
    `(let ((*restarts* ,form))
      ,@forms)))


#+(or WAM-CL LISP500) 
(defmacro restart-case (restartable-form &rest clauses)
  (let ((catch-tag (gensym))
	(bindings nil))
    `(catch ',catch-tag
      (restart-bind
	  ,(dolist (clause clauses (reverse bindings))
	     (let ((name (car clause))
		   (lambda-list (cadr clause))
		   (rest (cddr clause))
		   (interactive '#'(lambda () nil))
		   (report '#'(lambda (stream)
				(format stream "~A" (car clause))))
		   (test '#'(lambda (condition) t)))
	       (tagbody
		start
		  (when (member (car rest) '(:interactive :report :test))
		    (let ((value (cadr rest)))
		      (case (car rest)
			(:interactive (setq interactive `(function ,value)))
			(:report (setq report
				       (if (stringp value)
					   `#'(lambda (stream)
						(write-string ,value stream))
					   `(function ,value))))
			(:test (setq test `(function ,value)))))
		    (setq rest (cddr rest))
		    (go start)))
	       (push `(,(car clause)
		       #'(lambda ,(cadr clause)
			   (throw ',catch-tag (progn ,@rest)))
		       :interactive-function ,interactive
		       :report-function ,report
		       :test-function ,test)
		     bindings)))
	,restartable-form))))



#+(or WAM-CL LISP500) 
(defmacro with-simple-restart ((name format-control &rest format-arguments)
			       &rest forms)
  (let ((tag (gensym)))
    `(block ,tag
      (restart-bind
	  ((,name
	    #'(lambda () (return-from ,tag (values nil t)))
	     :interactive-function #'(lambda () nil)
	     :report-function #'(lambda (stream)
				  (apply #'format stream ',format-control
					 ',format-arguments))
	     :test-function #'(lambda () t)))
	,@forms))))



#+(or WAM-CL LISP500) 
(defun abort (&optional condition)
  (invoke-restart (find-restart 'abort condition))
  (error 'control-error))

#+(or WAM-CL LISP500) 
(defun continue (&optional condition)
  (invoke-restart (find-restart 'continue condition)))

#+(or WAM-CL LISP500) 
(defun muffle-warning (&optional condition)
  (invoke-restart (find-restart 'muffle-warning condition))
  (error 'control-error))

#+(or WAM-CL LISP500) 
(defun store-value (value &optional condition)
  (invoke-restart (find-restart 'store-value condition) value))

#+(or WAM-CL LISP500) 
(defun use-value (value &optional condition)
  (invoke-restart (find-restart 'use-value condition) value))


#+(or WAM-CL LISP500) 
(defun integer-string (integer &optional (radix 10))
  (if (= integer 0)
      "0"
      (labels ((recur (i l)
		 (if (= i 0)
		     l
		     (multiple-value-bind (ni r)
			 (floor i radix)
		       (recur ni (cons (code-char (+ (if (< r 10) 48 55) r))
				       l))))))
	(apply #'string (if (< 0 integer)
			    (recur integer nil)
			    (cons (code-char 45) (recur (- integer) nil)))))))



#+(or WAM-CL LISP500)
(defun designator-symbol (designator)
  (if (symbolp designator)
      designator
      (find-symbol designator)))

#+BUILTIN 
(defun symbolp (object) (or (null object) (eq (type-of object) 'symbol)))

#+BUILTIN 
(defun keywordp (object) 
(and (symbolp object)
       (string= (package-name (symbol-package object)) "KEYwORD")))

#+BUILTIN 
(defun make-symbol (name)
  (let ((symbol (makei 9 0 name nil nil nil nil (- 1) 0)))
    (imakunbound symbol 4)
    (imakunbound symbol 5)
    (imakunbound symbol 6)
    symbol))

#+(or WAM-CL LISP500)
(defvar *gensym-counter* 0)
#+(or WAM-CL LISP500)
(defun gen-sym (&optional x)
  (let ((prefix (if (stringp x) x "G"))
	(suffix (if (fixnump x)
		    x
		    (let ((x *gensym-counter*))
		      (setf *gensym-counter* (+ 1 *gensym-counter*))))))
    (make-symbol (conc-string prefix (integer-string suffix)))))

(let ((gentemp-counter 0))
  (defun gentemp (&optional (prefix "T") (package *package*))
    (setf gentemp-counter (+ 1 gentemp-counter))
    (intern (conc-string prefix (integer-string gentemp-counter))
	    package)))


#+BUILTIN 
#+(or WAM-CL LISP500)
(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

#+(or WAM-CL LISP500)
(defun (setf get) (new-value symbol indicator &optional default)
  (setf (getf (symbol-plist symbol) indicator default) new-value))

#+(or WAM-CL LISP500)
(defun (setf rest) (new-tail list) (setf (cdr list) new-tail))

#+(or WAM-CL LISP500)
(defun remprop (symbol indicator) (remf (symbol-plist symbol) indicator))

#+BUILTIN 
#+(or WAM-CL LISP500)
(defun makunbound (symbol) (imakunbound symbol 4))

#+BUILTIN 
#+(or WAM-CL LISP500)
(defun set (symbol value) (setf (symbol-value symbol) value))

#+(or WAM-CL LISP500)
(defun designator-string (designator)
  (if (stringp designator)
      designator
      (if (characterp designator)
	  (string designator)
	  (symbol-name designator))))

#+BUILTIN 
#+(or WAM-CL LISP500)
(defvar *package* (car (cdr *packages*)))

#+(or WAM-CL LISP500)
(defun list-all-packages ()
  (copy-list *packages*))

#+(or WAM-CL LISP500)
(defun /= (number &rest numbers)
  (tagbody
   start
     (when numbers
       (dolist (n numbers)
	 (when (= number n)
	   (return-from /=)))
       (setq number (pop numbers))
       (go start)))
  t)



#+BUILTIN
#+(or WAM-CL LISP500) 
(defun > (&rest numbers) (apply #'< (reverse numbers)))


#+BUILTIN
#+(or WAM-CL LISP500) 
(defun <= (number &rest numbers)
  (dolist (n numbers t)
    (when (< n number)
      (return-from <=))
    (setq number n)))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun >= (number &rest numbers)
  (dolist (n numbers t)
    (when (< number n)
      (return-from >=))
    (setq number n)))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun max (real &rest reals)
  (dolist (r reals real)
    (when (< real r)
      (setq real r))))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun min (real &rest reals)
  (dolist (r reals real)
    (when (< r real)
      (setq real r))))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun oddp (integer)
  (= (mod integer 2) 1))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun evenp (integer)
  (= (mod integer 2) 0))


#+(or WAM-CL LISP500) 
(defun minusp (real)
  (< real 0))


#+(or WAM-CL LISP500) 
(defun plusp (real)
  (< 0 real))


#+(or WAM-CL LISP500) 
(defun zerop (real)
  (= real 0))


#+(or WAM-CL LISP500) 
(defun abs (number)
  (if (< number 0)
      (- number)
      number))



#+(or WAM-CL LISP500) 
(defun byte (size position)
  (cons size position))

#+(or WAM-CL LISP500) 
(defun byte-size (bytespec)
  (car bytespec))

#+(or WAM-CL LISP500) 
(defun byte-position (bytespec)
  (cdr bytespec))

#+(or WAM-CL LISP500) 
(defun char= (&rest characters)
  (apply #'= (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char/= (&rest characters)
  (apply #'/= (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char< (&rest characters)
  (apply #'< (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char> (&rest characters)
  (apply #'> (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char<= (&rest characters)
  (apply #'<= (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char>= (&rest characters)
  (apply #'>= (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char-equal (&rest characters)
  (apply #'char= (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun char-not-equal (&rest characters)
  (apply #'char/= (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun char-lessp (&rest characters)
  (apply #'char< (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun char-greaterp (&rest characters)
  (apply #'char> (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun char-not-greaterp (&rest characters)
  (apply #'char<= (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun char-not-lessp (&rest characters)
  (apply #'char>= (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun character (character)
  (if (characterp character)
      character
      (let ((string (designator-string character)))
	(if (= (length string) 1)
	    (aref string 0)
	    (error 'type-error :datum string :expected-type '(string 1))))))

#+LISP500
(defun characterp (object) (= (ldb '(5 . 0) (ival object)) 24))

#+(or WAM-CL LISP500) 
(defun alpha-char-p (character)
  (let ((code (char-code character)))
    (or (< 64 code 91)
	(< 96 code 123)
	(< 159 code))))

#+(or WAM-CL LISP500) 
(defun alphanumericp (character)
  (let ((code (char-code character)))
    (or (< 47 code 58)
	(< 64 code 91)
	(< 96 code 123)
	(< 159 code))))

#+(or WAM-CL LISP500) 
(defun digit-char (weight &optional (radix 10))
  (when (< weight radix)
    (if (< weight 10)
	(code-char (+ 48 weight))
	(code-char (+ 55 weight)))))

#+(or WAM-CL LISP500) 
(defun digit-char-p (char &optional (radix 10))
  (let* ((code (char-code char))
	 (weight (if (< 47 code 58)
		     (- code 48)
		     (if (< 64 code 91)
			 (- code 55)
			 (when (< 96 code 123)
			   (- code 87))))))
    (and weight (< weight radix) weight)))

#+(or WAM-CL LISP500) 
(defun standard-char-p (character)
  (let ((code (char-code character)))
    (or (= code 10)
	(< 31 code 127))))

#+(or WAM-CL LISP500) 
(defun char-upcase (character)
  (let ((code (char-code character)))
    (if (< 96 code 123)
	(code-char (- code 32))
	character)))

#+(or WAM-CL LISP500) 
(defun char-downcase (character)
  (let ((code (char-code character)))
    (if (< 64 code 91)
	(code-char (+ code 32))
	character)))

#+(or WAM-CL LISP500) 
(defun upper-case-p (character)
  (< 64 (char-code character) 91))

#+(or WAM-CL LISP500) 
(defun lower-case-p (character)
  (< 96 (char-code character) 123))

#+(or WAM-CL LISP500) 
(defun both-case-p (character)
  (or (upper-case-p character) (lower-case-p character)))

#+(or WAM-CL LISP500) 
(defun char-int (character)
  (char-code character))

#+(or WAM-CL LISP500) 
(defconstant char-code-limit 256)

#+(or WAM-CL LISP500) 
(let ((char-names '((0 . "Null")
		    (8 . "Backspace")
		    (9 . "Tab")
		    (10 . "Newline")
		    (12 . "Page")
		    (13 . "Return")
		    (32 . "Space")
		    (127 . "Rubout"))))
  (defun char-name (character)
    (let* ((code (char-code character))
	   (name (cdr (assoc code char-names))))
      (or name (when (< code 32)
		 (conc-string "U+" (integer-string code))))))
  (defun name-char (name)
    (setq name (designator-string name))
    (if (< (length name) 2)
	(aref name 0)
	(if (= (char-code (aref name 0)) 85)
	    (code-char (parse-integer name :start 2))
	    (let ((code (car (rassoc name char-names :test #'string-equal))))
	      (when code (code-char code)))))))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun atom (object) (not (consp object)))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun rplaca (cons object) (setf (car cons) object) cons)

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun rplacd (cons object) (setf (cdr cons) object) cons)



#+(or WAM-CL LISP500) 
(defun copy-tree (tree)
  (if (consp tree) (cons (copy-tree (car tree)) (copy-tree (cdr tree))) tree))

#+(or WAM-CL LISP500) 
(defun sublis (alist tree &rest rest)
  (if (consp tree)
      (let ((a (apply #'sublis alist (car tree) rest))
	    (d (apply #'sublis alist (cdr tree) rest)))
	(if (and (eq a (car tree)) (eq d (cdr tree)))
	    tree
	    (cons a d)))
      (let ((a (apply #'assoc tree alist rest)))
	(if a (cdr a) tree))))

#+(or WAM-CL LISP500) 
(defun nsublis (alist tree &rest rest)
  (if (consp tree)
      (progn
	(setf (car tree) (apply #'nsublis alist (car tree) rest))
	(setf (cdr tree) (apply #'nsublis alist (cdr tree) rest))
	tree)
      (let ((a (apply #'assoc tree alist rest)))
	(if a (cdr a) tree))))

#+(or WAM-CL LISP500) 
(defun copy-list (list)
  (if (consp list) (cons (car list) (copy-list (cdr list))) list))

#+(or WAM-CL LISP500) 
(defun make-list (size &key initial-element)
  (if (= size 0) nil
      (cons initial-element
	    (make-list (- size 1) :initial-element initial-element))))

#+(or WAM-CL LISP500) 
(defun list* (&rest objects)
  (if (cdr objects)
      (cons (car objects) (apply #'list* (cdr objects)))
      (car objects)))

#+(or WAM-CL LISP500) 
(defun list-length (list)
  (let ((slow list)
	(fast list)
	(odd nil)
	(len 0))
    (tagbody
     start
       (when (atom fast) (return-from list-length len))
       (setf fast (cdr fast))
       (setf len (+ 1 len))
       (when odd (setf slow (cdr slow)))
       (setf odd (not odd))
       (unless (eq slow fast) (go start)))))

#+(or WAM-CL LISP500) 
(defun listp (object) (or (consp object) (eq object nil)))



#+(or WAM-CL LISP500) 
(defun nth (n list) (if (< n 1) (car list) (nth (- n 1) (cdr list))))
'(defun (setf nth) (new-object n list)
  (if (< n 1)
      (setf (car list) new-object)
      (setf (nth (- n 1) (cdr list)) new-object)))


#+(or WAM-CL LISP500) 
(defun endp (list) (not list))

#+(or WAM-CL LISP500) 
(defun nconc (&rest lists)
  (if (cdr lists)
      (if (car lists)
	  (progn (setf (cdr (last (car lists))) (apply #'nconc (cdr lists)))
		 (car lists))
	  (apply #'nconc (cdr lists)))
      (car lists)))

#+(or WAM-CL LISP500) 
(defun revappend (list tail)
  (if list
      (revappend (cdr list) (cons (car list) tail))
      tail))

#+(or WAM-CL LISP500) 
(defun nreconc (list tail)
  (if list
      (let ((new-list (cdr list)))
	(setf (cdr list) tail)
	(nreconc new-list list))
      tail))

#+(or WAM-CL LISP500) 
(defun butlast (list &optional (n 1))
  (let* ((r (cons nil nil))
	 (e list)
	 (m 0))
    (tagbody
     start
       (when (consp e)
	 (setf m (+ m 1))
	 (setf e (cdr e))
	 (go start)))
    (setf n (- m n))
    (setf e r)
    (tagbody
     start
       (unless (consp list) (return-from butlast nil))
       (unless (< n 1)
	 (setf e (setf (cdr e) (cons (car list) nil)))
	 (setf list (cdr list))
	 (setf n (- n 1))
	 (go start)))
    (cdr r)))

#+(or WAM-CL LISP500) 
(defun nbutlast (list &optional (n 1))
  (let* ((e list)
	 (m 0))
    (tagbody
     start
       (when (consp e)
	 (setf m (+ m 1))
	 (setf e (cdr e))
	 (go start)))
    (setf n (- m n))
    (setf e list)
    (tagbody
     start
       (unless (consp list) (return-from nbutlast nil))
       (unless (< n 2)
	 (setf e (cdr e))
	 (setf n (- n 1))
	 (go start)))
    (setf (cdr e) nil)
    list))

#+(or WAM-CL LISP500) 
(defun last (list &optional (n 1))
  (let* ((e list)
	 (m 0))
    (tagbody
     start
       (when (consp e)
	 (setf m (+ m 1))
	 (setf e (cdr e))
	 (go start)))
    (setf n (- m n))
    (setf e list)
    (tagbody
     start
       (when (< n 1) (return-from last e))
       (setf e (cdr e))
       (setf n (- n 1))
       (go start))))

#+(or WAM-CL LISP500) 
(defun ldiff (list object)
  (let* ((r (cons nil nil))
	 (e r))
    (tagbody
     start
       (unless (or (eq object list) (atom list))
	 (setf e (setf (cdr e) (cons (car list) nil)))
	 (setf list (cdr list))
	 (go start)))
    (cdr r)))

#+(or WAM-CL LISP500) 
(defun tailp (object list)
  (tagbody
   start
     (when (eq object list) (return-from tailp t))
     (unless (consp list) (return-from tailp nil))
     (setf list (cdr list))
     (go start)))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun nthcdr (n list) (if (< n 1) list (nthcdr (- n 1) (cdr list))))


#+(or WAM-CL LISP500) 
(defun rest (list) (cdr list))


#+(or WAM-CL LISP500) 
(labels ((all-end (lists)
	   (dolist (elem lists nil)
	     (unless elem (return-from all-end t))))
	 (all-car (lists)
	   (when lists (cons (caar lists) (all-car (cdr lists)))))
	 (all-cdr (lists)
	   (when lists (cons (cdar lists) (all-cdr (cdr lists))))))
  (defun mapc (function &rest lists)
    (let ((list-1 (car lists)))
      (tagbody
       start
	 (when (all-end lists) (return-from mapc list-1))
	 (apply function (all-car lists))
	 (setf lists (all-cdr lists))
	 (go start))))
  (defun mapcar (function &rest lists)
    (let ((result nil)
	  (end nil))
      (tagbody
       start
	 (when (all-end lists) (return-from mapcar result))
	 (let ((cons (cons (apply function (all-car lists)) nil)))
	   (setf end (if end (setf (cdr end) cons) (setf result cons))))
	 (setf lists (all-cdr lists))
	 (go start))))
  (defun mapl (function &rest lists)
    (let ((list-1 (car lists)))
      (tagbody
       start
	 (when (all-end lists) (return-from mapl list-1))
	 (apply function lists)
	 (setf lists (all-cdr lists))
	 (go start))))
  (defun maplist (function &rest lists)
    (let ((result nil)
	  (end nil))
      (tagbody
       start
	 (when (all-end lists) (return-from maplist result))
	 (let ((cons (cons (apply function lists) nil)))
	   (setf end (if end (setf (cdr end) cons) (setf result cons))))
	 (setf lists (all-cdr lists))
	 (go start)))))



#+(or WAM-CL LISP500) 
(defun mapcan (function &rest lists)
  (apply #'nconc (apply #'mapcar function lists)))

#+(or WAM-CL LISP500) 
(defun mapcon (function &rest lists)
  (apply #'nconc (apply #'maplist function lists)))

#+(or WAM-CL LISP500) 
(defun acons (key datum alist) (cons (cons key datum) alist))

#+(or WAM-CL LISP500) 
(defun copy-alist (alist)
  (when alist (cons (if (consp (car alist))
			(cons (caar alist) (cdar alist))
			(car alist))
		    (copy-alist (cdr alist)))))

#+(or WAM-CL LISP500) 
(defun pairlis (keys data &optional alist)
  (tagbody
   start
     (when (and keys data)
       (setf alist (acons (car keys) (car data) alist))
       (setf keys (cdr keys))
       (setf data (cdr data))
       (go start)))
  alist)



#+(or WAM-CL LISP500) 
(defun some-list-2 (predicate list1 list2)
  (tagbody
   start
     (when (and list1 list2)
       (when (funcall predicate (car list1) (car list2))
	 (return-from some-list-2 t))
       (pop list1)
       (pop list2)
       (go start))))


(flet 
  ((satisfies (object elem &key key test test-not)
	 (let* ((zi (if key (funcall key elem) elem))
		(r (funcall (or test test-not #'eql) object zi)))
	   (if test-not (not r) r)))

       (satisfies-if (predicate elem &key key)
	 (funcall predicate (if key (funcall key elem) elem)))
       (satisfies-if-not (predicate elem &key key)
	 (not (funcall predicate (if key (funcall key elem) elem))))
       (seq-start (sequence &key (start 0) end from-end)
	 (if (listp sequence)
	     (if from-end
		 (let ((acc nil)
		       (sequence (nthcdr start sequence)))
		   (tagbody
		    start
		      (when (and sequence (or (not end) (< start end)))
			(push sequence acc)
			(setf sequence (cdr sequence))
			(setf start (+ 1 start))
			(go start)))
		   (list 3 acc start))
		 (list 2 (nthcdr start sequence) start))
	     (if from-end (cons 1 (- end 1)) (cons 0 start))))
       (seq-position (iter)
	 (case (car iter)
	   ((0 1) (cdr iter))
	   (t (caddr iter))))
       (seq-next (iter)
	 (case (car iter)
	   (0 (setf (cdr iter) (+ 1 (cdr iter))))
	   (1 (setf (cdr iter) (- (cdr iter) 1)))
	   (2 (setf (cadr iter) (cdadr iter))
	      (setf (caddr iter) (+ 1 (caddr iter))))
	   (t (setf (cadr iter) (cdadr iter))
	      (setf (caddr iter) (- (caddr iter) 1)))))
       (seq-ref (sequence iter)
	 (case (car iter)
	   ((0 1) (aref sequence (cdr iter)))
	   (2 (caadr iter))
	   (t (caaadr iter))))
       (seq-set (sequence iter value)
	 (case (car iter)
	   ((0 1) (setf (aref sequence (cdr iter)) value))
	   (2 (setf (caadr iter) value))
	   (t (setf (caaadr iter) value))))
       (seq-end-p (sequence iter &key start end from-end)
	 (case (car iter)
	   (0 (or (= (cdr iter) (length sequence))
		  (and end (= end (cdr iter)))))
	   (1 (< (cdr iter) start))
	   (2 (or (null (cadr iter)) (and end (= end (caddr iter)))))
	   (t (or (null (cadr iter)) (< (caddr iter) start)))))
       (seq-result (sequence iter result)
	 (case (car iter)
	   (0 (make-array (length result)
			  :element-type (array-element-type sequence)
			  :initial-contents (reverse result)))
	   (1 (make-array (length result)
			  :element-type (array-element-type sequence)
			  :initial-contents result))
	   (2 (reverse result))
	   (3 result))))



#+BUILTIN 
  (defun member (item list &rest rest)
    (tagbody
       start
       (when list
	 (when (apply #'satisfies item (car list) rest)
	   (return-from member list))
	 (setf list (cdr list))
	 (go start))))


#+BUILTIN 
  (defun member-if (predicate list &rest rest)
    (tagbody
       start
       (when list
	 (when (apply #'satisfies-if predicate (car list) rest)
	   (return-from member-if list))
	 (setf list (cdr list))
	 (go start))))

#+BUILTIN 
  (defun member-if-not (predicate list &rest rest)
    (tagbody
       start
       (when list
	 (when (apply #'satisfies-if-not predicate (car list) rest)
	   (return-from member-if list))
	 (setf list (cdr list))
	 (go start))))
  (defun subst (new old tree &rest rest)
    (if (consp tree)
	(let ((a (apply #'subst new old (car tree) rest))
	      (d (apply #'subst new old (cdr tree) rest)))
	  (if (and (eq a (car tree)) (eq d (cdr tree)))
	      tree
	      (cons a d)))
	(if (apply #'satisfies old tree rest) new tree)))
  (defun subst-if (new predicate tree &rest rest)
    (if (consp tree)
	(let ((a (apply #'subst new predicate (car tree) rest))
	      (d (apply #'subst new predicate (cdr tree) rest)))
	  (if (and (eq a (car tree)) (eq d (cdr tree)))
	      tree
	      (cons a d)))
	(if (apply #'satisfies-if predicate tree rest) new tree)))
  (defun subst-if-not (new predicate tree &rest rest)
    (if (consp tree)
	(let ((a (apply #'subst new predicate (car tree) rest))
	      (d (apply #'subst new predicate (cdr tree) rest)))
	  (if (and (eq a (car tree)) (eq d (cdr tree)))
	      tree
	      (cons a d)))
	(if (apply #'satisfies-if-not predicate tree rest) new tree)))
  (defun nsubst (new old tree &rest rest)
    (if (consp tree)
	(progn
	  (setf (car tree) (apply #'subst new old (car tree) rest))
	  (setf (cdr tree) (apply #'subst new old (cdr tree) rest))
	  tree)
	(if (apply #'satisfies old tree rest) new tree)))
  (defun nsubst-if (new predicate tree &rest rest)
    (if (consp tree)
	(progn
	  (setf (car tree) (apply #'subst new predicate (car tree) rest))
	  (setf (cdr tree) (apply #'subst new predicate (cdr tree) rest))
	  tree)
	(if (apply #'satisfies-if predicate tree rest) new tree)))
  (defun nsubst-if-not (new predicate tree &rest rest)
    (if (consp tree)
	(progn
	  (setf (car tree) (apply #'subst new predicate (car tree) rest))
	  (setf (cdr tree) (apply #'subst new predicate (cdr tree) rest))
	  tree)
	(if (apply #'satisfies-if-not predicate tree rest) new tree)))

#+BUILTIN 
  (defun assoc (item alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies item (car elem) rest)
	(return-from assoc elem))))
  (defun assoc-if (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if predicate (car elem) rest)
	(return-from assoc-if elem))))
  (defun assoc-if-not (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if-not predicate (car elem) rest)
	(return-from assoc-if-not elem))))
  (defun rassoc (item alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies item (cdr elem) rest)
	(return-from rassoc elem))))
  (defun rassoc-if (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if predicate (cdr elem) rest)
	(return-from rassoc-if elem))))
  (defun rassoc-if-not (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if-not predicate (cdr elem) rest)
	(return-from rassoc-if-not elem))))
  (defun adjoin (item list &rest rest)
    (dolist (elem list (cons item list))
      (when (apply #'satisfies item elem rest)
	(return-from adjoin list))))
  (defun set-exclusive-or (list-1 list-2 &rest rest &key key)
    (let ((result nil))
      (dolist (item list-1)
	(unless (apply #'member (if key (funcall key item) item) list-2 rest)
	  (push item result)))
      (dolist (item list-2)
	(block matches
	  (dolist (elem list-1)
	    (when (apply #'satisfies
			 (if key (funcall key elem) elem) item rest)
	      (return-from matches)))
	  (push item result)))
      result))
  (defun nset-exclusive-or (list-1 list-2 &rest rest &key key)
    (let ((result nil)
	  (list nil)
	  (item nil))
      (tagbody
       start-1
	 (unless list-1 (go start-2))
	 (setf item (car list-1))
	 (setf list list-2)
	 (setf prev nil)
       start-1-in
	 (unless list (go end-1-in))
	 (let ((elem (if key (funcall key (car list)) (car list))))
	   (when (apply #'satisfies item (if key (funcall key elem) elem) rest)
	     (if prev
		 (setf (cdr prev) (cdr list))
		 (setf list-2 (cdr list)))
	     (setf list-1 (cdr list-1))
	     (go start-1)))
	 (setf prev list)
	 (setf list (cdr list))
	 (go start-1-in)
       end-1-in
	 (setf item (cdr list-1))
	 (setf (cdr list-1) result)
	 (unless result (setf end list-1))
	 (setf result list-1)
	 (setf list-1 item)
	 (go start-1)
       start-2
	 (return-from nset-exclusive-or
	   (if end (progn (setf (cdr end) list-2) result) list-2)))))
  (defun fill (sequence item &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (seq-set sequence iter item)
	   (seq-next iter)
	   (go start))))
    sequence)
  (defun every (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (unless (apply predicate (mapcar #'seq-ref sequences iters))
	     (return-from every nil))
	   (mapc #'seq-next iters)
	   (go start))))
    t)
  (defun some (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (let ((result (apply predicate (mapcar #'seq-ref sequences iters))))
	     (when result (return-from some result)))
	   (mapc #'seq-next iters)
	   (go start)))))
  (defun notevery (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (unless (apply predicate (mapcar #'seq-ref sequences iters))
	     (return-from every t))
	   (mapc #'seq-next iters)
	   (go start)))))
  (defun notany (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (when (apply predicate (mapcar #'seq-ref sequences iters))
	     (return-from every nil))
	   (mapc #'seq-next iters)
	   (go start))))
    t)
  (defun map-into (result-sequence function &rest sequences)
    (let ((result-iter (seq-start result-sequence))
	  (iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (seq-set result-sequence result-iter
		    (apply function (mapcar #'seq-ref sequences iters)))
	   (seq-next result-iter)
	   (mapc #'seq-next iters)
	   (go start))))
    result-sequence)
  (defun reduce (function sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (if (apply #'seq-end-p sequence iter rest)
	  (funcall function)
	  (let ((elem (seq-ref sequence iter)))
	    (seq-next iter)
	    (unless (apply #'seq-end-p sequence iter rest)
	      (tagbody
	       start
		 (setq elem (funcall function elem (seq-ref sequence iter)))
		 (seq-next iter)
		 (unless (apply #'seq-end-p sequence iter rest)
		   (go start))))
	    elem))))
  (defun count (item sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest))
	  (count 0))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies item (seq-ref sequence iter) rest)
	     (setf count (+ 1 count)))
	   (seq-next iter)
	   (go start)))
      count))
  (defun count-if (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest))
	  (count 0))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if predicate (seq-ref sequence iter) rest)
	     (setf count (+ 1 count)))
	   (seq-next iter)
	   (go start)))
      count))
  (defun count-if-not (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest))
	  (count 0))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if-not predicate (seq-ref sequence iter)
			rest)
	     (setf count (+ 1 count)))
	   (seq-next iter)
	   (go start)))
      count))

#+BUILTIN 
  (defun find (item sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (when (apply #'satisfies item elem rest)
	       (return-from find elem)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun find-if (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (when (apply #'satisfies-if predicate elem rest)
	       (return-from find-if elem)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun find-if-not (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (when (apply #'satisfies-if-not predicate elem rest)
	       (return-from find-if-not elem)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun position (item sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies item (seq-ref sequence iter) rest)
	     (return-from position (seq-position iter)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun position-if (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if predicate (seq-ref sequence iter) rest)
	     (return-from position-if (seq-position iter)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN
  (defun position-if-not (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if-not predicate (seq-ref sequence iter)
			rest)
	     (return-from position-if-not (seq-position iter)))
	   (seq-next iter)
	   (go start)))))
  (defun remove (item sequence &rest rest &key count)
    (let ((iter (apply #'seq-start sequence rest))
	  (result nil))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (unless (and (apply #'satisfies item elem rest)
			  (or (not count) (not (minusp (decf count)))))
	       (push elem result)))
	   (seq-next iter)
	   (go start)))
      (seq-result sequence iter result)))
  (defun remove-if (predicate sequence &rest rest &key count)
    (let ((iter (apply #'seq-start sequence rest))
	  (result nil))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (unless (and (apply #'satisfies-if predicate elem rest)
			  (or (not count) (not (minusp (decf count)))))
	       (push elem result)))
	   (seq-next iter)
	   (go start)))
      (seq-result sequence iter result)))
  (defun remove-if-not (predicate sequence &rest rest &key count)
    (let ((iter (apply #'seq-start sequence rest))
	  (result nil))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (unless (and (apply #'satisfies-if-not predicate elem rest)
			  (or (not count) (not (minusp (decf count)))))
	       (push elem result)))
	   (seq-next iter)
	   (go start)))
      (seq-result sequence iter result)))
)                     


#+BUILTIN
#+(or WAM-CL LISP500) 
(defun null (object) (if object nil t))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun not (object) (if object nil t))




#+(or WAM-CL LISP500) 
(defun mod (x y) (multiple-value-call #'(lambda (q r) r) (floor x y)))

#+(or WAM-CL LISP500) 
#+BUILTIN
(defun functionp (object) (eq (type-of object) 'function))



#+(or WAM-CL LISP500) 
(defun coerce (object result-type)
  (if (typep object result-type)
      object
      (case result-type
	((t) object)
	(character (character object))
	(function (if (and (consp object) (eq (car object) 'lambda))
		      (eval (list 'function object))
		      (if (fboundp object)
			  (fdefinition object))
			  (error 'type-error :datum object
				 :expected-type result-type)))
	(t (error 'type-error :datum object :expected-type result-type)))))




#+(or WAM-CL LISP500) 
(defmacro deftype (name lambda-list &rest forms)
  `(ensure-type ',name #'(lambda ,lambda-list (block ,name ,@forms))))


#+(or WAM-CL LISP500) 
(defun *= (cons number)
  (or (not cons) (eq (car cons) '*) (= (car cons) number)))



#+(or WAM-CL LISP500) 
(defun ensure-type (name expander)
  (let ((cons (assoc name *type-expanders*)))
    (if cons
	(setf (cdr cons) expander)
	(push (cons name expander) *type-expanders*))
    name))



#+WAM-CL
#+(or WAM-CL LISP500) 
(defun ensure-package (name nicknames shadow shadowing-import-from use
		       import-from intern export)
  (let ((package (find-package name)))
    (unless package
      (setq package (make-package name :nicknames nicknames)))
    (shadow shadow package)
    (mapc #'(lambda (list)
	      (let ((imported-package (find-package (car list)))
		    (symbol-names (cdr list)))
		(shadowing-import (mapcar #'(lambda (symbol-name)
					      (find-symbol symbol-name
							   imported-package))
					  symbol-names)
				  package)))
	  shadowing-import-from)
    (use-package use package)
    (mapc #'(lambda (list)
	      (let ((imported-package (find-package (car list)))
		    (symbol-names (cdr list)))
		(import (mapcar #'(lambda (symbol-name)
				    (find-symbol symbol-name imported-package))
				symbol-names)
			package)))
	  import-from)
    (mapc #'(lambda (symbol-name) (intern symbol-name package)) intern)
    (export export package)
    package))

#+WAM-CL
#+(or WAM-CL LISP500) 
(defmacro defpackage (defined-package-name &rest options)
  (flet ((option (option-name)
	   (mapcan #'(lambda (option)
		       (when (eq (car option) option-name)
			 (mapcar #'designator-string (cdr option))))
		   options))
	 (options (option-name)
	   (mapcan #'(lambda (option)
		       (when (eq (car option) option-name)
			 (list (mapcar #'designator-string (cdr option)))))
		   options)))
    `(ensure-package ,(designator-string defined-package-name)
      ,(option :nicknames)
      ,(option :shadow) ,(options :shadowing-import-from) ,(option :use)
      ,(options :import-from) ,(option :intern) ,(option :export))))



#+BUILTIN
#+(or WAM-CL LISP500) 
(defun backquote-expand (list level)
  (if (consp list) 
      (if (eq 'backquote (car list)) 
	  (list 'list ''backquote 
		(backquote-expand (car (cdr list)) (+ level 1))) 
	  (if (eq 'unquote (car list)) 
	      (if (= level 0) 
		  (car (cdr list)) 
		  (list 'list ''unquote 
			(backquote-expand (car (cdr list)) (- level 1)))) 
	      (if (eq 'unquote-splicing (car list)) 
		  (if (= level 0) 
		      (values (car (cdr list)) t) 
		      (list 'list ''unquote-splicing 
			    (backquote-expand (car (cdr list)) (- level 1)))) 
		  (labels ((collect (list) 
			     (if (consp list) 
				 (cons (multiple-value-call 
					   #'(lambda (value 
						      &optional splicingp) 
					       (if splicingp 
						   value 
						   (list 'list value))) 
				       (backquote-expand (car list) level)) 
				     (collect (cdr list))) 
				 (list (list 'quote list))))) 
		    (cons 'append (collect list)))))) 
      (list 'quote list))) 

#+BUILTIN
#+(or WAM-CL LISP500) 
(defmacro backquote (form)
  (backquote-expand form 0))

