;;; #+BUILTIN Means to ignore since it should already be defined
;;; #+WAM-CL Means we want it
;;; #+LISP500 Means probably we dont want it
;;; #+ALT Alternative definition
;;; #+ABCL From ABCL
;;; #+SBCL From SBCL
;;; #+ECL From ECL
;;; #+SICL From SICL


(in-package #:system)


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

#+(or WAM-CL LISP500)
(defmacro nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))

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
          ((setq temp (get-sysprop  (car form) 'setf-inverse))
           (get-setf-method-inverse form `(,temp) nil))
          ((setq temp (get-sysprop  (car form) 'setf-expander))
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
                (let ((inverse (get-sysprop  (car place) 'setf-inverse)))
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


;; #+(or ABCL WAM-CL) (flet () ;; FLET BEGIN

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

;; ) ;; FLET END


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



