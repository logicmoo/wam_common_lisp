;;; #+BUILTIN Means to ignore since it should already be defined
;;; #+WAM-CL Means we want it
;;; #+LISP500 Means probably we dont want it
;;; #+ALT Alternative definition
;;; #+ABCL From ABCL
;;; #+SBCL From SBCL
;;; #+ECL From ECL
;;; #+SICL From SICL


(in-package "SYSTEM")


(defmacro make-hash-table (&rest all) `(make-instance 'hash-table ,@all))


#|
(defun make-hash-table (&key (test 'eql) (size 11) (rehash-size 1.5)
                             (rehash-threshold 0.75)
                             (weakness nil))
  (setf test (coerce-to-function test))
  (unless (and (integerp size) (>= size 0))
    (error 'type-error :datum size :expected-type '(integer 0)))
  (let ((size (max 11 (min size array-dimension-limit)))
        (weakness-types '(or (eql :key) (eql :value)
                             (eql :key-and-value)
                             (eql :key-or-value))))
    (if weakness
        (if (not (typep weakness weakness-types))
            (error 'type-error :datum weakness 
                   :expected-type weakness-types)
            (make-instance 'weak-hash-table test size rehash-size 
                                   rehash-threshold weakness))
	(make-instance 'hash-table test size 
                          rehash-size rehash-threshold))))
|#
(defclass hash-table :slots
			(
			 (data :type vector)
			 (size :type :integer)
			 (count :type :integer)
			 (hash-function)
			 (test)
			 (rehash-size)
			 (empty :type symbol)
			 (deleted :type symbol)
			 (not-found)))
    
  


(export 'compiler-macroexpand)

(defvar *compiler-macros* (make-hash-table :test #'equal))

(defun compiler-macro-function (name &optional environment)
  (declare (ignore environment))
  (gethash1 name (the hash-table *compiler-macros*)))

(defun (setf compiler-macro-function) (new-function name &optional environment)
  (declare (ignore environment))
  (setf (gethash name (the hash-table *compiler-macros*)) new-function))

(defmacro define-compiler-macro (name lambda-list &rest body)
  (let* ((form (gensym))
         (env (gensym))
         (block-name (fdefinition-block-name name)))
    (multiple-value-bind (body decls)
        (parse-defmacro lambda-list form body name 'defmacro :environment env
                        ;; when we encounter an error
                        ;; parsing the arguments in the call
                        ;; (not in the difinition!), return
                        ;; the arguments unmodified -- ie skip the
                        ;; transform (see also source-transform.lisp)
                        :error-fun `(lambda (&rest ignored)
                                      (declare (ignore ignored))
                                      (return-from ,block-name ,form)))
      (let ((expander `(lambda (,form ,env)
                         (declare (ignorable ,env))
                         (block ,block-name ,body))))
        `(progn
	   (record-source-information-for-type ',name :compiler-macro)
           (setf (compiler-macro-function ',name) (function ,expander))
           ',name)))))

;;; Adapted from OpenMCL.
(defun compiler-macroexpand-1 (form &optional env)
  (let ((expander nil)
        (new-form nil))
    (if (and (consp form)
             (symbolp (%car form))
             (setq expander (compiler-macro-function (%car form) env)))
        (values (setq new-form (funcall expander form env))
                (neq new-form form))
        (values form
                nil))))

(defun compiler-macroexpand (nform &optional env)
 (let ((name (car nform)) (form nform))
  (let* ((fn (COMPILER-MACRO-FUNCTION name))
	 (new (if fn (FUNCALL *MACROEXPAND-HOOK* fn form env) form)))
    (when (and (not (eq form new))
	       (consp form))
      (setq form (compiler-macroexpand
		  new
		  (if (eq (first new) 'FUNCALL)
		      (second new)
		      (first new))
		  env)))
    form)))

(defun=sourceinfo compiler-macroexpand (form &optional env)
  (let ((expanded-p nil))
    (loop
      (multiple-value-bind (expansion exp-p)
          (compiler-macroexpand-1 form env)
        (if exp-p
            (setf form expansion
                  expanded-p t)
            (return))))
    (values form expanded-p)))

(export 'defconst)

(defmacro in-package (name) 
  `(%in-package ,(string name)))

(defmacro when (test-form &rest body)
  (if (cdr body)
      `(if ,test-form (progn ,@body))
      `(if ,test-form ,(car body))))

(defmacro unless (test-form &rest body)
  (if (cdr body)
      `(if (not ,test-form) (progn ,@body))
      `(if (not ,test-form) ,(car body))))

(defmacro return (&optional result)
  `(return-from nil ,result))

(defmacro defconstant=sourceinfo (name initial-value &optional docstring)
  `(progn
     (record-source-information-for-type ',name :constant)
     (%defconstant ',name ,initial-value ,docstring)))

(defmacro defparameter=sourceinfo (name initial-value &optional docstring)
  `(progn
     (record-source-information-for-type ',name :variable)
     (%defparameter ',name ,initial-value ,docstring)))

(defmacro truly-the (type value)
  `(the ,type ,value))

(defmacro %car (x)
  `(car (truly-the cons ,x)))

(defmacro %cdr (x)
  `(cdr (truly-the cons ,x)))

(defmacro %cadr (x)
  `(%car (%cdr ,x)))

(defmacro %caddr (x)
  `(%car (%cdr (%cdr ,x))))

(defmacro=sourceinfo prog1 (first-form &rest forms)
  (let ((result (gensym)))
    `(let ((,result ,first-form))
       ,@forms
       ,result)))

(defmacro prog2=sourceinfo (first-form second-form &rest forms)
  `(prog1 (progn ,first-form ,second-form) ,@forms))


(defmacro time (form)
  `(%time #'(lambda () ,form)))

(defmacro with-open-stream (&rest args)
  (let ((var (caar args))
        (stream (cadar args))
        (forms (cdr args))
        (abortp (gensym)))
    `(let ((,var ,stream)
	   (,abortp t))
       (unwind-protect
        (multiple-value-prog1
         (progn ,@forms)
         (setq ,abortp nil))
        (when ,var
          (close ,var :abort ,abortp))))))


(defmacro defvar (var &optional (val nil valp) (doc nil docp))
  `(progn
     (sys::record-source-information-for-type ',var :variable)
     (%defvar ',var)
     ,@(when valp
         `((unless (boundp ',var)
             (setq ,var ,val))))
     ,@(when docp
         `((%set-documentation ',var 'variable ',doc)))
     ',var))

(defmacro defconst (name value)
  `(defconstant ,name
     (if (boundp ',name)
         (symbol-value ',name)
         ,value)))


'(load "wam-cl-init-1")
'(load "wam-cl-init2")
'(load "wam-cl-init3")
'(write-line " WAM CommonLisp ")
'(read-eval-print-loop)

 

#|


(defun ansi-loop (exps)
  (let ((*warn-on-redefinition* nil))
    (require 'loop))
  (fmakunbound 'ansi-loop)
  `(loop ,@exps))

(defmacro loop (&rest exps)
  (dolist (exp exps)
    (when (atom exp)
      (return-from loop (ansi-loop exps))))
  (let ((tag (gensym)))
    `(block nil (tagbody ,tag ,@exps (go ,tag)))))


;; (when (eq (symbol-package sym) p) (format t "~a ~a ~a ~a~%" ......)) 

(defun packagesyminfo (p0)
 (let ((p (find-package p0)))
 (do-all-symbols (sym)    
  (when (eq (symbol-package sym) p)
   (format t "symbolinfo('~s','~s').~%"
    sn (package-name (symbol-package sym))
    (constantp sym)
    (special-operator-p sym)
    (symbol-plist sym)
    sn (symbol-package sym)
    (if (boundp sym) (symbol-value sym))
    (if (fboundp sym) (type-of (symbol-function sym)))
    (fboundp sym)))))))
(packagesyminfo :cl)





(defun packagesyminfo (p0)
 (let ((p (find-package p0)))
 (do-all-symbols (sym)    
  (when (eq (symbol-package sym) p)
   (format t "symbol_package('~a','~a').~%"
    sn (package-name (symbol-package sym)))))))
(packagesyminfo :cl)


|#



