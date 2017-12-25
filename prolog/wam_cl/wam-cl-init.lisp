
(in-package #:system)

(defpackage "SYSTEM" (:nicknames "SYS"))
(defpackage "COMMON-LISP" (:nicknames "CL" "LISP")(:uses "SYSTEM"))
(defpackage "COMMON-LISP-USER" (:nicknames "U" "USER" "CL-USER") (:uses "COMMON-LISP"))
(defvar *lisp-file-type* "lisp") 
(defvar *default-pathname-defaults* #P"")
 
(defun dd () 
 (let ((*lisp-file-type* "cl") 
        (*default-pathname-defaults* (merge-pathnames "daydreamer/"))) (load "dd")))


(defun show-ascii-art ()
        
(write-line "  __________    ")
(write-line " / ___  ___ \\   ")
(write-line "/ / @ \\/ @ \\ \\  ")
(write-line "\\ \\___/\\___/ /\\ ")
(write-line " \\____\\/____/|| ")
(write-line " /     /\\\\\\\\\\// ")
(write-line "|     |\\\\\\\\\\\\   ")
(write-line " \\      \\\\\\\\\\\\  ")
(write-line "   \\______/\\\\\\\\ ")
(write-line "    _||_||_     ")
(write-line "                "))

(show-ascii-art)


(defmacro eval-when-tl ((&rest when) &body body) (if (or (member 'eval when) (member ':execute when)) `(progn ,@body) nil)) 


(in-package "SYSTEM")

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
            (%make-weak-hash-table test size rehash-size 
                                   rehash-threshold weakness))
	(%make-hash-table test size 
                          rehash-size rehash-threshold))))

    
  

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

(defun compiler-macroexpand (form &optional env)
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

(defmacro defconstant (name initial-value &optional docstring)
  `(progn
     (record-source-information-for-type ',name :constant)
     (%defconstant ',name ,initial-value ,docstring)))

(defmacro defparameter (name initial-value &optional docstring)
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

(defmacro prog1 (first-form &rest forms)
  (let ((result (gensym)))
    `(let ((,result ,first-form))
       ,@forms
       ,result)))

(defmacro prog2 (first-form second-form &rest forms)
  `(prog1 (progn ,first-form ,second-form) ,@forms))

;; Adapted from SBCL.
(defmacro push (&environment env item place)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (cons ,item ,place))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (let ((g (gensym)))
          `(let* ((,g ,item)
                  ,@(mapcar #'list dummies vals)
                  (,(car newval) (cons ,g ,getter)))
             ,setter)))))

;; Adapted from SBCL.
(defmacro pushnew (&environment env item place &rest keys)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (adjoin ,item ,place ,@keys))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (let ((g (gensym)))
          `(let* ((,g ,item)
                  ,@(mapcar #'list dummies vals)
                  (,(car newval) (adjoin ,g ,getter ,@keys)))
             ,setter)))))

;; Adapted from SBCL.
(defmacro pop (&environment env place)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(prog1 (car ,place)
	      (setq ,place (cdr ,place)))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (do* ((d dummies (cdr d))
              (v vals (cdr v))
              (let-list nil))
             ((null d)
              (push (list (car newval) getter) let-list)
              `(let* ,(nreverse let-list)
                 (prog1 (car ,(car newval))
                        (setq ,(car newval) (cdr ,(car newval)))
                        ,setter)))
          (push (list (car d) (car v)) let-list)))))

(defmacro psetq (&environment env &rest args)
  (do ((l args (cddr l))
       (forms nil)
       (bindings nil))
    ((endp l) (list* 'let* (reverse bindings) (reverse (cons nil forms))))
    (if (and (symbolp (car l))
             (eq (car l) (macroexpand-1 (car l) env)))
        (let ((sym (gensym)))
          (push (list sym (cadr l)) bindings)
          (push (list 'setq (car l) sym) forms))
        (multiple-value-bind
              (dummies vals newval setter getter)
            (get-setf-expansion (macroexpand-1 (car l) env) env)
          (declare (ignore getter))
          (do ((d dummies (cdr d))
               (v vals (cdr v)))
              ((null d))
            (push (list (car d) (car v)) bindings))
          (push (list (car newval) (cadr l)) bindings)
          (push setter forms)))))

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


(load "wam-cl-init-1")
'(load "wam-cl-init2")
'(load "wam-cl-init3")
'(write-line " WAM CommonLisp ")
'(read-eval-print-loop)

 

#|

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
