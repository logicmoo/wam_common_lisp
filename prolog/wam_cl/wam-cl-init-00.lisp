;;; #+BUILTIN Means to ignore since it should already be defined
;;; #+WAM-CL Means we want it
;;; #+LISP500 Means probably we dont want it
;;; #+ALT Alternative definition
;;; #+ABCL From ABCL
;;; #+SBCL From SBCL
;;; #+ECL From ECL
;;; #+SICL From SICL


(in-package #:system)


#-WAM-CL (defmacro put-sysprop (s p v) `(setf (get ,s ,p) ,v ))


(defclass pathname ()
  ((name      :accessor pathname-name
              :initarg :name
              :initform nil)
   (type      :accessor pathname-type
              :initarg :type
              :initform nil)
   (host      :accessor pathname-host
              :initarg :host
              :initform nil)
   (device    :accessor pathname-device
              :initarg :device
              :initform :unspecific)
   (directory :accessor pathname-directory
              :initarg :directory
              :initform nil)
   (version   :accessor pathname-version
              :initarg :version
              :initform nil))
  (:documentation "A physical pathname."))

(defmethod print-object ((self pathname) stream)
  (format stream "~:[~;#P\"~]~A~0@*~:[~;\"~]" *print-escape* (namestring self))
  self)


(defmacro defun=sourceinfo (name ll &rest body)
  "Used to show what was already compiled"
   `(put-sysprop ',name 'defun=sourceinfo `(defun ,',name ,',ll ,',@body)))

(defmacro defmacro=sourceinfo (name ll &rest body)
   "Used to show what was already compiled"
   `(put-sysprop ',name 'defmacro=sourceinfo '(defmacro ,name ,ll ,@body)))


(defmacro assert (test-form &optional places string &rest args)
  (declare (dynamic-extent args))
  `(do nil (,test-form nil)
     (multiple-value-setq
	 ,places
       (apply 'assert-places ',places (list ,@places)
	      ,@(if string `(,string (list ,@args)) `("The assertion ~:@(~S~) failed." ',test-form nil))))))


(defmacro eval-when-tl ((&rest when) &body body) (if (or (member 'eval when) (member ':execute when)) `(progn ,@body) nil)) 

(defun list* (arg &rest others)
  "Return a list of the arguments with last cons a dotted pair"
  (cond ((null others) arg)
	((null (cdr others)) (cons arg (car others)))
	(t (do ((x others (cdr x)))
	       ((null (cddr x)) (rplacd x (cadr x))))
	   (cons arg others))))


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


(defmacro setf (&rest pairs &environment env)
             (let ((nargs (length pairs)))
               (assert (evenp nargs))
               (cond
                ((zerop nargs) nil)
                ((= nargs 2)
                 (let ((place (car pairs))
                       (value-form (cadr pairs)))
                   (cond
                    ((symbolp place)
                     `(setq ,place ,value-form))
                    ((consp place)
                     (if (eq (car place) 'the)
                         `(setf ,(caddr place) (the ,(cadr place) ,value-form))
                       (multiple-value-bind (temps vars newvals setter getter)
                           (get-setf-expansion place env)
                         (declare (ignore getter))
                         `(let (,@(mapcar #'list temps vars))
                            (multiple-value-bind ,newvals ,value-form
                              ,setter))))))))
                (t
                 (do* ((pairs pairs (cddr pairs))
                       (setfs (list 'progn))
                       (splice setfs))
                      ((endp pairs) setfs)
                   (setq splice (cdr (rplacd splice
                                             `((setf ,(car pairs) ,(cadr pairs)))))))))))
(defmacro psetf (&rest pairs &environment env)
             (let ((nargs (length pairs)))
               (assert (evenp nargs))
               (if (< nargs 4)
                   `(progn (setf ,@pairs) nil)
                 (let ((setters nil))
                   (labels ((expand (pairs)
                                    (if pairs
                                        (multiple-value-bind (temps vars newvals setter getter)
                                            (get-setf-expansion (car pairs) env)
                                          (declare (ignore getter))
                                          (setq setters (cons setter setters))
                                          `(let (,@(mapcar #'list temps vars))
                                             (multiple-value-bind ,newvals ,(cadr pairs)
                                               ,(expand (cddr pairs)))))
                                      `(progn ,@setters nil))))
                     (expand pairs))))))

(defmacro shiftf (&rest places-and-newvalue &environment env)
             (let ((nargs (length places-and-newvalue)))
               (assert (>= nargs 2))
               (let ((place (car places-and-newvalue)))
                 (multiple-value-bind (temps vars newvals setter getter)
                     (get-setf-expansion place env)
                   `(let (,@(mapcar #'list temps vars))
                      (multiple-value-prog1 ,getter
                        (multiple-value-bind ,newvals
                            ,(if (= nargs 2)
                                 (cadr places-and-newvalue)
                               `(shiftf ,@(cdr places-and-newvalue)))
                          ,setter)))))))


(defmacro rotatef (&rest places &environment env)
             (if (< (length places) 2)
                 nil
               (multiple-value-bind (temps vars newvals setter getter)
                   (get-setf-expansion (car places) env)
                 `(let (,@(mapcar #'list temps vars))
                    (multiple-value-bind ,newvals (shiftf ,@(cdr places) ,getter)
                      ,setter)
                    nil))))


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



