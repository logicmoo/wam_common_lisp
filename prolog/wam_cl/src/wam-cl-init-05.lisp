;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7 - OBJECTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro pjb-defclass (name super &rest args)
  "
This macro encapsulate DEFCLASS and allow the declaration of the attributes
in a shorter syntax.
ARGS  is a list of s-expr, whose car is either :ATT (to declare an attribute)
      or :DOC to give the documentation string of the class.
      (:OPT ...) is not implemented yet.
      (:ATT name type [ init-value [doc-string] | doc-string ]) defines
      an attribute named NAME, of type TYPE, with the given initial value
      and documentation strings.  An accessor and an initarg keyword of
      same NAME are also defined.
"
  (flet ((attrib (name type &rest args)
           "
This function outputs an attribute s-exp as used in defclass.
ARGS  may be of length 1 or 2.
      If (LENGTH ARGS) = 1
      then if the argument is a string,
           then it's taken as the documentation and the initial value is NIL
           else it's taken as the initial value and the documentation is NIL.
      else the first is the initial value and the second is the documentation.
The initarg an accessor are the same keyword built from the name.
"
           (let ((iarg (intern (if (symbolp name) (symbol-name name) name)
                               (find-package "KEYWORD")))
                 init doc)
             (cond  ((= 2 (length args))
                     (setq init (car  args)
                           doc  (cadr args)) )
                    ((= 1 (length args))
                     (if (stringp (car args))
                         (setq init nil
                               doc  (car args))
                         (setq init (car args)
                               doc  nil)) )
                    (t (error "Invalid attribute ~S"
                              `(:att ,name ,type ,@args))))
             (when (and (symbolp type) (null init))
               (setf type (list 'or 'null type)))
             (when (null doc)
               (setf doc (symbol-name name)))
             `(,name
               :initform ,init
               :initarg  ,iarg
               :accessor ,name
               :type     ,type
               :documentation ,doc))))
    (let ((fields  nil)
          (options nil))
      (do () ( (not args) )
        (cond ((eq :att (caar args))
               (push (apply (function attrib) (cdar args)) fields))
              ((eq :doc (caar args))
               (push (cons :documentation (cdar args)) options)))
        (setf args (cdr args)))
      (setf fields (nreverse fields))
      (setf options (nreverse options))
      `(defclass ,name ,super ,fields ,@options))))




(defun get-option (key options &optional list)
  (let ((opt (remove-if (lambda (x) (not (eq key (if (symbolp x) x (car x)))))
                        options)))
    (cond
      (list opt)
      ((null opt) nil)
      ((null (cdr opt))
       (if (symbolp (car opt)) t (cdar opt)))
      (t (error "Expected only one ~A option."
                (if (symbolp (car opt)) (car opt) (caar opt))))))) ;;GET-OPTION


(defun make-name (option prefix name suffix)
  (cond
    ((or (null option) (and option (not (listp option))))
     (intern (with-standard-io-syntax (format nil "~A~A~A" prefix name suffix))))
    ((and option (listp option) (car option))
     (car option))
    (t nil)))


(defun get-name (option)
  (if (and option (listp option))
      (car option)
      nil))


(defmacro define-structure-class (name-and-options &rest doc-and-slots)
  "
DO:     Define a class implementing the structure API.
        This macro presents the same API as DEFSTRUCT, but instead of
        defining a structure, it defines a class, and the same functions
        as would be defined by DEFSTRUCT.
        The DEFSTRUCT options: :TYPE and :INITIAL-OFFSET are not supported.
"
  (let (name options documentation slots slot-names accessors
        conc-name constructors copier
        include initial-offset predicate
        print-function print-object)
    (declare (ignorable initial-offset))
    (if (symbolp name-and-options)
        (setf name    name-and-options
              options nil)
        (setf name    (car name-and-options)
              options (cdr name-and-options)))
    (if (stringp (car doc-and-slots))
        (setf documentation (car doc-and-slots)
              slots         (cdr doc-and-slots))
        (setf documentation nil
              slots         doc-and-slots))
    (setf conc-name      (get-option :conc-name      options)
          constructors   (get-option :constructor    options :list)
          copier         (get-option :copier         options)
          predicate      (get-option :predicate      options)
          include        (get-option :include        options)
          initial-offset (get-option :initial-offset options)
          print-function (get-option :print-function options)
          print-object   (get-option :print-object   options))
    (when (and print-object print-function)
      (error "Cannot have :print-object and :print-function options."))
    (when (cdr include)
      (setf slots   (append (cddr include) slots)
            include (list (car include))))
    (setf conc-name (make-name conc-name ""      name "-")
          copier    (make-name copier    "COPY-" name "")
          predicate (make-name predicate ""      name "-P")
          print-function (get-name print-function)
          print-object   (get-name print-object))
    (setf slot-names (mapcar (lambda (s) (if (symbolp s) s (car s))) slots))
    (setf accessors  (mapcar
                      (lambda (s) (make-name nil (or conc-name "")
                                             (if (symbolp s) s (car s)) "")) slots))
    (if (null constructors)
        (setf constructors (list (make-name nil "MAKE-" name "")))
        (setf constructors
              (mapcan (lambda (x)
                        (cond
                          ((or (symbolp x) (= 1 (length x)))
                           (list (make-name nil "MAKE-" name "")))
                          ((null (second x))
                           nil)
                          ((= 2 (length x))
                           (list (second x)))
                          (t
                           (list (list (second x) (third x)))))) constructors)))
    `(progn
       (defclass ,name ,include
         ,(mapcar
           (lambda (slot accessor)
             (if (symbolp slot)
                 `(,slot :accessor  ,accessor)
                 (let* ((name        (first slot))
                        (initform-p  (cdr slot))
                        (initform    (car initform-p))
                        (type-p      (member :type (cddr slot)))
                        (type        (cadr type-p))
                        (read-only-p (member :read-only (cddr slot)))
                        (read-only   (cadr read-only-p)))
                   `(,name
                     ,(if (and read-only-p read-only) :reader :accessor)
                     ,accessor
                     ,@(when initform-p  (list :initform initform))
                     ,@(when type-p      (list :type     type))))))
           slots accessors)
         ,@(when documentation (list `(:documentation ,documentation))))
       ,@(mapcar
          (lambda (constructor)
            ;; generate a constructor.
            (if (symbolp constructor)
                (let ((preds (mapcar (lambda (x) (declare (ignore x)) (gensym))
                                     slot-names)))
                  `(defun ,constructor
                       (&key ,@(mapcar (lambda (s p) (list s nil p)) slot-names preds))
                     (let ((args nil))
                       ,@(mapcar
                          (lambda (s p)
                            `(when ,p
                               (push ,s args)
                               (push ,(keywordize s) args)))
                          slot-names preds)
                       (apply (function make-instance) ',name args))))
                (let ((cname  (first  constructor))
                      (pospar (second constructor)))
                  (declare (ignore pospar))
                  (warn "pjb-defclass does not implement this case yet.")
                  `(defun ,cname (&rest args)
                     (declare (ignore args))
                     (error "pjb-defclass does not implement this yet.")))))
          constructors)
       ,@(when copier
           (list `(defmethod ,copier ((self ,name))
                    (make-instance ',name
                                   ,@(mapcan
                                      (lambda (slot accessor)
                                        (list (keywordize slot) (list accessor 'self)))
                                      slot-names accessors)))))
       ,@(when predicate
           (list `(defmethod ,predicate (object)
                    (eq (type-of object) ',name))))
       ,@(when print-function
           (list `(defmethod print-object ((self ,name) stream)
                    (,print-function self stream 0))))
       ,@(when print-object
           (list `(defmethod print-object ((self ,name) stream)
                    (,print-object self stream)))))))



(defmacro define-with-object (class-name slots)
  "
DO:       Define a macro: (WITH-{CLASS-NAME} object &body body)
          expanding to:   (with-slots ({slots}) object @body)
"
  `(defmacro
       ,(intern (with-standard-io-syntax (format nil "WITH-~A" class-name)))
       (object &body body)
     `(with-slots (quote ,,(mapcar (lambda (slot) (list slot slot)) slots))
          ,object ,@body)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printing objects.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (declaration stepper))
(declaim (ftype (function ((or string symbol character)) symbol) keywordize))

;;;
;;; We have two way to print easily objects:
;;;
;;; 1- inherit from the SLOTED-OBJECT mixin class, and define a
;;;    SLOTS-FOR-PRINT method on your classes.
;;;
;;; 2- define a PRINT-OBJECT method on your classes using the macro
;;;    PRINT-PARSEABLE-OBJECT.
;;;

(defclass sloted-object ()
  ()
  (:documentation "
This is a mixin class providing generic SLOTS and PRINT-OBJECT
methods.
"))

(defgeneric extract-slots (object slots)
  (:documentation "
RETURN:         A plist slot values.
OBJECT:         A lisp object.
SLOTS:          A list of slot names.
")
  (:method (object slots)
    (assert (every (function symbolp) slots) (slots))
    (loop
      :for slot :in slots
      :collect (keywordize slot)
      :collect (if (slot-boundp object slot)
                   (slot-value object slot)
                   '#:unbound))))

(defgeneric slots-for-print (object)
  (:method-combination append)
  (:documentation "
This generic function collects a p-list describing the slots of the OBJECT.
The generic function EXTRACT-SLOTS can be used to build this p-list.
The APPEND method combination automatically appends the lists provided
by the SLOTS-FOR-PRINT methods on the various subclasses.
")
  (:method append ((object sloted-object))
    '()))

(defmethod print-object ((self sloted-object) stream)
  (declare (stepper disable))
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~{~S~^ ~}" (slots-for-print self)))
  self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun object-identity (object)
  "
RETURN:         A string containing the object identity as printed by
                PRINT-UNREADABLE-OBJECT.
"
  (declare (stepper disable))
  (let ((*step-mode* :run)
        (*print-readably* nil))
    (declare (special *step-mode*))
    (let ((ident
            (with-output-to-string (stream)
              (print-unreadable-object (object stream :type nil :identity t)))))
      (subseq ident 3 (1- (length ident))))))

(defun call-print-parseable-object (object stream type identity thunk)
  "
SEE:            PRINT-PARSEABLE-OBJECT
"
  (declare (stepper disable))
  (let ((*step-mode* :run))
    (declare (special *step-mode*))
    (if *print-readably*
        (error 'print-not-readable :object object)
        (progn
          (format stream "~S"
                  (append (when type
                            (list (class-name (class-of object))))
                          (funcall thunk object)
                          (when identity
                            (list :id (object-identity object)))))
          object))))

(defun gen-extract-slots (ovar slots)
  "
SEE:            PRINT-PARSEABLE-OBJECT
RETURN:         A form building a plist of slot values.
"
  (cons 'list
        (loop
          :for slot :in slots
          :collect  (if (symbolp slot)
                        (keywordize slot)
                        `(quote ,(first slot)))
          :collect  (if (symbolp slot)
                        `(if (slot-boundp ,ovar ',slot)
                             (slot-value ,ovar ',slot)
                             '#:unbound)
                        `(ignore-errors ,(second slot))))))

(defmacro print-parseable-object ((object stream &key (type t) identity) &rest slots)
  "

DO:             Prints on the STREAM the object as a list.  If all the
                objects printed inside it are printed readably or with
                PRINT-PARSEABLE-OBJECT, then that list should be
                readable, at least with *READ-SUPPRESS* set to T.

OBJECT:         Either a variable bound to the object to be printed,
                or a binding list (VARNAME OBJECT-EXPRESSION), in
                which case the VARNAME is bound to the
                OBJECT-EXPRESSION during the evaluation of the SLOTS.

STREAM:         The output stream where the object is printed to.

TYPE:           If true, the class-name of the OBJECT is printed as
                first element of the list.

IDENTITY:       If true, the object identity is printed as a string in
                the last position of the list.

SLOTS:          A list of either a symbol naming the slot, or a list
                (name expression), name being included quoted in the
                list, and the expression being evaluated to obtain the
                value.

RETURN:         The object that bas been printed (so that you can use
                it in tail position in PRINT-OBJECT conformingly).

EXAMPLE:        (print-parseable-object (object stream :type t :identity t)
                  slot-1
                  (:slot-2 (thing-to-list (slot-2 object)))
                  slot-3)
"
  `(locally (declare (stepper disable))
     ,(if (symbolp object)
          `(call-print-parseable-object ,object ,stream ,type ,identity
                                        (lambda (,object)
                                          (declare (ignorable ,object) (stepper disable))
                                          ,(gen-extract-slots object slots)))
          (destructuring-bind (ovar oval) object
            `(let ((,ovar ,oval))
               (call-print-parseable-object ,ovar ,stream ,type ,identity
                                            (lambda (,ovar)
                                              (declare (ignorable ,ovar) (stepper disable))
                                              ,(gen-extract-slots object slots))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8 - STRUCTURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (DEFMACRO DEFINE-WITH-STRUCTURE (NAME-AND-OPTIONS SLOTS)
;;   "
;; NAME-AND-OPTIONS:  Either a structure name or a list (name . options).
;;           Valid options are: (:conc-name prefix).
;; DO:       Define a macro: (WITH-{NAME} object &body body)
;;           expanding to a symbol-macrolet embedding body where
;;           symbol macros are defined to access the slots.
;; "
;;   (LET* ((NAME      (IF (SYMBOLP NAME-AND-OPTIONS)
;;                         NAME-AND-OPTIONS (CAR NAME-AND-OPTIONS)))
;;          (CONC-NAME (IF (SYMBOLP NAME-AND-OPTIONS)
;;                         (CONCATENATE 'STRING (STRING NAME) "-")
;;                         (LET ((CONC-OPT (CAR (MEMBER :CONC-NAME
;;                                                      (CDR NAME-AND-OPTIONS)
;;                                                      :KEY (FUNCTION CAR)))))
;;                           (IF CONC-OPT
;;                               (SECOND CONC-OPT)
;;                               (CONCATENATE 'STRING (STRING NAME) "-"))))))
;;     `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;        (DEFMACRO
;;            ,(INTERN (WITH-STANDARD-IO-SYNTAX (FORMAT NIL "WITH-~A" NAME)))
;;            (OBJECT &BODY BODY)
;;          (IF (SYMBOLP OBJECT)
;;              `(SYMBOL-MACROLET
;;                   ,(MAPCAR
;;                     (LAMBDA (SLOT)
;;                       (LIST SLOT
;;                             (LIST
;;                              (INTERN (WITH-STANDARD-IO-SYNTAX
;;                                        (CONCATENATE 'STRING
;;                                          (STRING ',CONC-NAME) (STRING SLOT))))
;;                              OBJECT))) ',SLOTS)
;;                 ,@BODY)
;;              (LET ((OBJV (GENSYM)))
;;                `(LET ((,OBJV ,OBJECT))
;;                   (SYMBOL-MACROLET
;;                       ,(MAPCAR
;;                         (LAMBDA (SLOT)
;;                           (LIST SLOT
;;                                 (LIST
;;                                  (INTERN (WITH-STANDARD-IO-SYNTAX
;;                                            (CONCATENATE 'STRING
;;                                              (STRING ',CONC-NAME) (STRING SLOT))))
;;
;;                                  OBJV))) ',SLOTS)
;;                     ,@BODY)))))))) ;;DEFINE-WITH-STRUCTURE

(defmacro define-with-structure (name-and-options &rest slots)
  "
NAME-AND-OPTIONS:  Either a structure name or a list (name . options).
          Valid options are: (:conc-name prefix).
DO:       Define a macro: (WITH-{NAME} object &body body)
          expanding to a symbol-macrolet embedding body where
          symbol macros are defined to access the slots.
"
  (let* ((name      (if (symbolp name-and-options)
                        name-and-options (car name-and-options)))
         (conc-name (if (symbolp name-and-options)
                        (concatenate 'string (string name) "-")
                        (let ((conc-opt (car (member :conc-name
                                                     (cdr name-and-options)
                                                     :key (function car)))))
                          (if conc-opt
                              (second conc-opt)
                              (concatenate 'string (string name) "-")))))
         (slot-names (mapcar (lambda (slot) (if (listp slot) (car slot) slot))
                             slots)))
    `(progn
       (defstruct ,name-and-options ,@slots)
       (defmacro
           ,(intern (with-standard-io-syntax (format nil "WITH-~A" name)))
           (object &body body)
         (if (symbolp object)
             `(symbol-macrolet
                  ,(mapcar
                    (lambda (slot)
                      (list slot
                            (list
                             (intern (concatenate 'string (string ',conc-name) (string slot)))
                             object))) ',slot-names)
                ,@body)
             (let ((objv (gensym)))
               `(let ((,objv ,object))
                  (symbol-macrolet
                      ,(mapcar
                        (lambda (slot)
                          (list slot
                                (list
                                 (intern (concatenate 'string (string ',conc-name) (string slot)))
                                 objv))) ',slot-names)
                    ,@body))))))))
