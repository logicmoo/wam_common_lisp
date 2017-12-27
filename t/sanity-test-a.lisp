
;; 3.7 Changing a class

(list Eric (class-of Eric) (slot-exists-p Eric 'has-tail-p))

(defclass animal ()
  ((legs :reader leg-count :initarg :legs)
   (has-tail-p :reader has-tail-p :initform t)
   (comes-from :reader comes-from :initarg :comes-from)))

(list Eric (class-of Eric) #-(or cormanlisp CLISP WAM-CL) (slot-value Eric 'has-tail-p))

(defclass antelope (mammal)
  ((diet :reader munched-by)))

(change-class Eric 'antelope
              :diet 'greens)

(list (slot-exists-p Eric 'potter) (munched-by Eric))

;; 3.8 Implementation notes: object wrappers

#-(or cormanlisp CLISP WAM-CL)
(#+lispworks clos::wrapper-of #+allegro excl::wrapper-of
             Eric)


;; 4.1 Review - etypecase to drive function dispatch

(defun my-describe (thing)
  (typecase thing
    (cons   (describe-cons thing))
    (symbol (describe-symbol thing))
    (array  (describe-array thing))
    (number (describe-number thing))
    ;; [ etc etc etc ]
    (t      (describe-whatever thing))))

(defun describe-symbol (symbol)
  (let ((package (symbol-package symbol))
        (boundp (boundp symbol)))
    (format t
            "~s is a symbol. ~
             It ~:[~*does not have a home~;is in the ~s~] package. ~
             Its value is ~:[unbound~;~s~]."
            symbol
            package (when package (package-name package))
            boundp (when boundp (symbol-value symbol)))))

(my-describe :foo)

(my-describe '#:foo)

;; 4.2 defmethod

(fmakunbound 'my-describe)

(defmethod my-describe (thing)
  (format t
          "~s could be anything, for all I care."
          thing))

(defmethod my-describe ((animal animal))
  (format t
          "~s is an animal. It has ~d leg~:p ~
           and comes from ~a."
          animal
          (leg-count animal)
          (comes-from animal)))

(my-describe Eric)

(my-describe (make-instance 'figurine))

(mapcar 'class-name
        (#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
	   (class-of Eric)))

;; 4.3 Generic functions and method combination

#'my-describe

(#-allegro generic-function-methods #+allegro aclmop:generic-function-methods
   #'my-describe)

(#-allegro method-generic-function #+allegro aclmop:method-generic-function
   (car *))

(defmethod my-describe ((antelope antelope))
  (if (string= (slot-value antelope 'comes-from)
               "Brittany")
      (format t "Eric? Is that you?")
    (call-next-method)))

(my-describe 
 (make-instance 'antelope :comes-from 'nowhere :legs 4))

(my-describe Eric)

;; 4.5. Other specializers (you still don't need CLOS objects to use CLOS)

(defmethod my-describe ((self #+(or lispworks allegro) structure-object #+(or cormanlisp CLISP WAM-CL) structure))
  (format t "~s is a structure object."
          self))

(my-describe (make-foo))

(defmethod my-describe ((self foo))
  (format t "bar"))

(my-describe (make-foo))

(defmethod my-describe ((self (eql pi)))
  (format t "approximately 22/7"))

(defmethod my-describe ((self float))
  (format t "some float"))

(my-describe pi)

;; 4.6. Qualifiers and method combination

(defmethod my-describe :around (self)
  (call-next-method)
  (values))

(my-describe Eric)


(defun generate-defclass (class-name class-options supers slots)
  (let ((conc-name nil)
        (predicate nil)
        (predicate-forms nil))
    (loop
       for (option-name . args) in class-options
       do (ecase option-name
            (:conc-name
             (when conc-name
               (error "Can't specify the :CONC-NAME argument more than once."))
             (setf conc-name (first args)))
            (:predicate
             (when predicate
               (error "Can't specify the :PREDICATE argument more than once."))
             (setf predicate (if (eql t (first args))
                                 (intern (strcat class-name :-p) *package*)
                                 (first args))))))
    (setf slots
          (mapcar
           (lambda (slot-spec)
             (destructuring-bind (name
                                  &optional initform
                                  &rest options)
                 (ensure-list slot-spec)
               `(,name
                 :initform ,initform
                 ,@(when conc-name
                     `(:accessor ,(intern (strcat conc-name name)
                                          (symbol-package conc-name))))
                 :initarg ,(intern (symbol-name name) :keyword)
                 ,@options)))
           slots)
          predicate-forms
          (if predicate
              (with-unique-names (obj)
                `((defmethod ,predicate ((,obj ,class-name)) t)
                  (defmethod ,predicate ((,obj t)) nil)))
              nil))
    `(prog1
         (defclass ,class-name ,supers ,slots)
       ,@predicate-forms)))


