
(in-package "SYSTEM")


;;; The DEFSTRUCT macro.

(defmacro defstruct (name &rest slots)
  "Syntax: (defstruct
         {name | (name {:conc-name | (:conc-name prefix-string) |
                        :constructor | (:constructor symbol [lambda-list]) |
                        :copier | (:copier symbol) |
                        :predicate | (:predicate symbol) |
                        (:include symbol) |
                        (:print-function function) |
                        (:type {vector | (vector type) | list}) |
                        :named |
                        (:initial-offset number)}*)}
         [doc]
         {slot-name |
          (slot-name [default-value-form] {:type type | :read-only flag}*) }*
         )
Defines a structure named by NAME.  The doc-string DOC, if supplied, is saved
as a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure)."
  (let*((slot-descriptions slots)
	;;#+clos
	local-slot-descriptions
        options
        conc-name
        constructors default-constructor no-constructor
        copier
        predicate predicate-specified
        include
        print-function type named initial-offset
        offset name-offset
        documentation)

    (when (consp name)
          ;; The defstruct options are supplied.
          (setq options (cdr name))
          (setq name (car name)))

    ;; The default conc-name.
    (setq conc-name (string-concatenate (string name) "-"))

    ;; The default constructor.
    (setq default-constructor
          (intern (string-concatenate "MAKE-" (string name))))

    ;; The default copier and predicate.
    (setq copier
          (intern (string-concatenate "COPY-" (string name)))
          predicate
          (intern (string-concatenate (string name) "-P")))

    ;; Parse the defstruct options.
    (do ((os options (cdr os)) (o) (v))
        ((endp os))
      (cond ((and (consp (car os)) (not (endp (cdar os))))
             (setq o (caar os) v (cadar os))
             (case o
               (:CONC-NAME
                (if (null v)
                    (setq conc-name nil)
                    (setq conc-name v)))
               (:CONSTRUCTOR
                (if (null v)
                    (setq no-constructor t)
                    (if (endp (cddar os))
                        (setq constructors (cons v constructors))
                        (setq constructors (cons (cdar os) constructors)))))
               (:COPIER (setq copier v))
               (:PREDICATE
                (setq predicate v)
                (setq predicate-specified t))
               (:INCLUDE
                (setq include (cdar os))
                (unless (get-sysprop v 'IS-A-STRUCTURE)
                        (error "~S is an illegal included structure." v)))
               (:PRINT-FUNCTION (setq print-function v))
               (:TYPE (setq type v))
               (:INITIAL-OFFSET (setq initial-offset v))
               (t (error "~S is an illegal defstruct option." o))))
            (t
             (if (consp (car os))
                 (setq o (caar os))
                 (setq o (car os)))
             (case o
               (:CONSTRUCTOR
                (setq constructors
                      (cons default-constructor constructors)))
               ((:CONC-NAME :COPIER :PREDICATE :PRINT-FUNCTION))
               (:NAMED (setq named t))
               (t (error "~S is an illegal defstruct option." o))))))

    (setq conc-name (intern (string conc-name)))

    ;; Skip the documentation string.
    (when (and (not (endp slot-descriptions))
               (stringp (car slot-descriptions)))
          (setq documentation (car slot-descriptions))
          (setq slot-descriptions (cdr slot-descriptions)))
    
    ;; Check the include option.
    (when include
          (unless (equal type (get-sysprop (car include) 'STRUCTURE-TYPE))
                  (error "~S is an illegal structure include."
                         (car include))))

    ;; Set OFFSET.
    (setq offset (if include
		     (get-sysprop (car include) 'STRUCTURE-OFFSET)
		     0))

    ;; Increment OFFSET.
    (when (and type initial-offset)
          (setq offset (+ offset initial-offset)))
    (when (and type named)
          (setq name-offset offset)
          (setq offset (1+ offset)))

    ;; Parse slot-descriptions, incrementing OFFSET for each one.
    (do ((ds slot-descriptions (cdr ds))
         (sds nil))
        ((endp ds)
         (setq slot-descriptions (nreverse sds)))
      (push (parse-slot-description (car ds) offset) sds)
      (setq offset (1+ offset)))

    ;; If TYPE is non-NIL and structure is named,
    ;;  add the slot for the structure-name to the slot-descriptions.
    (when (and type named)
          (setq slot-descriptions
                (cons (list nil name) slot-descriptions)))

    ;; Pad the slot-descriptions with the initial-offset number of NILs.
    (when (and type initial-offset)
          (setq slot-descriptions
                (append (make-list initial-offset) slot-descriptions)))

    ;;#+clos
    (setq local-slot-descriptions slot-descriptions)

    ;; Append the slot-descriptions of the included structure.
    ;; The slot-descriptions in the include option are also counted.
    (cond ((null include))
          ((endp (cdr include))
           (setq slot-descriptions
                 (append (get-sysprop (car include) 'STRUCTURE-SLOT-DESCRIPTIONS)
                         slot-descriptions)))
          (t
           (setq slot-descriptions
                 (append (overwrite-slot-descriptions
                          (mapcar #'(lambda (sd)
                                      (parse-slot-description sd 0))
                                  (cdr include))
                          (get-sysprop (car include) 'STRUCTURE-SLOT-DESCRIPTIONS))
                         slot-descriptions))))

    (cond (no-constructor
           ;; If a constructor option is NIL,
           ;;  no constructor should have been specified.
           (when constructors
                 (error "Contradictory constructor options.")))
          ((null constructors)
           ;; If no constructor is specified,
           ;;  the default-constructor is made.
           (setq constructors (list default-constructor))))

    ;; Check the named option and set the predicate.
    (when (and type (not named))
          (when predicate-specified
                (error "~S is an illegal structure predicate."
                       predicate))
          (setq predicate nil))

    (when include (setq include (car include)))

    ;; Check the print-function.
    (when (and print-function type)
          (error "An print function is supplied to a typed structure."))

    (if (or type (not (member ':CLOS *features*)))
	`(eval-when (compile load eval)

	  (define-structure ',name ',conc-name ',type ',named ',slots
			    ',slot-descriptions ',copier ',include
			    ',print-function ',constructors ',offset
			    ',documentation)
	  ,@(mapcar #'(lambda (constructor)
			(make-constructor name constructor type named
					  slot-descriptions))
	     constructors)
	  ,@(when predicate
	      (list `(fset ',predicate
		      (make-predicate ',name ',type ',named ',name-offset))))
	  ',name)

      ;; else (and (not type) (member :CLOS *features*))

      `(eval-when (compile load eval)

	(defclass ,name (,(or include 'STRUCTURE-OBJECT))
	  ,(mapcar
	    #'(lambda (sd)
		(if sd
		    (list* (first sd)
			   :initform (second sd)
			   :initarg 
			   (intern (symbol-name (first sd))
				   (find-package 'KEYWORD))
			   (when (third sd) (list :type (third sd))))
		    nil))		; for initial offset slots
	    local-slot-descriptions)
	  (:metaclass structure-class))

#|	   (with-slots (defstruct-form slot-descriptions initial-offset
			 constructors documentation copier predicate
			 print-function)
		       (find-class ',name)
              (setq defstruct-form '(defstruct ,name ,@slots))
	      (setq slot-descriptions ',slot-descriptions)
	      (setq initial-offset ',structure-offset)
	      (setq constructors ',constructors)
	      (setq documentation ,documentation)
	      (setq copier ,copier)
	      (setq predicate ,predicate)
	      (setq print-function ,print-function))
|#

	,@(if print-function
	      `((defmethod print-object
		    ((obj ,name) stream)
		  (,print-function obj stream *print-level*))))

	(define-structure ',name ',conc-name ',type ',named ',slots
			  ',slot-descriptions ',copier ',include
			  ',print-function ',constructors ',offset
			  ',documentation)
	,@(mapcar #'(lambda (constructor)
		      (make-constructor name constructor type named
					slot-descriptions))
	   constructors)
	,@(when predicate
	    (list `(fset ',predicate
		    (make-predicate ',name ',type ',named ',name-offset))))
	',name))))


;;; The #S reader.

(defun sharp-s-reader (stream subchar arg)
  (declare (ignore subchar))
  (when (and arg (null *read-suppress*))
        (error "An extra argument was supplied for the #S readmacro."))
  (let ((l (read stream)))
    (unless (get-sysprop (car l) 'IS-A-STRUCTURE)
            (error "~S is not a structure." (car l)))
    ;; Intern keywords in the keyword package.
    (do ((ll (cdr l) (cddr ll)))
        ((endp ll)
         ;; Find an appropriate construtor.
         (do ((cs (get-sysprop (car l) 'STRUCTURE-CONSTRUCTORS) (cdr cs)))
             ((endp cs)
              (error "The structure ~S has no structure constructor."
                     (car l)))
           (when (symbolp (car cs))
                 (return (apply (car cs) (cdr l))))))
      (rplaca ll (intern (string (car ll)) 'KEYWORD)))))



(defun make-constructor (name constructor type named slot-descriptions)
  (declare (ignore named))
  (let ((slot-names
         ;; Collect the slot-names.
         (mapcar #'(lambda (x)
                     (cond ((null x)
                            ;; If the slot-description is NIL,
                            ;;  it is in the padding of initial-offset.
                            nil)
                           ((null (car x))
                            ;; If the slot name is NIL,
                            ;;  it is the structure name.
                            ;;  This is for typed structures with names.
                            (list 'QUOTE (cadr x)))
                           (t (car x))))
                 slot-descriptions))
        (keys
         ;; Make the keyword parameters.
         (mapcan #'(lambda (x)
                     (cond ((null x) nil)
                           ((null (car x)) nil)
                           ((null (cadr x)) (list (car x)))
                           (t (list (list  (car x) (cadr x))))))
                 slot-descriptions)))
    (cond ((consp constructor)
           ;; The case for a BOA constructor.
           ;; Dirty code!!
           ;; We must add an initial value for an optional parameter,
           ;;  if the default value is not specified
           ;;  in the given parameter list and yet the initial value
           ;;  is supplied in the slot description.
           (do ((a (cadr constructor) (cdr a)) (l nil) (vs nil))
               ((endp a)
                ;; Add those options that do not appear in the parameter list
                ;;  as auxiliary paramters.
                ;; The parameters are accumulated in the variable VS.
                (setq keys
                      (nreconc (cons '&aux l)
                               (mapcan #'(lambda (k)
                                           (if (member (if (atom k) k (car k))
                                                       vs)
                                               nil
                                               (list k)))
                                       keys))))
             ;; Skip until &OPTIONAL appears.
             (cond ((eq (car a) '&optional)
                    (setq l (cons '&optional l))
                    (do ((aa (cdr a) (cdr aa)) (ov) (y))
                        ((endp aa)
                         ;; Add those options that do not appear in the
                         ;;  parameter list.
                         (setq keys
                               (nreconc (cons '&aux l)
                                        (mapcan #'(lambda (k)
                                                    (if (member (if (atom k)
                                                                    k
                                                                    (car k))
                                                                vs)
                                                        nil
                                                        (list k)))
                                                keys)))
                         (return nil))
                      (when (member (car aa) lambda-list-keywords)
                            (when (eq (car aa) '&rest)
                                  ;; &REST is found.
                                  (setq l (cons '&rest l))
                                  (setq aa (cdr aa))
                                  (unless (and (not (endp aa))
                                               (symbolp (car aa)))
                                          (illegal-boa))
                                  (setq vs (cons (car aa) vs))
                                  (setq l (cons (car aa) l))
                                  (setq aa (cdr aa))
                                  (when (endp aa)
                                        (setq keys
                                              (nreconc
                                               (cons '&aux l)
                                               (mapcan
                                                #'(lambda (k)
                                                    (if (member (if (atom k)
                                                                    k
                                                                    (car k))
                                                                vs)
                                                        nil
                                                        (list k)))
                                                keys)))
                                        (return nil)))
                            ;; &AUX should follow.
                            (unless (eq (car aa) '&aux)
                                    (illegal-boa))
                            (setq l (cons '&aux l))
                            (do ((aaa (cdr aa) (cdr aaa)))
                                ((endp aaa))
                              (setq l (cons (car aaa) l))
                              (cond ((and (atom (car aaa))
                                          (symbolp (car aaa)))
                                     (setq vs (cons (car aaa) vs)))
                                    ((and (symbolp (caar aaa))
                                          (or (endp (cdar aaa))
                                              (endp (cddar aaa))))
                                     (setq vs (cons (caar aaa) vs)))
                                    (t (illegal-boa))))
                            ;; End of the parameter list.
                            (setq keys
                                  (nreconc l
                                           (mapcan
                                            #'(lambda (k)
                                                (if (member (if (atom k)
                                                                k
                                                                (car k))
                                                            vs)
                                                    nil
                                                    (list k)))
                                            keys)))
                            (return nil))
                      ;; Checks if the optional paramter without a default
                      ;;  value has a default value in the slot-description.
                      (if (and (cond ((atom (car aa)) (setq ov (car aa)) t)
                                     ((endp (cdar aa)) (setq ov (caar aa)) t)
                                     (t nil))
                               (setq y (member ov
                                               keys
                                               :key
                                               #'(lambda (x)
                                                   (if (consp x)
                                                       ;; With default value.
                                                       (car x))))))
                          ;; If no default value is supplied for
                          ;;  the optional parameter and yet appears
                          ;;  in KEYS with a default value,
                          ;;  then cons the pair to L,
                          (setq l (cons (car y) l))
                          ;;  otherwise cons just the parameter to L.
                          (setq l (cons (car aa) l)))
                      ;; Checks the form of the optional parameter.
                      (cond ((atom (car aa))
                             (unless (symbolp (car aa))
                                     (illegal-boa))
                             (setq vs (cons (car aa) vs)))
                            ((not (symbolp (caar aa)))
                             (illegal-boa))
                            ((or (endp (cdar aa)) (endp (cddar aa)))
                             (setq vs (cons (caar aa) vs)))
                            ((not (symbolp (caddar aa)))
                             (illegal-boa))
                            ((not (endp (cdddar aa)))
                             (illegal-boa))
                            (t
                             (setq vs (cons (caar aa) vs))
                             (setq vs (cons (caddar aa) vs)))))
                    ;; RETURN from the outside DO.
                    (return nil))
                   (t
                    (unless (symbolp (car a))
                            (illegal-boa))
                    (setq l (cons (car a) l))
                    (setq vs (cons (car a) vs)))))
           (setq constructor (car constructor)))
          (t
           ;; If not a BOA constructor, just cons &KEY.
           (setq keys (cons '&key keys))))
    (cond ((null type)
           `(defun ,constructor ,keys
	      #-CLOS
              (sys:make-structure ',name ,@slot-names)
	      #+CLOS
	      (sys:make-structure (find-class ',name) ,@slot-names)))
          ((or (eq type 'VECTOR)
               (and (consp type) (eq (car type) 'vector)))
           `(defun ,constructor ,keys
              (vector ,@slot-names)))
          ((eq type 'LIST)
           `(defun ,constructor ,keys
              (list ,@slot-names)))
          ((error "~S is an illegal structure type" type)))))

