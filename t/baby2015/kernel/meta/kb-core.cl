;;; -*- Mode: LISP; Package: BABYLON; Syntax: Common-lisp; Base: 10 -*-

(in-package "BABYLON")
 
;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHORS:  Franco di Primio, Eckehard Gross, Juergen Walther


;; CONTENTS: the base flavor for knowledge bases


;;-----------------------------------------------------------------------
;;                BASE FLAVOR FOR KNOWLEDGE BASES
;;-----------------------------------------------------------------------

(def$flavor kb-processor-core
        ((kb-name nil)
	 (language *default-language*)
	 (instructions nil)
	 (procs nil))
	()
  :settable-instance-variables
  (:documentation "This is the base flavor for knowledge-bases.
It provides all the facilities to handle knowledge base operations,
which are global and not exclusively handled by one of the special
processors."))

;;-----------------------------------------------------------------------
;;              KNOWLEDGE BASE CONFIGURATION CONSTRUCTOR 
;;-----------------------------------------------------------------------

(defun filter-options (key options)
  (remove key options :test #'(lambda (a-key element)
				(and (listp element)
				     (eq a-key (first element))))))

(defmacro def-kb-configuration (flavor-name &rest options)
  "knowledge base configuration constructor.
Generates a knowledge base flavor and a method to determine the type of an expression
which has to be passed to the appropriate processor. The most important options
are: (:special . special-mixins) (:procs . processor-mixins) (:interface . interface-mixins)
which determine the special,processor and interface mixins to be included in the flavor.
No defaults are used if these options are not specified.
All other options are passed to the defflavor form."
  
  (let ((special-mixins (rest (assoc :special options)))
	(proc-mixins (rest (assoc :procs options))) 
	(interface-mixins (rest (assoc :interface options)))
        (rest-options (filter-options :special
			(filter-options :interface
			  (filter-options :procs options)))))
    (dolist (flavor (append proc-mixins interface-mixins special-mixins))
	   (or (flavorp flavor) (bab-require flavor)))
    
    `(progn
      
       (dolist (flavor (append ',proc-mixins ',interface-mixins ',special-mixins))
	 (or (flavorp flavor) (bab-require flavor)))
       
       (def$flavor ,flavor-name  ()
		  (,@special-mixins
		   ,@interface-mixins
		   ,@proc-mixins
		   meta-processor-core
		   kb-processor-core)
	 :settable-instance-variables
	 ,@rest-options)
       
       (def$method (,flavor-name  :get-type) (request)
	 (or ,@(mapcan #'(lambda (proc-mixin)
			   (let ((fkt (get proc-mixin :typefkt)))
			     (if fkt
				 `((,fkt request)))))
		       proc-mixins)))

       ',flavor-name)))

;;-----------------------------------------------------------------------
;;              KNOWLEDGE BASE INSTANCE CONSTRUCTOR 
;;-----------------------------------------------------------------------

	
(def$method (kb-processor-core :make-yourself-known) ()
  "internal method.
adds the the name of the kb to the list of known kbs."
  (unless (member kb-name *known-knowledge-bases*)
    (push kb-name *known-knowledge-bases*)))

(def$method (kb-processor-core :make-yourself-unknown) ()
  "internal method.
removes the name of the kb from the list of known kbs."  
  (setf *known-knowledge-bases*
	(remove kb-name *known-knowledge-bases*)))

(def$method (kb-processor-core :make-yourself-current) ()
  "makes the kb to the current one and its language to the current language."
  (if (and (is-activated-kb)
	   (not (eq *current-knowledge-base* self)))
      ($send *current-knowledge-base* :deselect-kb))
  (setq *current-knowledge-base* self)
  (setq *language* language))

(def$method (kb-processor-core :store-deklaration) (spec)
  "internal method."
  (declare (ignore spec))
  t)

(defun use-old-kb? (kb-name)
  (when (boundp kb-name)
    (let ((kb (symbol-value kb-name)))
      (cond ((not (flavor-typep kb 'kb-processor-core)) nil)
	    ((y-or-n-p (format nil (getentry kb-exists-fstr babylon-io-table)
			       kb-name (flavor-type-of kb)))
	     ($send kb :make-yourself-current) t)
	    (t ($send kb :kill-kb) nil)))))


(defmacro def-kb-instance (kb-name kb-configuration &rest init-plist)
  "knowledge base instance constructor.
generates an instance of the flavor kb-configuration and assigns it to kb-name.
the generated kb is automatically made current."
  
    `(eval-when (compile load eval)
       (or (find-package ',kb-name) (make-package ',kb-name :use '()))
;       (in-package 'user :use (list (or (find-package ',kb-name)
;				(make-package ',kb-name))))
;       (unuse-package ',kb-name :user)
       (or (flavorp ',kb-configuration) (bab-require ',kb-configuration))
       (unless (use-old-kb? ',kb-name)
	 (setq ,kb-name 
	       (make-window-or-instance ',kb-configuration 
					:kb-name ',kb-name			    
					,@init-plist))
	 ($send ,kb-name :store-deklaration
		'(def-kb-instance ,kb-name ,kb-configuration ,@init-plist))
	 ($send ,kb-name :make-yourself-known)
	 ($send ,kb-name :make-yourself-current)
	 ',kb-name)))


;;-----------------------------------------------------------------------
;;             KNOWLEDGE BASE CONSTRUCTOR  zur  kompatibilitaet
;;-----------------------------------------------------------------------


(defun get-special-mixins-to-include (plist)
  (second (member :special plist)))

(defun get-proc-mixins-to-include (plist)
  (or (second (member :procs plist))
      *default-procs*))

(defun get-interface-mixins-to-include (plist)
  (or (second (member :interface plist))
      *default-interface*))

(defun get-kb-configuration ()
  *default-kb-configuration*)


(defun filter-plist (key plist)
  (let ((tail (member key plist)))
    (append (ldiff plist tail) (rest (rest tail)))))

(defmacro knowledge-base (kb-name &rest init-plist)
  "knowledge base constructor.
generates eventually a kb configuration named PROCESSOR-FOR-<kb-name> and
makes an instance of this configuration. init-plist is searched for values 
of the keys :special, :procs and :interface. these are used to build the
:special, :procs and :interface options of the def-kb-configuration form.
if one of the values is missing defaults are used instead. if no values
for :special, :procs and :interface are specified, a default kb-configuration
is used instead of creating a new one. defaults are taken from *default-procs*,
*default-interface* and *default-kb-configuration* respectively."

  (cond ((or (member :procs init-plist)
	     (member :interface init-plist)
	     (member :special init-plist))
	 (let ((kb-configuration (intern (format nil "PROCESSOR-FOR-~S" kb-name)))    
	       (special-mixins (get-special-mixins-to-include init-plist))
	       (proc-mixins (get-proc-mixins-to-include init-plist))
	       (interface-mixins (get-interface-mixins-to-include init-plist))
	       (rest-plist (filter-plist :special
			     (filter-plist :interface
			       (filter-plist :procs init-plist)))))
	   `(progn (def-kb-configuration
		     ,kb-configuration
		     (:special ,@special-mixins)
		     (:procs ,@proc-mixins)
		     (:interface ,@interface-mixins))
		   (def-kb-instance ,kb-name ,kb-configuration ,@rest-plist)
		   ($send ,kb-name :store-deklaration
			  '(knowledge-base ,kb-name ,@init-plist)))))
	(t (let ((kb-configuration (get-kb-configuration)))
	     `(progn (def-kb-instance ,kb-name ,kb-configuration ,@init-plist)
		     ($send ,kb-name :store-deklaration
			    '(knowledge-base ,kb-name ,@init-plist)))))))


;;-----------------------------------------------------------------------
;;                 INSTRUCTIONS CONSTRUCTOR 
;;-----------------------------------------------------------------------

(defmacro instructions (&rest instructions)
  "assigns instructions to the instructions slot of the current kb."
  `(cond ((current-kb-typep 'kb-processor-core)
	  (send-kb :set-instructions ',instructions)
	  `(instructions defined for ,(send-kb :kb-name)))))


;;-----------------------------------------------------------------------
;;                 UTILITY FUNCTIONS 
;;-----------------------------------------------------------------------



(defun search-for-kb (kb-name)
  (let ((kb (if (boundp kb-name) (symbol-value kb-name))))
      (if (not (flavor-typep kb 'kb-processor-core))
	  (baberror (getentry kb-does-not-exist-fstr babylon-io-table) kb-name))
      (if (not (current-p kb))
	  (if (y-or-n-p (format nil (getentry use-kb-fstr babylon-io-table) kb-name))
	      ($send kb :make-yourself-current)
	      (baberror (getentry unwanted-kb-fstr babylon-io-table) kb-name)))))


(defmacro use-kb-instance (kb-name)
  "makes kb with name kb-name current. if the external representation
of a kb is distributed over several files this form may insure that
the right kb is current when any of the files is evaluated."
  `(eval-when (compile load eval)
     ;(in-package "BABYLON" :use (list (or (find-package ',kb-name)
     ;                                  (make-package ',kb-name))))
     (in-package "BABYLON")
     (use-package (list (or (find-package ',kb-name)
                            (make-package ',kb-name))) 
                  "BABYLON")
     (search-for-kb ',kb-name)))

;;-----------------------------------------------------------------------


(defun is-activated-kb ()
  (and (boundp '*current-knowledge-base*)
       (flavor-typep *current-knowledge-base* 'kb-processor-core)))

;;-----------------------------------------------------------------------


(def$method (kb-processor-core :initialize) ()
  "method to be specialized by the user."
  t)


;;-----------------------------------------------------------------------
;;                 KNOWLEDGE BASE OPERATIONS
;;-----------------------------------------------------------------------

(def$method (kb-processor-core :run) ()
 "method to be overwritten by one of the diverse interface mixins.
it is called by :select-kb which is used normally to activate a kb."
  t)

(def$method (kb-processor-core :deselect-kb) ()
  "method to be specialized by interface mixins."
  t)

(def$method (kb-processor-core :select-kb) ()
  "activates a kb making it current."
  ($send self :make-yourself-current)
  ($send self :run))


(def$method (kb-processor-core :reset-kb) ()
  "sends all processors a :reset message."
  (let ((*current-knowledge-base* self))
    (mapc #'(lambda (proc)
	      ($send proc :send-if-handles :reset-proc))
	  procs)
    t))

;;-----------------------------------------------------------------------

(def$method (kb-processor-core :kill-kb) ()
  "makes the kb unaccessable. 
if the kb was current one of the remaining known kbs is made current." 
  ($send self :make-yourself-unknown)
  (setf (symbol-value kb-name) nil)
  (if (current-p self)
      (let ((next-kb (first  *known-knowledge-bases*)))
	(if next-kb
	    ($send (symbol-value next-kb) :make-yourself-current)
	    (setf *current-knowledge-base* nil))))
  t)


;;-----------------------------------------------------------------------


(def$method (kb-processor-core :global-trace-status) ()
  "provides information on trace in form of a menu item list."
  (let ((trace-items
	  (cons ($send self :trace-status)
		(delete nil
			(mapcar #'(lambda (proc)
				    ($send proc :send-if-handles :trace-status))
				procs))))
	(prot-item ($send self :send-if-handles :prot-status)))
    (if prot-item
	(cons prot-item trace-items)
	trace-items)))

;;-----------------------------------------------------------------------

(def$method (kb-processor-core :kb-inform) (&optional (stream *default-dialog-stream*))
  "prints statistics about the kb to stream."
  (format stream (getentry state-of-kb-fstr babylon-io-table) kb-name)
  (mapc #'(lambda (proc)
	    ($send proc :send-if-handles :kb-inform stream))
	procs)    
  (format stream "~%")
  kb-name)

(def$method (kb-processor-core :describe-kb) ()
  "prints statistics about the kb to the dialog-stream of the kb."
  ($send self :kb-inform ($send self :dialog-stream)))

;;-----------------------------------------------------------------------
;;             STARTING THE KNOWLEDGE BASE
;;-----------------------------------------------------------------------  


(def$method (kb-processor-core  :start)
	   (&optional (list-of-instructions nil))
  "initializes the kb and calls :start-execution within an environment
where *current-knowledge-base* is bound to the kb and *language* to its language.
:start-execution is evaluated within a catch-form with tag knowledge-base-stop-tag
which is used by the functiom stop-kb-execution."
  (let ((*current-knowledge-base* self)
	(*language* ($send self :language)))
    ;; Dies ist notwendig weil innerhalb der "procs" 
    ;; auf die globalen Variablen referiert wird.
    ($send self :initialize)
    (catch 'knowledge-base-stop-tag	    
      ($send self :start-execution list-of-instructions))))

(def$method (kb-processor-core  :start-kb)
	   (&optional (list-of-instructions nil))
  "same as :start."
  ($send self :start list-of-instructions))

(defun stop-kb-execution (&optional (result 'knowledge-base-stopped))
    (throw 'knowledge-base-stop-tag result))

;(declare-lisp-fns stop-kb-execution) ;; for use in rules

(def$method (kb-processor-core :start-execution)
	   (&optional (list-of-instructions nil))
  "evaluates the instructions provided by the parameter list-of-instructions 
or those from the slot instructions within a progn form with self bound to the kb."
  (let (($self self))
    (declare (special $self))
    (eval `(progn . ,(subst '$self 'self
			    (or list-of-instructions instructions))))))


;;-----------------------------------------------------------------------
;;             INTERACTIVE OPERATIONS
;;-----------------------------------------------------------------------



(def$method (kb-processor-core :reset-kb-confirmed) ()
  "asks whether to reset the kb resetting it eventually."
  (if ($send self :confirm
	    (format nil (getentry reset-kb-fstr babylon-io-table) kb-name))
      ($send self :reset-kb)))


(def$method (kb-processor-core :start-kb-confirmed) ()
  "asks whether to start the kb starting it eventually."
  (when (lexpr-$send self :confirm 
		    (format nil (getentry start-fstr babylon-io-table)
			    ($send self :kb-name))	 
		    ($send self :global-trace-status))
    ($send self :babylon-format
		  (getentry starting-kb-fstr babylon-io-table) kb-name)
    ($send self :start)
    ($send self :babylon-format  "~%")))



;;----------------------------------------------------------------------- 


(defun reset-knowledge-base ()
  "asks whether to reset the current kb resetting it eventually."
  (send-kb :reset-kb-confirmed))

(defun start-knowledge-base ()
  "asks whether to start the current kb starting it eventually."
  (send-kb :start-kb-confirmed))

(defun call-kb (&optional (kb *current-knowledge-base*))
  "selects kb."
  ($send kb :select-kb))
